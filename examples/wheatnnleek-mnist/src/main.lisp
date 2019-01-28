(uiop/package:define-package :wheatnnleek-mnist/src/main
    (:nicknames :wheatnnleek-mnist) (:use :cl :cl-wheatnnleek-cffi/ffi)
  (:export :train :label-neurons))
(in-package :wheatnnleek-mnist/src/main)
;;;don't edit above

(defvar *data-path*
  (merge-pathnames "data/" (asdf:system-source-directory :wheatnnleek-mnist)))

(defun get-mnist-training-data ()
  (mnist-database:open-image-data (merge-pathnames "train-images.idx3-ubyte" *data-path*)))

(defun get-mnist-testing-data ()
  (mnist-database:open-image-data (merge-pathnames "t10k-images.idx3-ubyte" *data-path*)))

(defun get-mnist-training-label ()
  (mnist-database:open-label-data (merge-pathnames "train-labels.idx1-ubyte" *data-path*)))

(defun get-mnist-testing-label ()
  (mnist-database:open-label-data (merge-pathnames "t10k-labels.idx1-ubyte" *data-path*)))

(defparameter *neuron-number* 100)

(defparameter *training-data-size-to-use* 60000)

(defparameter *start-intensity* 2)

(defvar *training-data*)

(defvar *training-labels*)

(defvar *input-layer-population*)

(defvar *excitatory-layer-population*)

(defvar *inhibitory-layer-population*)

(defvar *neuron-classes*)

(defvar *neuron-count-per-class*)

(defun create-neurons ()
  (setf *input-layer-population*
        (getf (network-create 784 "StaticPoisson" nil)
              :|population|))
  (setf *excitatory-layer-population*
        (getf (network-create *neuron-number* "ConductionBasedAdaptiveThresholdLIF" nil)
              :|population|))
  (setf *inhibitory-layer-population*
        (getf (network-create *neuron-number* "ConductionBasedAdaptiveThresholdLIF" '(:|e_i| -85
                                                                                      :|tau_m| 10
                                                                                      :|theta| 0
                                                                                      :|theta_plus| 0
                                                                                      :|v_th| -40
                                                                                      :|refact| 2
                                                                                      :|e_l| -60
                                                                                      :|reset_v| -45))
              :|population|)))

(defun clear-and-restore-network (weight-save-filepath theta-save-filepath)
  (cl-wheatnnleek-cffi/ffi::network-clear)
  (create-neurons)
  (let ((input-population-id (getf *input-layer-population* :|id|))
        (excitatory-population-id (getf *excitatory-layer-population* :|id|))
        (inhibitory-population-id (getf *inhibitory-layer-population* :|id|)))
    (network-set-properties excitatory-population-id
                            "theta"
                            (with-open-file (theta-input-stream (uiop:ensure-pathname theta-save-filepath))
                              (loop for i from 0 below *neuron-number*
                                    collect (coerce (read theta-input-stream)
                                                    'double-float))))
    (network-static-connect input-population-id
                            excitatory-population-id
                            10d0
                            "all_to_all"
                            "Excitatory")
    (network-static-connect excitatory-population-id
                            inhibitory-population-id
                            5d0
                            "linear"
                            "Excitatory")
    (network-static-connect inhibitory-population-id
                            excitatory-population-id
                            0d0
                            "all_to_all_except_diagonal"
                            "Inhibitory")
    (with-open-file (input-file-stream (uiop:ensure-pathname weight-save-filepath))
      (dotimes (i 784)
        (dotimes (j *neuron-number*)
          (assert (= (read input-file-stream) i))
          (assert (= (read input-file-stream)
                     (+ 784 j)))
          ;; after network-clear at the beginning, connection id should start from zero
          (network-set-weight-by-conn-id (+ (* i *neuron-number*)
                                            j)
                                         (coerce (read input-file-stream)
                                                 'double-float))))
                                        ; read weight saved
      (values input-population-id
              excitatory-population-id
              inhibitory-population-id))))

(defun set-input-layer-firing-freq (image-as-pixel-array &key (intensity-plus 0))
  (dotimes (i 28)
    (dotimes (j 28)
      (let ((pixel (aref image-as-pixel-array i j)))
        (network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                         :|neuron_ids|))
                                            (* i 28)
                                            j)
                                         (coerce (* (/ pixel 8)
                                                    (+ *start-intensity* intensity-plus))
                                                 'double-float))))))

(defun neuron-firing-count (neuron-spike-record)
  (length (first (second neuron-spike-record))))


(defmacro loop-run-image-input (excitatory-population-id-symbol image-label-symbol prompt-message-prefix-string each-image-form &body body)
  `(loop for i from 0 below (mnist-database:number-of-images *training-data*)
         for k from 0 below *training-data-size-to-use*
         do (let ((image (mnist-database:image *training-data* i))
                  (,image-label-symbol (mnist-database:label *training-labels* i)))
              (format t "~a:~a/~a~a" ,prompt-message-prefix-string (1+ k) *training-data-size-to-use* #\Return)
              (force-output)
              (set-input-layer-firing-freq image)
              (let ((run-network t)
                    (intensity-plus 0))
                (loop while run-network
                      do (block continue
                           (progn
                             (network-clear-spike-records ,excitatory-population-id-symbol)
                             (network-set-property ,excitatory-population-id-symbol
                                                   "fix_theta"
                                                   0d0)
                             (network-run 350d0)
                             (let ((spike-records (network-get-spike-records)))
                               (if (> (apply '+
                                             (mapcar #'(lambda (neuron-spike-record)
                                                         (neuron-firing-count neuron-spike-record))
                                                     spike-records))
                                      5)
                                   (setf run-network nil)
                                   (progn
                                     (set-input-layer-firing-freq image
                                                                  :intensity-plus (incf intensity-plus))
                                     (return-from continue))))
                             ,@body))))
              ,each-image-form)))
  
(defun train (weight-save-filepath theta-save-filepath)
  (cl-wheatnnleek-cffi/ffi::network-clear)
  (with-open-file (weight-output-stream (uiop:ensure-pathname weight-save-filepath)
                                        :direction :output
                                        :if-exists :supersede)
    (with-open-file (theta-output-stream (uiop:ensure-pathname theta-save-filepath)
                                         :direction :output
                                         :if-exists :supersede)
      (setf *training-data* (get-mnist-training-data))
      (setf *training-labels* (get-mnist-training-label))
      (create-neurons)
      (let* ((input-population-id (getf *input-layer-population* :|id|))
             (excitatory-population-id (getf *excitatory-layer-population* :|id|))
             (inhibitory-population-id (getf *inhibitory-layer-population* :|id|))
             (stdp-connection-ids (network-stdp-connect input-population-id excitatory-population-id 10d0)))
        (network-static-connect excitatory-population-id
                                inhibitory-population-id
                                5d0
                                "linear"
                                "Excitatory")
        (network-static-connect inhibitory-population-id
                                excitatory-population-id
                                0d0
                                "all_to_all_except_diagonal"
                                "Inhibitory")
        (network-record-spikes excitatory-population-id)
        (loop-run-image-input excitatory-population-id image-label "training" nil
          (dotimes (i 28)
            (dotimes (j 28)
              (network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                               :|neuron_ids|))
                                                  (* i 28)
                                                  j)
                                               0d0)))
          (network-set-property excitatory-population-id
                                "fix_theta"
                                1d0)
          (network-run 150d0))          
        (mnist-database:close-data *training-data*)
        (mnist-database:close-data *training-labels*)
        (loop for connection-id in stdp-connection-ids
              do (let ((conn-info (network-get-conn-info-by-id connection-id)))
                   (format weight-output-stream
                           "~{~a~^ ~}~%"
                           (mapcar #'(lambda (keyword)
                                       (getf conn-info keyword))
                                   '(:|source| :|target| :|weight|)))))
        (format theta-output-stream
                "~{~a~%~}"
                (network-get-property excitatory-population-id
                                      "theta"))))))

(defun label-neurons (weight-save-filepath theta-save-filepath label-save-path)
  (multiple-value-bind (input-population-id excitatory-population-id)
      (clear-and-restore-network weight-save-filepath theta-save-filepath)
    (declare (ignore input-population-id))
    (network-record-spikes excitatory-population-id)
    (network-set-property excitatory-population-id
                          "fix_theta"
                          1d0)
    (setf *training-data* (get-mnist-training-data))
    (setf *training-labels* (get-mnist-training-label))
    (let ((neuron-response-counts (make-array (list *neuron-number*
                                                    10)
                                              :initial-element 0))
          (label-counts (make-array '(10)
                                    :initial-element 0)))
      ;;        (neuron-label-array (make-array (list *neuron-number*))))
      (loop-run-image-input excitatory-population-id image-label "labeling" 
          (progn
            (incf (aref label-counts image-label))
            (let ((spike-records (network-get-spike-records)))
              (loop for neuron-spike-record in spike-records
                    for i from 0 below *neuron-number*
                    do (incf (aref neuron-response-counts
                                   i
                                   image-label)
                             (neuron-firing-count neuron-spike-record))))))
      (with-open-file (output-file-stream (uiop:ensure-pathname label-save-path)
                                          :direction :output
                                          :if-exists :supersede)
        (loop for i from 0 below *neuron-number*
              do (let ((current-max-index (random 10))
                       (current-max-number 0))
                   (dotimes (j 10)
                     (let ((response-degree (/ (aref neuron-response-counts i j)
                                               (aref label-counts j))))
                       ;; (format output-file-stream
                       ;;         "~a "
                       ;;         response-counts)
                       (when (> response-degree current-max-number)
                         (setf current-max-index j)
                         (setf current-max-number response-degree))))
                   ;;                 (setf (aref neuron-label-array i) current-max-index)
                   (format output-file-stream
                           "~a~%"
                           current-max-index)))))))

(defun load-predictor (weight-save-filepath label-save-path theta-save-filepath)
  (multiple-value-bind (input-population-id excitatory-population-id)
      (clear-and-restore-network weight-save-filepath theta-save-filepath)
    (declare (ignore input-population-id))
    (network-record-spikes excitatory-population-id)
    (network-set-property excitatory-population-id
                          "fix_theta"
                          1d0)
    (with-open-file (input-file-stream (uiop:ensure-pathname label-save-path))
      (setf *neuron-count-per-class*
            (make-array '(10)
                        :initial-element 0))
      (setf *neuron-classes*
            (make-array (list *neuron-number*)
                        :initial-contents (loop for i from 0 below *neuron-number*
                                                collect (anaphora:aprog1 (read input-file-stream)
                                                          (incf (aref *neuron-count-per-class*
                                                                      anaphora:it)))))))))

(defun predict (image-as-pixel-array)
  (let ((excitatory-population-id (getf *excitatory-layer-population* :|id|)))
    (set-input-layer-firing-freq image-as-pixel-array)
    (let ((run-network t)
          (intensity-plus 0)
          (result))
      (loop while run-network
            do (progn
                 (network-clear-spike-records excitatory-population-id)
                 (network-run 350d0)
                 (let ((firing-count-per-class (make-array '(10) :initial-element 0))
                       (spike-records (network-get-spike-records))
                       (total-firing-count 0))
                   (loop for neuron-spike-record in spike-records
                         for i from 0 below *neuron-number*
                         do (progn
                              (let ((neuron-firing-count (neuron-firing-count neuron-spike-record)))
                                (incf total-firing-count
                                      neuron-firing-count)
                                (incf (aref firing-count-per-class
                                            (aref *neuron-classes* i))
                                      neuron-firing-count))))
                   (if (> total-firing-count 5)
                       (progn
                         (setf run-network nil)
                         (setf result
                               (car (car (sort (loop for firing-count across firing-count-per-class
                                                     for neuron-count across *neuron-count-per-class*
                                                     for index from 0
                                                     collect (list index (if (zerop neuron-count)
                                                                             0
                                                                             (/ firing-count neuron-count))))
                                               #'>
                                               :key #'second)))))
                       (set-input-layer-firing-freq image-as-pixel-array
                                                    :intensity-plus (incf intensity-plus))))))
      result)))
