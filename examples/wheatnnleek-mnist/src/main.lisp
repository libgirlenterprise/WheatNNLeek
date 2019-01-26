(uiop/package:define-package :wheatnnleek-mnist/src/main
    (:nicknames :wheatnnleek-mnist) (:use :cl :cl-wheatnnleek-cffi/ffi)
  (:export :train))
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

(defvar *training-data*)

(defvar *training-labels*)

(defvar *input-layer-population*)

(defvar *excitatory-layer-population*)

(defvar *inhibitory-layer-population*)

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

(defun train (weight-save-filepath)
  (cl-wheatnnleek-cffi/ffi::network-clear)
  (with-open-file (output-file-stream (uiop:ensure-pathname weight-save-filepath)
                                      :direction :output
                                      :if-exists :supersede)
    (setf *training-data* (get-mnist-training-data))
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
      (loop for i from 0 below (mnist-database:number-of-images *training-data*)
            for k from 0 below *training-data-size-to-use*
            do (let ((image (mnist-database:image *training-data* i)))
                 (format t "~a~%~%" k)
                 (dotimes (i 28)
                   (dotimes (j 28)
                     (network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                                                              :|neuron_ids|))
                                                                                 (* i 28)
                                                                                 j)
                                                                              0d0)))
                 (network-run 150d0)
                 (dotimes (i 28)
                   (dotimes (j 28)
                     (let ((pixel (aref image i j)))
                       (network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                                                                :|neuron_ids|))
                                                                                   (* i 28)
                                                                                   j)
                                                                                (coerce (/ pixel 4)
                                                                                        'double-float)))))
                 (network-run 350d0)))
      (mnist-database:close-data *training-data*)
      (loop for connection-id in stdp-connection-ids
            do (let ((conn-info (network-get-conn-info-by-id connection-id)))
                 (format output-file-stream
                         "~{~a~^ ~}~%"
                         (mapcar #'(lambda (keyword)
                                     (getf conn-info keyword))
                                 '(:|source| :|target| :|weight|))))))))
                         
(defun label-neurons (weight-save-filepath label-save-path)
  (cl-wheatnnleek-cffi/ffi::network-clear)
  (with-open-file (input-file-stream (uiop:ensure-pathname weight-save-filepath))
    (create-neurons)
    (let ((input-population-id (getf *input-layer-population* :|id|))
          (excitatory-population-id (getf *excitatory-layer-population* :|id|))
          (inhibitory-population-id (getf *inhibitory-layer-population* :|id|)))
      (dotimes (i 784)
        (dotimes (j *neuron-number*)
          (assert (= (read) i))
          (assert (= (read)
                     (+ 784 j)))
          (network-static-i-j-connect input-population-id
                                                              excitatory-population-id
                                                              i
                                                              j
                                                              (read) ; read weight saved
                                                              10d0)))))
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
  (setf *training-data* (get-mnist-training-data))
  (setf *training-labels* (get-mnist-training-label))
  (let ((neuron-response-counts ((make-array (list *neuron-number*
                                                   10)
                                             :initial-element 0))))
;;        (neuron-label-array (make-array (list *neuron-number*))))
    (loop for i from 0 below (mnist-database:number-of-images *training-data*)
          for k from 0 below *training-data-size-to-use*
          do (let ((image (mnist-database:image *training-data* i))
                   (image-label (mnist-database:label *training-labels* i)))
               (format t "~a~%~%" k)
               (dotimes (i 28)
                 (dotimes (j 28)
                   (let ((pixel (aref image i j)))
                     (network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                                                              :|neuron_ids|))
                                                                                 (* i 28)
                                                                                 j)
                                                                              (coerce (/ pixel 4)
                                                                                      'double-float)))))
               (network-clear-spike-records excitatory-population-id)
               (network-run 350d0)
               (let ((spike-records (network-get-spike-records)))
                 (loop for neuron-spike-record in spike-records
                       for i from 0 below *neuron-number*
                       do (incf (aref neuron-response-counts
                                      i
                                      image-label)
                                (lenght (first (second neuron-spike-record))))))))
    (with-open-file (output-file-stream (uiop:ensure-pathname label-save-path)
                                        :direction :output
                                        :if-exists :supersede)
      (loop for i from 0 below *neuron-number*
            do (let ((current-max-index (random 10))
                     (current-max-number 0))
                 (dotimes (j 10)
                   (let ((response-counts (aref neuron-response-counts i j)))
                     (when (> response-counts current-max-number)
                       (setf current-max-index j)
                       (setf current-max-number response-counts))))
                 ;;                 (setf (aref neuron-label-array i) current-max-index)
                 (format output-file-stream
                         "~a~%"
                         current-max-index))))))
