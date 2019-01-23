(uiop/package:define-package :wheatnnleek-mnist/src/main
    (:nicknames :wheatnnleek-mnist) (:use :cl)
  (:export :train))
(in-package :wheatnnleek-mnist/src/main)
;;;don't edit above

(defun get-mnist-training-data ()
  (mnist-database:open-image-data (uiop:subpathname (asdf:system-source-directory :neuromancer-mnist)
                                                    "./data/train-images.idx3-ubyte")))

(defun get-mnist-testing-data ()
  (mnist-database:open-image-data (uiop:subpathname (asdf:system-source-directory :neuromancer-mnist)
                                                    "./data/t10k-images.idx3-ubyte")))

(defun get-mnist-training-label ()
  (mnist-database:open-label-data (uiop:subpathname (asdf:system-source-directory :neuromancer-mnist)
                                                    "./data/train-labels.idx1-ubyte")))

(defun get-mnist-testing-label ()
  (mnist-database:open-label-data (uiop:subpathname (asdf:system-source-directory :neuromancer-mnist)
                                                    "./data/t10k-labels.idx1-ubyte")))

(defparameter *neuron-number* 100)

(defparameter *training-data-size-to-use* 60000)

(defvar *training-data*)

(defvar *input-layer-population*)

(defvar *excitatory-layer-population*)

(defvar *inhibitory-layer-population*)

(defun train (weight-save-filepath)
  (cl-wheatnnleek-cffi/ffi::network-clear)
  (with-open-file (output-file-stream (uiop:ensure-pathname weight-save-filepath)
                                      :direction :output
                                      :if-exists :supersede)
    (setf *training-data* (get-mnist-training-data))
    (setf *input-layer-population*
          (getf (cl-wheatnnleek-cffi/ffi:network-create 784 "StaticPoisson" nil)
                :|population|))
    (setf *excitatory-layer-population*
          (getf (cl-wheatnnleek-cffi/ffi:network-create *neuron-number* "ConductionBasedAdaptiveThresholdLIF" nil)
                :|population|))
    (setf *inhibitory-layer-population*
          (getf (cl-wheatnnleek-cffi/ffi:network-create *neuron-number* "ConductionBasedAdaptiveThresholdLIF" '(:|e_i| -85
                                                                                                                :|tau_m| 10
                                                                                                                :|theta| 0
                                                                                                                :|theta_plus| 0
                                                                                                                :|v_th| -40
                                                                                                                :|refact| 2
                                                                                                                :|e_l| -60
                                                                                                                :|reset_v| -45))
                :|population|))
    (let ((input-population-id (getf *input-layer-population* :|id|))
          (excitatory-population-id (getf *excitatory-layer-population* :|id|))
          (inhibitory-population-id (getf *inhibitory-layer-population* :|id|))
          (stdp-connection-ids))
      (setf stdp-connection-ids
            (cl-wheatnnleek-cffi/ffi:network-stdp-connect input-population-id excitatory-population-id 10d0))
      (cl-wheatnnleek-cffi/ffi:network-static-connect excitatory-population-id inhibitory-population-id 5d0 "linear" "Excitatory")
      (cl-wheatnnleek-cffi/ffi:network-static-connect inhibitory-population-id excitatory-population-id 0d0 "all_to_all_except_diagonal" "Inhibitory")
      (loop for i from 0 below (mnist-database:number-of-images *training-data*)
            for k from 0 below *training-data-size-to-use*
            do (let ((image (mnist-database:image *training-data* i)))
                 (format t "~a~%~%" k)
                 (dotimes (i 28)
                   (dotimes (j 28)
                     (let ((pixel (aref image i j)))
                       (cl-wheatnnleek-cffi/ffi:network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                                                                :|neuron_ids|))
                                                                                   (* i 28)
                                                                                   j)
                                                                                (coerce (/ pixel 4)
                                                                                        'double-float)))))
                 (cl-wheatnnleek-cffi/ffi:network-run 350d0)
                 (dotimes (i 28)
                   (dotimes (j 28)
                     (cl-wheatnnleek-cffi/ffi:network-set-static-poisson-freq (+ (first (getf *input-layer-population*
                                                                                              :|neuron_ids|))
                                                                                 (* i 28)
                                                                                 j)
                                                                              0d0)))
                 (cl-wheatnnleek-cffi/ffi:network-run 150d0)))
      (loop for connection-id in stdp-connection-ids
            do (let ((conn-info (cl-wheatnnleek-cffi/ffi:network-get-conn-info-by-id connection-id)))
                 (format output-file-stream
                         "~{~a~^ ~}~%"
                         (mapcar #'(lambda (keyword)
                                     (getf conn-info keyword))
                                 '(:|source| :|target| :|weight|))))))))
                         
;; (defun label-neurons (weight-save-filepath label-save-path)
;;   (cl-wheatnnleek-cffi/ffi::network-clear)
;;   (with-open-file (input-file-stream (uiop:ensure-pathname weight-save-filepath))
;;     (with-open-file (output-file-stream (uiop:ensure-pathname label-save-path)
;;                                         :direction :output
;;                                         :if-exists :supersede)
      
