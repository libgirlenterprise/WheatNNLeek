(uiop/package:define-package :wheatnnleek-mnist/src/validate (:use :cl :cl-wheatnnleek-cffi/ffi))
(in-package :wheatnnleek-mnist/src/validate)
;;;don't edit above

(defun test-data ()
  (wheatnnleek-mnist/src/main::get-mnist-testing-data))

(defun test-label ()
  (wheatnnleek-mnist/src/main::get-mnist-testing-label))

(defun validate (&key (count 100)
                      weight-path
                      label-path
                      theta-path)
  (wheatnnleek-mnist/src/main::load-predictor weight-path label-path theta-path)
  (let* ((data-set (test-data))
         (label-set (test-label))
         (data-length (mnist-database:number-of-images data-set))
         (from 0 #+nil(random data-length)))
    (when (> count data-length)
      (error "count is too big:~A" count))
    (loop with number-of-equal = 0
          for i from 0 below count
          for ix = (mod (+ from i) data-length)
          for image = (mnist-database:image data-set ix)
          for label = (mnist-database:label label-set ix)
          for predict = (wheatnnleek-mnist/src/main::predict image)
          for result = (equal predict label)
          do (when result
               (incf number-of-equal))
             (format t "~A/~A ~A ~S/~S ~A~%"
                     (1+ i)
                     count
                     (if result :t :f)
                     predict
                     label
                     number-of-equal)
             (force-output))))
