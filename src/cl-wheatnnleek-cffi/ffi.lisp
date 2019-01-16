(uiop/package:define-package :cl-wheatnnleek-cffi/ffi
  (:use :cl)
  (:export
   :hello-world
   :sum
   :network-create
   :network-connect
   :static-connect
   :stdp-connect
   :Network-run
   :get-population-by-id
   :set-static-poisson-freq
   ))
(in-package :cl-wheatnnleek-cffi/ffi)
;;;don't edit above
(cffi:define-foreign-library libwheatnnleek
  (:darwin #.(ignore-errors (namestring (probe-file (merge-pathnames "../core/target/debug/libwheatnnleek.dylib" (asdf:system-source-directory (asdf:find-system :cl-wheatnnleek-cffi nil)))))))
  (:unix #.(ignore-errors (namestring (probe-file (merge-pathnames "../core/target/debug/libwheatnnleek.so" (asdf:system-source-directory (asdf:find-system :cl-wheatnnleek-cffi nil))))))))

(cffi:use-foreign-library libwheatnnleek)
(cffi:defcfun ("hello_world" %hello_world) :pointer)
(cffi:defcfun ("json_string_free" %json_string_free) :void
  (p :pointer))

(defun hello-world ()
  (let ((p (%hello_world)))
    (unwind-protect
         (cffi:foreign-string-to-lisp p)
      (%json_string_free p))))

(cffi:defcfun ("sum" sum) :int
  (a :int)
  (b :int))

(cffi:defcfun ("Network_create" %Network_create) :pointer
  (neuron_number :int)
  (neuron_type_buf :string)
  (rests :string))

(defun network-create (neuron_number neuron_type_buf params-plist)
  (let ((p (%Network_create neuron_number neuron_type_buf (jonathan:to-json params-plist))))
    (unwind-protect
         (let ((string (cffi:foreign-string-to-lisp p)))
           (and string
                (list :|population| (jonathan:parse string))))
      (%json_string_free p))))

(cffi:defcfun ("Network_connect" network-connect) :boolean
  (neuron_id1 :int)
  (neuron_id2 :int))

(cffi:defcfun ("static_connect" static-connect) :boolean
  (neuron_id1 :int)
  (neuron_id2 :int)
  (connection_delay :double)
  (connector :string)
  (post_syn_effect :string))

(cffi:defcfun ("stdp_connect" stdp-connect) :boolean
  (neuron_id1 :int)
  (neuron_id2 :int)
  (connection_delay :double))

(cffi:defcfun ("Network_run" Network-run) :boolean
  (time :double))

(cffi:defcfun ("get_population_by_id" %get_population_by_id) :pointer
  (population_id :int))

(defun get-population-by-id (population-id)
  (let ((p (%get_population_by_id population-id)))
    (unwind-protect
         (let ((string (cffi:foreign-string-to-lisp p)))
           (and string
                (list :|population| (jonathan:parse string))))
      (%json_string_free p))))

(cffi:defcfun ("set_static_poisson_freq" set-static-poisson-freq) :boolean
  (neuron_id :int)
  (freq :double))
