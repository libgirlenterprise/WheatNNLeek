;;don't edit
(DEFSYSTEM "cl-wheatnnleek-cffi" :LICENSE "apache-2.0" :CLASS
 :PACKAGE-INFERRED-SYSTEM :COMPONENTS ((:FILE "ffi")) :DEPENDS-ON
 (:CFFI :JONATHAN) :AUTHOR "Libgirl" :MAILTO "team@libgirl.com"
 :IN-ORDER-TO ((TEST-OP (TEST-OP "cl-wheatnnleek-cffi/tests"))))
(DEFSYSTEM "cl-wheatnnleek-cffi/tests" :DEPENDS-ON
 ("cl-wheatnnleek-cffi" "rove") :COMPONENTS
 ((:FILE "tests/test")) :PERFORM
 (TEST-OP (O C)
  (SYMBOL-CALL :ROVE :RUN "cl-wheatnnleek-cffi/tests/test")))
