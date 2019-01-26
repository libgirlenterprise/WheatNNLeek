;;don't edit
(DEFSYSTEM "wheatnnleek-mnist" :DEPENDS-ON
 (:ANAPHORA :MNIST-DATABASE :CL-WHEATNNLEEK-CFFI) :CLASS
 :PACKAGE-INFERRED-SYSTEM :COMPONENTS ((:FILE "src/main")) :AUTHOR "Shaka Chen"
 :MAILTO "scchen@libgirl.com")
