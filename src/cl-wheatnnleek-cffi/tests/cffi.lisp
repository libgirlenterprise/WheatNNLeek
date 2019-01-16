(uiop/package:define-package :cl-wheatnnleek-cffi/tests/cffi
                             (:nicknames) (:use :rove :cl :cl-wheatnnleek-cffi/ffi) (:shadow) (:export)
                             (:intern))
(in-package :cl-wheatnnleek-cffi/tests/cffi)
;;don't edit above

(setup)

(deftest test-basic-call
  (sleep 1)
  (testing "say_hello"
    (ok (string= (hello-world)
                 "Hello World")))
  (testing "sum"
    (ok (= (sum 423 -582)
           (+ 423 -582)))))

(deftest wheatnnleek-jsonrpc-call
  (testing "|Network::create|"
    (ok (equal (network-create 1 "Izhikevich" nil)
               '(:|population| (:|size| 1 :|neuron_ids| (0) :|id| 0))))
    (ok (equal (network-create 3 "Izhikevich" nil)
               '(:|population| (:|size| 3 :|neuron_ids| (1 2 3) :|id| 1))))
    (ok (equal (network-create 3 "StaticPoisson" nil)
               '(:|population| (:|size| 3 :|neuron_ids| (4 5 6) :|id| 2)))))
  (testing "|get-population-by-id|"
    (ok (equal (get-population-by-id 0)
               '(:|population| (:|size| 1 :|neuron_ids| (0) :|id| 0))))
    (ok (equal (get-population-by-id 1)
               '(:|population| (:|size| 3 :|neuron_ids| (1 2 3) :|id| 1))))
    (ok (equal (get-population-by-id 2)
               '(:|population| (:|size| 3 :|neuron_ids| (4 5 6) :|id| 2)))))
  (testing "stdp-connect"
    (ok (eql
         (stdp-connect 0 1 10d0)
         t))
    (ok (eql (static-connect 1 2 10d0 
                                         "all_to_all_except_diagonal"
                                         "Inhibitory")
             t)))
  (testing "|set-static-poisson-freq|"
    (ok (eql (set-static-poisson-freq 5 63.75d0)
             t))))

(teardown)
