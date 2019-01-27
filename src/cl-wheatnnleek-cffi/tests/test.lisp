(uiop/package:define-package :cl-wheatnnleek-cffi/tests/test
                             (:nicknames) (:use :rove :cl :cl-wheatnnleek-cffi/ffi) (:shadow) (:export)
                             (:intern))
(in-package :cl-wheatnnleek-cffi/tests/test)
;;don't edit above

(setup)

(deftest test-basic-call
  (cl-wheatnnleek-cffi/ffi::network-clear)
  (testing "say_hello"
    (ok (string= (hello-world)
                 "Hello World")))
  (testing "sum"
    (ok (= (sum 423 -582)
           (+ 423 -582)))))

(deftest wheatnnleek-cffi-call
  (testing "|Network::create|"
    (ok (equal (network-create 1 "Izhikevich" nil)
               '(:|population| (:|size| 1 :|neuron_ids| (0) :|id| 0))))
    (ok (equal (network-create 3 "StaticPoisson" nil)
               '(:|population| (:|size| 3 :|neuron_ids| (1 2 3) :|id| 1))))
    (ok (equal (network-create 3 "ConductionBasedAdaptiveThresholdLIF" nil)
               '(:|population| (:|size| 3 :|neuron_ids| (4 5 6) :|id| 2)))))
  (testing "|get-population-by-id|"
    (ok (equal (network-get-population-by-id 0)
               '(:|population| (:|size| 1 :|neuron_ids| (0) :|id| 0))))
    (ok (equal (network-get-population-by-id 1)
               '(:|population| (:|size| 3 :|neuron_ids| (1 2 3) :|id| 1))))
    (ok (equal (network-get-population-by-id 2)
               '(:|population| (:|size| 3 :|neuron_ids| (4 5 6) :|id| 2)))))
  (testing "stdp-connect"
    (ok (equal (network-stdp-connect 1 2 10d0)
               '(0 1 2 3 4 5 6 7 8)))
    (ok (equal (network-static-connect 0 1 10d0
                                         "all_to_all_except_diagonal"
                                         "Inhibitory")
               nil)))
  (testing "|set-static-poisson-freq|"
    (ok (eql (network-set-static-poisson-freq 1 1000d0)
             t))
    (ok (eql (network-set-static-poisson-freq 2 1000d0)
             t))
    (ok (eql (network-set-static-poisson-freq 3 1000d0)
             t)))
  (testing "|Network_run|"
    (ok (eql (network-run 1000d0)
             t)))
  (testing "spike recording"
    (ok (eql (network-record-spikes 2)
             t))
    (ok (eql (network-run 1000d0)
             t))
    (ok (not (equal (network-get-spike-records)
                    '((4 (NIL)) (5 (NIL)) (6 (NIL))))))
    (ok (eql (network-clear-spike-records 2)
             t))
    (ok (equal (network-get-spike-records)
               '((4 NIL) (5 NIL) (6 NIL))))))

(teardown)
