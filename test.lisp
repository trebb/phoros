(defpackage phoros-test
  (:use :common-lisp
        :rt))

(in-package :phoros-test)

(deftest cs2cs/1
    (proj:cs2cs (list (proj:degrees-to-radians -112.5d0)
                      (proj:degrees-to-radians +45.25919444444d0)
                      100d0)
                :source-cs "+proj=latlong +datum=NAD83"
                :destination-cs "+proj=utm +zone=10 +datum=NAD27")
  (1323852.8261961075 5065462.320411285 99.99994720239192))

(deftest geographic-to-utm/1
    (phoros::geographic-to-utm 31 13d0 50d0 200d0)
  (1216025.3178661424 5586720.844934666 200.0))

(deftest geographic-to-utm/2
    (phoros::geographic-to-utm 32 13d0 50d0 200d0)
  (786627.9510428045 5546300.84739345 200.0))

(deftest geographic-to-utm/3
    (phoros::geographic-to-utm 32 13 50 200)
  (786627.9510428045 5546300.84739345 200.0))

(deftest assert-utm-zone/1
    (phoros::assert-utm-zone 9.427261747452828 3 9.28838684 53.19274138 68.969 519267.427 5893750.59 68.969)
  nil)

(deftest assert-utm-zone/2
    (handler-case
        (phoros::assert-utm-zone 9.427261747452828 3 9.28838684 53.19274138 68.969 519267.627 5893750.59 68.969)
      (simple-error () "expected error"))
  "expected error")

