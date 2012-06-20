(defpackage phoros-test
  (:use :common-lisp
        :phoros
        #+sbcl :sb-rt
        #-sbcl :rt))

(in-package phoros)

(do-symbols (s) (import (list s) :phoros-test))

(in-package :phoros-test)

(defun round-numbers (decimal-digits numbers)
  (let ((format-string  (format nil "~~{~~,~DF~~#^ ~~}" decimal-digits)))
    (format nil format-string numbers)))

(deftest cs2cs/1
    (round-numbers
     8 (proj:cs2cs (list (proj:degrees-to-radians -112.5d0)
                         (proj:degrees-to-radians +45.25919444444d0)
                         100d0)
                   :source-cs "+proj=latlong +datum=NAD83"
                   :destination-cs "+proj=utm +zone=10 +datum=NAD27"))
  "1323852.82619611 5065462.32041128 99.99994720")

(deftest geographic-to-utm/1
    (round-numbers
     8 (geographic-to-utm 31 13d0 50d0 200d0))
  "1216025.31786614 5586720.84493467 200.00000000")

(deftest geographic-to-utm/2
    (round-numbers
     8 (geographic-to-utm 32 13d0 50d0 200d0))
  "786627.95104280 5546300.84739345 200.00000000")

(deftest geographic-to-utm/3
    (round-numbers
     8 (geographic-to-utm 32 13 50 200))
  "786627.95104280 5546300.84739345 200.00000000")

(deftest assert-utm-zone/1
    (assert-utm-zone
     9.427261747452828 3 9.28838684 53.19274138 68.969 519267.427 5893750.59 68.969)
  nil)

(deftest assert-utm-zone/2
    (handler-case
        (assert-utm-zone
         9.427261747452828 3 9.28838684 53.19274138 68.969 519267.627 5893750.59 68.969)
      (simple-error () "expected error"))
  "expected error")

(deftest find-keyword/1
    (img:find-keyword "testsuite/cottbus_0696/einzelbilder/cott001_CCDFRONT_0.pictures"
                      "Some-Junk=" 1103 500)
  nil)

(deftest find-keyword/2
    (img:find-keyword "testsuite/cottbus_0696/einzelbilder/cott001_CCDFRONT_0.pictures"
                      "timeTrigger=" 1103 500)
  1177)

(deftest find-keyword-value/1
    (img:find-keyword-value "testsuite/cottbus_0696/einzelbilder/cott001_CCDFRONT_0.pictures"
                      "height=" 1103 500)
  1040)
