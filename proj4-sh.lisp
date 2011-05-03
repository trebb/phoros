;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2011 Bert Burgemeister
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(defpackage proj
  (:use :common-lisp :cffi)
  (:export cs2cs degrees-to-radians radians-to-degrees version)
  (:documentation
   "Interface to the PROJ.4 cartographic projection library.
This is an alternative to the CFFI interface defined in proj4.lisp which doesn't work well with certain versions of proj4, see below.

;;; (proj:version)
;;; \"Rel. 4.7.1, 23 September 2009\"
;;; (proj:cs2cs (list (proj:degrees-to-radians 9.28838684)
;;;                   (proj:degrees-to-radians 53.19274138)
;;;                   68.969)
;;;             :source-cs "+proj=longlat +datum=WGS84 +no_defs"
;;;             :destination-cs "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
;;; (519267.42716064927 5893750.589875963 68.969)
;;; echo 9.28838684 -53.19274138 68.969 | cs2cs -f %.9f +proj=longlat +datum=WGS84 +no_defs +to  +proj=utm +south +zone=32 +datum=WGS84 +units=m +no_defs 
;;; 519267.427160649	4106249.410124036 68.969000000

;;; (proj:version)
;;; \"Rel. 4.6.1, 21 August 2008\"
;;; (proj:cs2cs (list (proj:degrees-to-radians 9.28838684)
;;;                   (proj:degrees-to-radians 53.19274138)
;;;                   68.969)
;;;             :source-cs " +proj=longlat +datum=WGS84 +no_defs"
;;;             :destination-cs "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
;;; (519267.46293972526 5893728.74020727 68.969)
;;; echo 9.28838684 -53.19274138 68.969 | cs2cs -f %.9f +proj=longlat +datum=WGS84 +no_defs +to  +proj=utm +south +zone=32 +datum=WGS84 +units=m +no_defs
;;; 519267.427160649	4106249.410124036 68.969000000"))

(in-package :proj)

(defun version ()
  "PROJ.4 version."
  (multiple-value-bind (standard-output error-output exit-status)
      (trivial-shell:shell-command "cs2cs")
    (declare (ignore standard-output) (ignore exit-status))
    (car (cl-utilities:split-sequence #\Newline error-output))))


(defun degrees-to-radians (degrees)
  "Convert degrees into radians."
  (* degrees  (/ pi 180)))

(defun radians-to-degrees (radians)
  "Convert radians into degrees."
  (* radians  (/ 180 pi)))

(defun cs2cs (point &key
              (source-cs "+proj=latlong +datum=WGS84")
              (destination-cs "+proj=latlong +datum=WGS84"))
  "Transform point (a list of (x y z)) from source-cs to
destination-cs.  Geographic coordinates are in radians."
  (let ((command
         (format
          nil
          "cs2cs -f %.9f ~A +no_defs +to ~A +no_defs"
          source-cs destination-cs)))
    (multiple-value-bind (standard-output error-output exit-status) 
        (trivial-shell:shell-command
         command
         :input (format nil "~{~S ~}~S"
                        (mapcar #'radians-to-degrees (butlast point))
                        (third point)))
      (unless (zerop exit-status)
        (error "Attempt to call `~A´ returned ~D: ~A"
               command exit-status error-output))
      (with-input-from-string (stream standard-output)
        (loop
           for number = (read stream nil)
           repeat 3
           while number
           do (assert (numberp number))
           collect number)))))
