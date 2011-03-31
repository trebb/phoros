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
  (:documentation "Interface to the PROJ.4 cartographic projection library."))

(in-package :proj)

(load-foreign-library '(:default "libproj"))

(cffi:defcfun "pj_transform" :int
  (src :pointer)
  (dst :pointer)
  (point_count :long)
  (point_offset :int)
  (x :pointer)
  (y :pointer)
  (z :pointer))

(cffi:defcfun "pj_free" :void
  (arg0 :pointer))

(cffi:defcfun "pj_init_plus" :pointer
  (arg0 :string))

(cffi:defcfun "pj_get_release" :string)

(defun version ()
  "PROJ.4 version.  Last working one was \"Rel. 4.7.1, 23 September 2009\""
  (pj-get-release))

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
  (with-foreign-objects ((x :double) (y :double) (z :double)
                         (proj-source :pointer) (proj-destination :pointer))
    (setf proj-source (pj-init-plus source-cs)
          proj-destination (pj-init-plus destination-cs)
          (mem-ref x :double) (coerce (first point) 'double-float)
          (mem-ref y :double) (coerce (second point) 'double-float)
          (mem-ref z :double) (coerce (third point) 'double-float))
    (pj-transform proj-source proj-destination 1 1 x y z)
    (pj-free proj-source) (pj-free proj-destination)
    (list (mem-ref x :double) (mem-ref y :double) (mem-ref z :double))))
