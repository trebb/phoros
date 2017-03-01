;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2016, 2017 Bert Burgemeister
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


(defpackage imread
  (:use :common-lisp :cffi)
  (:export png2mem ping)
  (:documentation "Interface to the C implementation of image-reader.lisp."))

(in-package :imread)

(load-foreign-library
 '(:or "./libimread.so"
   (:default "libimread")))

(cffi:defcfun "png2mem" :int
  (path :string)
  (blob-start :int)
  (blob-size :int)
  (image-width :int)
  (image-height :int)
  (channels :int)
  (baypat :pointer)
  (demosaic_fast :bool)
  (compr-mode :int)
  (mem-png :pointer)
  (reversep :bool)
  (brightenp :bool)
  (color-raiser :pointer))

(cffi:defcfun "ping" :int
  (n :int))
