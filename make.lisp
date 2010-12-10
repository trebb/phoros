;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010 Bert Burgemeister
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


;;;; Make an executable.
;; TODO: don't expect quicklisp to be preloaded

(push (make-pathname :directory '(:relative :up "photogrammetrie"))
      asdf:*central-registry*)

(push (make-pathname :directory '(:relative :up "phoros"))
      asdf:*central-registry*)

(ql:quickload "phoros")

(in-package :phoros)

(sb-ext:save-lisp-and-die
 "phoros" :save-runtime-options t :toplevel (function main) :executable t)
