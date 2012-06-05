;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2012 Bert Burgemeister
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

(in-package :phoros)

(named-readtables:defreadtable json-syntax
  (:merge :standard)
  (:macro-char #\[ #'(lambda (stream char)
                       (declare (ignore char))
                       (coerce (read-delimited-list #\] stream)
                               'vector)))
  (:macro-char #\{ #'(lambda (stream char)
                       (declare (ignore char))
                       (read-delimited-list #\} stream)))
  (:syntax-from nil #\) #\})
  (:syntax-from nil #\) #\])
  (:syntax-from nil #\Space #\:)
  (:syntax-from nil #\Space #\,)
  (:case :preserve))


(defun pp-json (object &optional stream)
  "Write object as indented JSON to stream.  Vectors are represented
as JSON vectors.  Lists, which should have an even number of elements,
are represented as JSON objects."
  (cond
    ((stringp object)
     (prin1 object stream))
    ((consp object)
     (pprint-logical-block
         (stream object :prefix "{" :suffix "}")
       (loop
          (pprint-exit-if-list-exhausted)
          (pp-json (pprint-pop) stream)
          (princ ":" stream)
          (pp-json (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (princ "," stream)
          (pprint-newline :linear stream))))
    ((vectorp object)
     (pprint-logical-block
         (stream (coerce object 'list) :prefix "[" :suffix "]")
       (loop
          (pprint-exit-if-list-exhausted)
          (pp-json (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (princ "," stream)
          (pprint-newline :linear stream))))
    (t
     (princ object stream))))

(defun indent-json (json-text)
  "Indent json-text."
  (unwind-protect 
       (let ((*read-default-float-format* 'long-float)
             (*print-right-margin* 100))
         (named-readtables:in-readtable json-syntax)
         (with-output-to-string (s)
             (pp-json (read-from-string json-text nil)
                      s)))
    (named-readtables:in-readtable :standard)))