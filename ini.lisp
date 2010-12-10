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


(defun nice-name (string)
  (string-trim " 	" string))

(defun nice-keyword (string)
  (s-sql:from-sql-name (nice-name string)))

(defun read-from-string-maybe (string)
  (handler-case
      (let ((token (read-from-string string)))
        (if (numberp token) token string))
    (error () string)))

(defun groupname (line)
  "Return group name if any, or nil."
  (let* ((groupname-start (position #\[ line))
         (groupname-end
          (and groupname-start (position #\] line :start groupname-start))))
    (when groupname-end
      (nice-keyword (subseq line (1+ groupname-start) groupname-end)))))

(defun key-value-pair (line)
  "Return a key/value pair if any, or nil."
  (let ((equal-sign-position (position #\= line)))
    (when equal-sign-position
      (list (nice-keyword (subseq line 0 equal-sign-position))
            (read-from-string-maybe
             (nice-name (subseq line (1+ equal-sign-position))))))))

(defun discard-comment (line)
  (let ((half-cleaned (subseq line 0 (position #\# line))))
    (subseq half-cleaned 0 (position #\; half-cleaned))))


(defun ini (path)
  "Read the ini file from path.  Return an alist of plists."
  (with-open-file (stream path)
    (let ((ini (list (cons nil nil))))  ;group of the groupless
      (loop
         for line = (discard-comment (read-line stream nil))
         while line do
           (let ((groupname (groupname line)))
             (if groupname
                 (setf ini (append ini (list (cons groupname nil))))
                 (let ((key-value-pair (key-value-pair line)))
                   (when key-value-pair
                     (setf (cdar (last ini))
                           (append (cdar (last ini)) key-value-pair)))))))
      ini)))