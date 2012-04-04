;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2011, 2012 Bert Burgemeister
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

(defmacro defun* (name lambda-list &body body)
  "Like defun, define a function, but with an additional lambda list
keyword &mandatory-key which goes after the &key section or in place
of it.  &mandatory-key argument definitions are plain symbols (no
lists).  An error is signalled on function calls where one of those
keyargs is missing."
  (let ((mandatory-key-position (position '&mandatory-key lambda-list))
        (after-key-position (or (position '&allow-other-keys lambda-list)
                                (position '&aux lambda-list))))
    (when mandatory-key-position
      (setf lambda-list
            (append (subseq lambda-list 0 mandatory-key-position)
                    (unless (position '&key lambda-list)
                      '(&key))
                    (mapcar
                     #'(lambda (k)
                         `(,k (error ,(format nil "~A: argument ~A undefined"
                                              name k))))
                     (subseq lambda-list
                             (1+ mandatory-key-position)
                             after-key-position))
                    (when after-key-position
                      (subseq lambda-list after-key-position))))))
  `(defun ,name ,lambda-list ,@body))

(defmacro logged-query (message-tag &rest args)
  "Act like postmodern:query; additionally log some debug information
tagged by the short string message-tag."
  (cl-utilities:with-unique-names
      (executed-query query-milliseconds query-result)
    `(let* (,executed-query
            ,query-milliseconds
            ,query-result
            (cl-postgres:*query-callback*
             #'(lambda (query-string clock-ticks)
                 (setf ,query-milliseconds clock-ticks)
                 (setf ,executed-query query-string))))
       (prog1
           (setf ,query-result
                 (etypecase (car ',args)
                   (list
                    (typecase (caar ',args)
                      (keyword          ;s-sql form
                       (query (sql-compile ',(car args))
                              ,@(cdr args)))
                      (t       ;function (supposedly) returning string
                       (query ,@args))))
                   (string
                    (query ,@args))
                   (symbol
                    (query ,@args))))
         (cl-log:log-message
          :sql
          "[~A] Query ~S~& took ~F seconds and yielded~& ~A."
          ,message-tag
          ,executed-query
          (/ ,query-milliseconds 1000)
          ,query-result)))))
