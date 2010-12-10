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


(in-package :phoros)

(cl-log:defcategory :db-sys)
(cl-log:defcategory :db-dat)
(cl-log:defcategory :orphan)
(cl-log:defcategory :error)
(cl-log:defcategory :warning (or :warning :error))
(cl-log:defcategory :db (or :db-sys :db-dat :warning :error))
(cl-log:defcategory :debug (or :debug :db-sys :db-dat :orphan :warning :error))

(defun launch-logger (&optional (log-dir ""))
  "Start logging facility.  Create log-dir if necessary."
  (let ((log-dir (pathname-directory (ensure-directories-exist
                                      (pathname log-dir)))))

    (setf (cl-log:log-manager)
          (make-instance 'cl-log:log-manager
                         :message-class 'cl-log:formatted-message))

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :orphan
     :filename (make-pathname :directory log-dir :name "orphans" :type "log")
     :category :orphan)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :debug
     :filename (make-pathname :directory log-dir :name "debug" :type "log")
     :category :debug)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :warning
     :filename (make-pathname :directory log-dir :name "warnings" :type "log")
     :category :warning)

    (cl-log:start-messenger
     'cl-log:text-file-messenger
     :name :db
     :filename (make-pathname :directory log-dir :name "phoros" :type "log")
     :category :db)

    (cl-log:start-messenger
     'cl-log:text-stream-messenger
     :name :stream
     :stream *error-output*
     :category :debug)))


(defmethod cl-log:format-message ((self cl-log:formatted-message))
  (format nil "~A ~A ~?~&"
          (timestring (cl-log:timestamp-universal-time
                       (cl-log:message-timestamp self)))
          (cl-log:message-category self)
          (cl-log:message-description self)
          (cl-log:message-arguments self)))

(defun timestring (time)
  "ISO 8601 representation of time."
  (multiple-value-bind (whole-seconds remainder) (floor time)
    (when (zerop remainder) (setf remainder nil))
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time whole-seconds 0)
      (format
       nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~@[~0F~]Z"
       year month date hour minute second remainder))))