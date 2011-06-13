;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010, 2011 Bert Burgemeister
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

(cl-log:defcategory :access)
(cl-log:defcategory :db-sys)
(cl-log:defcategory :db-dat)
(cl-log:defcategory :db (or :db-sys :db-dat))
(cl-log:defcategory :orphan)
(cl-log:defcategory :info (or :info :db :orphan))
(cl-log:defcategory :warning)
(cl-log:defcategory :error)
(cl-log:defcategory :debug (or :debug :db :info :warning :error))

(defun launch-logger (&optional (log-dir ""))
  "Start logging facility.  Create log-dir if necessary."
  (let ((log-dir (pathname-directory (ensure-directories-exist
                                      (pathname log-dir)))))

    (setf (cl-log:log-manager)
          (make-instance 'cl-log:log-manager
                         :message-class 'cl-log:formatted-message))

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :access
     :filename (make-pathname :directory log-dir :name "access" :type "log")
     :category :access)

    (cl-log:start-messenger
     'cl-log:text-file-messenger
     :name :db
     :filename (make-pathname :directory log-dir :name "db" :type "log")
     :category :db)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :orphan
     :filename (make-pathname :directory log-dir :name "orphan" :type "log")
     :category :orphan)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :info
     :filename (make-pathname :directory log-dir :name "info" :type "log")
     :category :info)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :warning
     :filename (make-pathname :directory log-dir :name "warning" :type "log")
     :category :warning)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :error
     :filename (make-pathname :directory log-dir :name "error" :type "log")
     :category :error)

    (cl-log:start-messenger
     'cl-log:text-file-messenger 
     :name :debug
     :filename (make-pathname :directory log-dir :name "debug" :type "log")
     :category :debug)

    (cl-log:start-messenger
     'cl-log:text-stream-messenger
     :name :stream
     :stream *error-output*
     :category :debug)))

(defmethod cl-log:format-message ((self cl-log:formatted-message))
  (if (eq (cl-log:message-category self) :access)
      (destructuring-bind (remote-addr*
                           header-in*
                           authorization
                           ;;iso-time
                           request-method*
                           script-name*
                           query-string*
                           server-protocol*
                           return-code
                           content
                           content-length
                           referer
                           user-agent)
          (cl-log:message-arguments self)                  
        (format nil
                "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                      ~A\" ~A ~:[~*-~;~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
                remote-addr*
                header-in*
                authorization
                (timestring (cl-log:timestamp-universal-time
                             (cl-log:message-timestamp self)))
                request-method*
                script-name*
                query-string*
                server-protocol*
                return-code
                content
                content-length
                referer
                user-agent))
      (format nil "~A ~A ~?~&"
              (timestring (cl-log:timestamp-universal-time
                           (cl-log:message-timestamp self)))
              (cl-log:message-category self)
              (cl-log:message-description self)
              (cl-log:message-arguments self))))

(defun timestring (time)
  "ISO 8601 representation of time."
  (multiple-value-bind (whole-seconds remainder) (floor time)
    (when (zerop remainder) (setf remainder nil))
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time whole-seconds 0)
      (format
       nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~@[~0F~]Z"
       year month date hour minute second remainder))))

(defun log-http-access (&key return-code content content-length)
  "Log HTTP access.  Use as :access-logger in a hunchentoot:accessor."
  (cl-log:log-message :access nil
                      (hunchentoot:remote-addr*)
                      (hunchentoot:header-in* :x-forwarded-for)
                      (hunchentoot:authorization)
                      ;;(hunchentoot:iso-time)
                      (hunchentoot:request-method*)
                      (hunchentoot:script-name*)
                      (hunchentoot:query-string*)
                      (hunchentoot:server-protocol*)
                      return-code
                      content
                      content-length
                      (hunchentoot:referer)
                      (hunchentoot:user-agent)))

(defun log-hunchentoot-message (severity format-string &rest args)
  "Log hunchentoot messages.  Use as :message-logger in a
hunchentoot:accessor.  For severity, hunchentoot uses :info, :warning,
and :error."
  (cl-log:log-message severity "~?" format-string args))