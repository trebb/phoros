(in-package :phoros)

(cl-log:defcategory :db-sys)
(cl-log:defcategory :db-dat)
(cl-log:defcategory :orphan)
(cl-log:defcategory :warning)
(cl-log:defcategory :db (or :db-sys :db-dat :warning))
(cl-log:defcategory :debug (or :debug :orphan))

(defun launch-logger (log-dir)
  (let ((log-dir (pathname-directory log-dir)))
    (setf (cl-log:log-manager)
          (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message))
    (cl-log:start-messenger 'cl-log:text-file-messenger 
                            :name :orphan
                            :filename (make-pathname :directory log-dir :name "orphans" :type "log")
                            :category :orphan)
    (cl-log:start-messenger 'cl-log:text-file-messenger 
                            :name :debug
                            :filename (make-pathname :directory log-dir :name "debug" :type "log")
                            :category :debug)
    (cl-log:start-messenger 'cl-log:text-file-messenger
                            :name :db
                            :filename (make-pathname :directory log-dir :name "phoros" :type "log")
                            :category :db)
    (cl-log:start-messenger 'cl-log:text-stream-messenger
                            :name :stream
                            :stream *error-output*
                            :category :warning)))

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