(in-package :phoros)

(cl-log:defcategory :db)
(cl-log:defcategory :orphan)
(cl-log:defcategory :warning)
(cl-log:defcategory :debug (or :debug :db :warning :orphan))

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
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore day daylight-p))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[+~;-~]~2,'0:D" ; flipping sign of timezone
            year month date hour minute second (plusp zone) (abs zone))))