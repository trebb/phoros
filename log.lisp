(in-package :phoros)

(cl-log:defcategory :orphan)
(cl-log:defcategory :warning)
(cl-log:defcategory :debug (or :debug :warning :orphan))

(setf (cl-log:log-manager)
      (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message))

(cl-log:start-messenger 'cl-log:text-file-messenger 
                        :name :orphan
                        :filename "orphans.log"
                        :category :orphan)

(cl-log:start-messenger 'cl-log:text-file-messenger 
                        :name :debug
                        :filename "debug.log"
                        :category :debug)

(cl-log:start-messenger 'cl-log:text-stream-messenger
                        :name :stream
                        :stream *error-output*
                        :category :warning)

(defmethod cl-log:format-message ((self cl-log:formatted-message))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (cl-log:timestamp-universal-time
                              (cl-log:message-timestamp self)))
    (declare (ignore day daylight-p))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[+~;-~]~2,'0:D ~A ~?~&"
            year month date hour minute second (minusp zone) (abs zone)
            (cl-log:message-category self)
            (cl-log:message-description self)
            (cl-log:message-arguments self))))
