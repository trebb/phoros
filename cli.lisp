(in-package :phoros)

(defparameter *opt-spec*
 '((("all" #\a) :type boolean :documentation "do it all")
   ("blah" :type string :initial-value "blob" :documentation "This is a very long multi line documentation. The function SHOW-OPTION-HELP should display this properly indented, that is all lines should start at the same column.")
   ("check-db" :action #'check-database-connection :documentation "Check usability of database connection and exit.")
   ("host" :type string :action *pg-host* :documentation "Database server.")
   ("port" :type integer :action *pg-port* :documentation "Port on database server.")
   ("database" :type string :action *pg-database* :documentation "Name of database.")
   ("user" :type string :action *pg-user* :documentation "Database user.")
   ("password" :type string :action *pg-password* :documentation "Database user's password.")
   ("use-ssl" :type string :action *pg-use-ssl* :documentation "Use SSL in database connection.")
   (("help" #\h) :action #'cli-help :documentation "Display this help and exit.")
   (("version") :action #'cli-version :documentation "Output version information and exit.")))

(defun main ()
  "The UNIX command line entry point."
  (let ((arglist
         (command-line-arguments:handle-command-line *opt-spec* #'list)))))

(defun cli-help (&rest rest)
  "Print --help message."
  (declare (ignore rest))
  (format *standard-output* "~&Usage: ...~&~A"
          (asdf:system-long-description (asdf:find-system :phoros)))
  (command-line-arguments:show-option-help *opt-spec*))

(defun cli-version (&rest rest)
  "Print --version message."
  (declare (ignore rest))
  (format *standard-output* "~&~A ~A~&"
          (asdf:system-description (asdf:find-system :phoros))
          (asdf:component-version (asdf:find-system :phoros))))

(defparameter *pg-database* "")
(defparameter *pg-user* "")
(defparameter *pg-password* "")
(defparameter *pg-host* "")
(defparameter *pg-port* 5432)
(defparameter *pg-use-ssl* :no)

(defun check-database-connection (&rest rest)
  (declare (ignore rest))
  (command-line-arguments:handle-command-line *opt-spec* #'list)
  (let (connection)
    (handler-case
        (setf
         connection
         (connect *pg-database* *pg-user* *pg-password* *pg-host* :port *pg-port* :use-ssl *pg-use-ssl*))
      (error (e) (format t "~A~&" e)))
    (and connection (disconnect connection))))


