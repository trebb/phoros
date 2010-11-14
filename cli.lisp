(in-package :phoros)

(defparameter *cli-db-options*
  '(("host" :type string :initial-value "localhost" :documentation "Database server.")
    ("port" :type integer :initial-value 5432 :documentation "Port on database server.")
    ("database" :type string :initial-value "phoros" :documentation "Name of database.")
    ("user" :type string :documentation "Database user.")
    ("password" :type string :documentation "Database user's password.")
    ("use-ssl" :type string :initial-value "no" :documentation "Use SSL in database connection. [yes|no|try]")))

(defparameter *cli-main-options*
  '((("help" #\h) :action #'cli-help-action  :documentation "Print this help and exit.")
    (("version") :action #'cli-version-action :documentation "Output version information and exit.")
    ("check-db" :action #'check-db-action :documentation "Check database connection and exit.")))

(defparameter *opt-spec* (append *cli-main-options* *cli-db-options*))

(defun main ()
  "The UNIX command line entry point."
  (handler-case
      (command-line-arguments:compute-and-process-command-line-options *opt-spec*)
    (error (e) (format *error-output* "~A~&" e))))

(defun cli-help-action (&rest rest)
  "Print --help message."
  (declare (ignore rest))
  (format *standard-output* "~&Usage: ...~&~A"
          (asdf:system-long-description (asdf:find-system :phoros)))
  (command-line-arguments:show-option-help *opt-spec*))

(defun cli-version-action (&rest rest)
  "Print --version message."
  (declare (ignore rest))
  (format *standard-output* "~&~A ~A~%"
          (asdf:system-description (asdf:find-system :phoros))
          (asdf:component-version (asdf:find-system :phoros))))

(defun check-db-action (&rest rest)
  (declare (ignore rest))
  (apply #'check-database-connection (command-line-arguments:process-command-line-options *opt-spec* command-line-arguments:*command-line-arguments*)))

(defun check-database-connection (&key host port database (user "") (password "") use-ssl &allow-other-keys)
  (let (connection)
    (handler-case
        (setf
         connection
         (connect database user password host :port port
                  :use-ssl (cond ((string-equal use-ssl "no") :no)
                                 ((string-equal use-ssl "yes") :yes)
                                 ((string-equal use-ssl "try") :try))))
      (error (e) (format *error-output* "~A~&" e)))
    (when connection
      (disconnect connection)
      (format *error-output* "~&OK~%"))))
