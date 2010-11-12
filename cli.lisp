(in-package :phoros)

(defparameter *opt-spec*
 '((("all" #\a) :type boolean :documentation "do it all")
   ("blah" :type string :initial-value "blob" :documentation "This is a very long multi line documentation. The function SHOW-OPTION-HELP should display this properly indented, that is all lines should start at the same column.")
   (("help" #\h) :action #'cli-help :documentation "Display this help and exit.")
   (("version") :action #'cli-version :documentation "Output version information and exit.")))

(defun cli-help (&rest rest)
  "Print --help message."
  (format *standard-output* "~&Usage: ...~&~A"
          (asdf:system-long-description (asdf:find-system :phoros)))
  (command-line-arguments:show-option-help *opt-spec*))

(defun cli-version (&rest rest)
  "Print --version message."
  (format *standard-output* "~&~A ~A~&"
          (asdf:system-description (asdf:find-system :phoros))
          (asdf:component-version (asdf:find-system :phoros))))

(defun main ()
  "The UNIX command line entry point."
  (let ((arglist
         (command-line-arguments:handle-command-line *opt-spec* #'list)))))
