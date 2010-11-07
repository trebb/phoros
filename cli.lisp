(in-package :phoros)

;;;Make an executable along the lines of:
;;; ../sbcl/bin/sbcl --eval '(ql:quickload "unix-options")' --load unix-cmd.lisp --eval '(sb-ext:save-lisp-and-die "cmd" :toplevel (function main) :executable t)'

(defun main ()
  (unix-options:with-cli-options
      ((unix-options:cli-options)
       "Usage:~%~@{~A~%~}~%end summary~%")
      ((plint ((#\Q prrinnt) nil "Print something.  Make this text rather really long.  Test the way it is formatted."))
       other-print
       &parameters
       (in ((#\I in-file) "some input" "The inner file."))
       out-file other-arg)
    (format t "~&~A~%~A~%~A~%~A" plint in out-file unix-options:free)))