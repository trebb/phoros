;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :phoros
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "phoros"))
  :depends-on (#+nil :cl-ppcre))
