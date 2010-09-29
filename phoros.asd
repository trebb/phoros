(defsystem :phoros
  :serial t
  :components ((:file "package")
               (:file "phoros"))
  :depends-on (:photogrammetrie :hunchentoot :cl-who :parenscript :cl-json :postmodern))
