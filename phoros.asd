(defsystem :phoros
  :serial t
  :components ((:file "package")
               (:file "phoros")
               (:file "read-pictures-file"))
  :depends-on (:photogrammetrie :hunchentoot :cl-who :parenscript :cl-json :postmodern :zpng))
