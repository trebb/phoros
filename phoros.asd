(defsystem :phoros
  :description "PHOROS (Photogrammetric Road Survey)"
  :author "Bert Burgemeister"
  :maintainer "Bert Burgemeister"
  :long-description "TODO"
  :version "0.0"
  :licence "GPL"
  :serial t
  :components ((:file "package")
               (:file "log")
               (:file "phoros")
               (:file "pictures-file")
               (:file "db-tables")
               (:file "stuff-db")
               (:file "cli"))
  :depends-on (:photogrammetrie
               :hunchentoot
               :cl-who
               :parenscript
               :cl-json
               :postmodern
               :zpng
               :drakma
               :trivial-shell
               :command-line-arguments
               :cl-log))
