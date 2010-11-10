(defsystem :phoros
  :serial t
  :components ((:file "package")
               (:file "phoros")
               (:file "read-pictures-file")
               (:file "db-tables")
               (:file "stuff-db"))
  :depends-on (:photogrammetrie
               :hunchentoot
               :cl-who
               :parenscript
               :cl-json
               :postmodern
               :zpng
               :drakma
               :trivial-shell
               :unix-options))
;; try: cl-log