(defsystem :phoros
  :description                          ;goes with --version output
  "PHOROS (Photogrammetric Road Survey)"
  :author "Bert Burgemeister"
  :maintainer "Bert Burgemeister"
  :long-description                     ;goes with --help output
  "TODO: write blurb"
  ;; :version is major.minor.revision where
  ;; different major means data incompatibility,
  ;; different minor means changed feature set,
  ;; different revision means any other change.
  :version "0.0.0"
  :licence "GPL"
  :serial t
  :components ((:file "package")
               (:file "proj4")
               (:file "log")
               (:file "phoros")
               (:file "blurb")
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
               :command-line-arguments
               :cl-utilities
               :cl-log))
