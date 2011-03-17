(defsystem :phoros

  :description                          ;goes with --version output
  "PHOROS (Photogrammetric Road Survey)"

  :author "Bert Burgemeister"

  :maintainer "Bert Burgemeister"

  :long-description                     ;goes with --help output
  "Phoros is a facility for photogrammetric road survey.
It stores data in a PostgreSQL database and then makes it available
over a web interface."

  ;; :version is major.minor.revision where
  ;; different major means data incompatibility,
  ;; different minor means changed feature set,
  ;; different revision means any other change.
  :version "0.2.1"

  :licence
  "Copyright (C) 2011 Bert Burgemeister

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA."

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
;; TODO: put Phoros homepage somewhere.