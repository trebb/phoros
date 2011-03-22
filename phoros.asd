(defsystem :phoros

  :description                          ;goes with --version output
  "PHOROS (Photogrammetric Road Survey)"

  :author "Bert Burgemeister <trebbu@googlemail.com>"

  :maintainer "Bert Burgemeister <trebbu@googlemail.com>"

  :long-description                     ;goes with --help output
  "Phoros is a tool for photogrammetric road survey.  It stores data
in a PostgreSQL database and then makes it available over a web
interface.                              http://phoros.berlios.de"

  :version "0.2.2"
  ;; :version is MAJOR.MINOR.REVISION where
  ;; different MAJOR means data incompatibility,
  ;; different MINOR means changed feature set,
  ;; different REVISION means any other change.

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

  :depends-on (:phoml
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
