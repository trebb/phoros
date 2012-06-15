;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010, 2011, 2012 Bert Burgemeister
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


(defpackage :phoros-photogrammetry
  (:documentation "Interface to the PhoML library.")
  (:use :cl
        :phoml)
  (:export :photogrammetry
           :*photogrammetry-mutex*
           :del-all))

(defpackage :phoros
  (:use :cl
        :phoros-photogrammetry
        :parenscript
        :postmodern)
  (:export :defun*
           :unqualified-symbol
           :phoml
           :launch-logger
           :muffle-postgresql-warnings
           :*number-of-images*
           :*aux-numeric-labels*
           :*aux-text-labels*
           :*login-intro*
           :*postgresql-credentials*
           :*postgresql-aux-credentials*
           :phoros-version
           :check-db
           :check-dependencies
           :nuke-all-tables
           :create-sys-tables
           :create-acquisition-project
           :assert-acquisition-project
           :delete-acquisition-project
           :delete-measurement
           :store-images-and-points
           :delete-imageless-points
           :insert-footprints
           :store-camera-hardware
           :store-lens
           :store-generic-device
           :store-device-stage-of-life
           :store-device-stage-of-life-end
           :store-camera-calibration
           :timestring
           :create-presentation-project
           :assert-presentation-project
           :user-point-table-name
           :user-line-table-name
           :delete-presentation-project
           :add-to-presentation-project
           :remove-from-presentation-project
           :create-image-attribute
           :delete-image-attribute
           :create-presentation-project-trigger-function
           :fire-presentation-project-trigger-function
           :aux-view-exists-p
           :aux-point-view-name
           :delete-aux-view
           :create-aux-view
           :add-spherical-mercator-ref
           :store-user-points
           :get-user-points
           :create-user
           :delete-user
           :insert-all-footprints
           :delete-all-imageless-points
           :start-server
           :utc-from-unix
           :thread-aux-points-function-name))

(defpackage :phoros-image-reader
  (:documentation "The part of Phoros that makes servable images from
  raw files of measuring data.")
  (:nicknames :img)
  (:use :cl
        :phoros)
  (:export :send-png
           :send-nth-png))

(defpackage :phoros-command-line-interface
  (:documentation "The part of Phoros that provides its UNIX command
  line interface.")
  (:nicknames :cli)
  (:use :cl
        :phoros
        :postmodern)
  (:import-from :com.dvlsoft.clon
                :defsynopsis
                :make-stropt
                :make-path
                :make-lispobj
                :make-switch
                :make-context
                :with-context
                :cmdline
                :getopt
                :do-cmdline-options
                :help)
  (:export :help ;re-export from com.dvlsoft.clon
           :with-options ;Phoros CLI stuff
           :verbosity-level
           :main
           :set-umask))
