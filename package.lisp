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
        :postmodern))

(defpackage :phoros-command-line-interface
  (:documentation "The part of Phoros that provides its UNIX command
  line interface.")
  (:nicknames :cli)
  (:use :cl)
  (:import-from :command-line-arguments
                :compute-and-process-command-line-options
                :*command-line-arguments*
                :show-option-help
                :process-command-line-options)
  (:export :compute-and-process-command-line-options
           :*command-line-arguments*
           :show-option-help
           :process-command-line-options
           :*general-options*
           :*db-connection-options*
           :*aux-db-connection-options*
           :*get-image-options*
           :*camera-hardware-options*
           :*lens-options*
           :*generic-device-options*
           :*device-stage-of-life-options*
           :*device-stage-of-life-end-options*
           :*camera-calibration-options*
           :*acquisition-project-options*
           :*store-images-and-points-options*
           :*start-server-options*
           :*presentation-project-options*
           :*aux-view-options*
           :*user-points-options*
           :*user-options*
           :*options*
           :.phoros-options
           :with-options
           :remaining-options
           :help-action
           :version-action
           :licence-action
           :main
           :check-db-action
           :check-dependencies-action
           :nuke-all-tables-action
           :create-sys-tables-action
           :create-acquisition-project-action
           :delete-acquisition-project-action
           :delete-measurement-action
           :list-acquisition-project-action
           :store-images-and-points-action
           :insert-footprints-action
           :canonicalize-bayer-pattern
           :canonicalize-color-raiser
           :store-stuff
           :store-camera-hardware-action
           :store-lens-action
           :store-generic-device-action
           :store-device-stage-of-life-action
           :store-device-stage-of-life-end-action
           :store-camera-calibration-action
           :get-image-action
           :create-presentation-project-action
           :delete-presentation-project-action
           :add-to-presentation-project-action
           :remove-from-presentation-project-action
           :redefine-trigger-function-action
           :create-aux-view-action
           :store-user-points-action
           :get-user-points-action
           :create-user-action
           :delete-user-action
           :list-user-action
           :list-presentation-project-action
           :format-table
           :server-action))
