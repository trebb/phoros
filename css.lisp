;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2011, 2012 Bert Burgemeister
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

(in-package :phoros)

(hunchentoot:define-easy-handler
    (style.css
     :uri (format nil "/phoros/lib/css-~A/style.css" (phoros-version)))
    ()
  ""
  (setf (hunchentoot:content-type*) "text/css")
  (format nil "
/**
 * CSS Reset
 * From Blueprint reset.css
 * http://blueprintcss.googlecode.com
 */
html, body, div, span, object, iframe, h1, h2, h3, h4, h5, h6, p, blockquote, pre, a, abbr, acronym, address, code, del, dfn, em, img, q, dl, dt, dd, fieldset, form, label, legend, table, caption, tbody, tfoot, thead, tr, th, td {margin:0;padding:0;border
:0;font-weight:inherit;font-style:inherit;font-size:100%;font-family:inherit;vertical-align:baseline;}
body {line-height:1.5;}
table {border-collapse:separate;border-spacing:0;}
caption, th, td {text-align:left;font-weight:normal;}
table, td, th {vertical-align:middle;}
blockquote:before, blockquote:after, q:before, q:after {content:'';}
blockquote, q {quotes:'' '';}
a img {border:none;}

/**
 * Basic Typography
 * From OpenLayers style.css
 * http://openlayers.org
 */
body {
    font-family: 'Lucida Grande', Verdana, Geneva, Lucida, Arial, Helvetica, sans-serif;
    font-size: 80%;
    color: #222;
    background: #fff;
    margin: 1em 1.5em;
}
pre, code {
    margin: 1.5em 0;
    white-space: pre;
}
pre, code {
    font-family: 'andale mono', 'lucida console', monospace;
    line-height:1.5;
}
a[href] {
    color: #436976;
    background-color: transparent;
}
h1, h2, h3, h4, h5, h6 {
    color: #003a6b;
    background-color: transparent;
    font: 100% 'Lucida Grande', Verdana, Geneva, Lucida, Arial, Helvetica, sans-serif;
    margin: 0;
    padding-top: 0.5em;
}
h1 {
    font-size: 130%;
    margin-bottom: 0.5em;
    border-bottom: 1px solid #fcb100;
}
h2 {
    font-size: 120%;
    margin-bottom: 0.5em;
    border-bottom: 1px solid #aaa;
}
h3 {
    font-size: 110%;
    margin-bottom: 0.5em;
    text-decoration: underline;
}
h4 {
    font-size: 100%;
    font-weight: bold;
}
h5 {
    font-size: 100%;
    font-weight: bold;
}
h6 {
    font-size: 80%;
    font-weight: bold;
}

/**
 * Phoros Specific
 */
button, select {
    cursor: pointer;
    vertical-align: middle;
}
#download-user-points-button {
    float: left;
    height: 25px;
    margin-right: 2px;
}
#blurb-button {
    float: left;
    height: 25px;
    padding-bottom: 1px;
}
#logout-button {
    float:right;
    height: 25px;
}
#h2-help {
    clear: both;
}
.h1-right {
    float: right;
}
#caching-indicator {
    background: url(/~@*~A/lib/ol/theme/theme/default/img/save_features_off.png) no-repeat center center;
    float: left;
    margin-top: 3px;
    margin-right: 15px;
    height: 16px;
    width: 16px;
}
.vanilla-input {
    font-family: 'andale mono', 'lucida console', monospace;
    padding: 0px;
    margin: 0px;
}
.tight-input {
    padding: 0px;
    margin-left: 0px;
    margin-right: 0px;
    margin-top: 3px;
    margin-bottom: 2px;
}
.combobox {
    height: 20px;
}
.combobox-select {
    font-family: 'andale mono', 'lucida console', monospace;
    position:absolute;
    height: 20px;
    border-width: 2px;
}
.combobox-input {
    font-family: 'andale mono', 'lucida console', monospace;
    position:absolute;
    height: 16px;
    border-width: 0px;
    margin: 2px 18px 2px 2px;
    padding: 0 0px 0 0;
}
#point-description {
    float: left;
    width: 210px;
    margin-bottom: 2px;
}
#point-description-select {
    width: 210px;
}
#point-description-input {
    width: 190px;
}
#point-numeric-description {
    width: 118px;
    height: 16px;
    float: right;
}
#point-attribute {
    float: left;
    width: 86px;
}
#point-attribute-select {
    width: 86px;
}
#point-attribute-input {
    width: 66px;
}
#finish-point-button {
    float:right;
    width: 175px;
    height: 30px;
}
#uniquify-buttons {
    float:right;
    width: 175px;
    height: 30px;
}
#suggest-unique-button {
    float: left;
    width: 130px;
    height: 30px;
}
#force-duplicate-button {
    float: left;
    width: 45px;
    height: 30px;
}
#delete-point-button {
    float: left;
    width: 33px;
    height: 30px;
}
#aux-point-distance-or-point-creation-date {
    float: left;
    width: 210px;
    height: 21px;
    margin-top: 5px;
}
#aux-point-distance {
    width: 100px;
    height: 21px;
    float: left;
}
#include-aux-data {
    float: right;
}
#point-creation-date {
    padding-left: 5px;
}
#aux-data {
    float: left;
    width: 210px;
    height: 145px;
    margin-top: 5px;
    overflow: auto;
    line-height: 1;
}
.aux-data-table {
}
.aux-data-label {
    padding-right: 5px;
}
.aux-data-value {
    font-family: 'andale mono', 'lucida console', monospace;
}
.phoros-controls {
    float: left;
    text-align:left;
    width: 210px;
    height: 323px;
}
#real-phoros-controls, #multiple-points-phoros-controls {
    height: 285px;
    width: 210px;
}
.help-div {
    float: left;
    cursor: default;
    background-image: url(/~@*~A/lib/public_html/phoros-logo-background.png);
    background-position: 40px 90px;
    background-repeat: no-repeat;
    width: 256px;
    height: 320px;
    margin-left: 1em;
}
.controlled-streetmap {
    float: left;
    margin-left: 1px;
    margin-right: 1em;
    margin-top: 1px;
    margin-bottom: 1px;
    border: 1px solid #00008B;
}
.streetmap {
    float: left;
    width: 512px;
    height: 320px;
}
.streetmap-controls {
    float: left;
    width: 200px;
    height: 320px;
    line-height: 1;
    background-color: #00008B;
}
.streetmap-zoom {
    height: 18px;
    width: 126px;
    clear: both;
}
.streetmap-layer-switcher {
    /* height: 92px; */
}
.streetmap-layer-switcher span {    /*layer name*/
    cursor: pointer;
    color: white; 
}
.dataLayersDiv {
    cursor: pointer;            /*TODO: doesn't work*/
}
.streetmap-vertical-strut {
    float: right;
    height: 174px;
    width: px;
    background-color: black;
}
.streetmap-mouse-position {
    font-family: 'andale mono', 'lucida console', monospace;
    color: white;
    clear: both;
    float: left;
    height: 18px;
    margin-left: 5px;
}
.streetmap-overview {
    width: 190px;
    height: 110px;
    clear: both;
    float: left;
}
.image-main-controls, .walk-mode-controls {
    height: 18px;
    background-color: #00008B;
    margin-bottom: 2px;
    clear: right;
}
#step-button {
    color: #00008B;
    cursor: pointer;
    font-size: smaller;
    font-weight: bold;
    background-color: white;
    float: right;
    height: 12px;
    padding-left: 3px;
    padding-right: 3px;
    padding-bottom: 2px;
    margin-top: 2px;
    margin-bottom: 2px;
    margin-left: 10px;
    margin-right: 5px;
}
#remove-work-layers-button {
    color: #00008B;
    cursor: pointer;
    font-size: smaller;
    font-weight: bold;
    background-color: white;
    float: right;
    height: 12px;
    padding-left: 3px;
    padding-right: 3px;
    padding-bottom: 2px;
    margin-top: 2px;
    margin-bottom: 2px;
    margin-left: 6px;
    margin-right: 5px;
}
#auto-zoom, #walk-mode, #brighten-images {
    height: 18px;
    font-size: smaller;
    color: white;
    float: left;
    margin-left: 5px;
    margin-right: 5px;
}
#step-size {
    font-family: 'andale mono', 'lucida console', monospace;
    color: white;
    float: left;
    height: 18px;
    cursor: pointer;
}
#auto-zoom, #walk-mode, #include-aux-data-p, label {
    cursor: pointer;
}
#zoom-images-to-max-extent {
    background: url(/~@*~A/lib/ol/img/zoom-world-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
    margin-left: 10px;
    margin-right: 6px;
}    
#no-footprints-p {
    color: red;
    float: left;
    width: 8px;
    height: 18px;
    font-size: small;
}
.image-controls {
    width: 100%;
    height: 18px;
    background-color: #00008B;
}
.image-zoom {
    height: 18px;
    width: 126px;
    float: left;
}
.image-layer-switcher {
    cursor: pointer;            /*TODO: doesn't work*/
    height: 18px;
    float: left;
}
.image-usable {
    color: red;
    float: left;
    width: 5px;
    height: 18px;
    font-size: small;
}
.image-trigger-time {
    font: smaller 'andale mono', 'lucida console', monospace;
    color: white; 
    float: right;
    height: 15px;
    margin-right: 5px;
    margin-top: 3px;
    float: right;
}
.dataLbl, .baseLbl {            /*of layer-switcher*/
    display: none;
}
.image-layer-switcher span {    /*layer name*/
    display: none; 
}
.image {
    background-image: url(/~@*~A/lib/public_html/phoros-logo-background.png);
    background-position: center;
    background-repeat: no-repeat;
    /* width and height are read via CSS DOM where we expect them
       to be in px. */
    width: 300px;
    height: 256px;
}
.controlled-image {
    float: left;
    border: 1px solid #00008B;
    margin: 1px;
}
.olControlPanWestItemInactive {
    background: url(/~@*~A/lib/ol/img/west-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlPanEastItemInactive {
    background: url(/~@*~A/lib/ol/img/east-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlPanNorthItemInactive {
    background: url(/~@*~A/lib/ol/img/north-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlPanSouthItemInactive {
    background: url(/~@*~A/lib/ol/img/south-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlZoomInItemInactive, #increase-step-size {
    background: url(/~@*~A/lib/ol/img/zoom-plus-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlZoomOutItemInactive, #decrease-step-size {
    background: url(/~@*~A/lib/ol/img/zoom-minus-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlZoomToMaxExtentItemInactive {
    background: url(/~@*~A/lib/ol/img/zoom-world-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olControlScaleLineBottom {     /* Imperial units */
    display: none;
}
.streetmapZoomToMaxExtentItemInactive {
    background: url(/~@*~A/lib/ol/img/zoom-world-mini.png) no-repeat;
    cursor: pointer;
    float: left;
    height: 18px;
    width: 18px;
}
.olLayerGoogleCopyright {
    display: none;
}
" *proxy-root*))

;; (pushnew (hunchentoot:create-folder-dispatcher-and-handler
;;           (format nil "/phoros/lib/css-~A/" (phoros-version)) "css/") ;TODO: merge this style.css into public_html/style.css
;;          hunchentoot:*dispatch-table*)


