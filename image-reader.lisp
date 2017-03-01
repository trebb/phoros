;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010, 2011, 2012, 2016, 2017 Bert Burgemeister
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


(in-package :img)

(defparameter *picture-header-length-tolerance* 20
  "Amount of leeway for the length of a picture header in a .pictures
  file.")

(defun find-keyword-in-stream (stream keyword &optional
                               (start-position 0 start-position-p)
                               search-range)
  "Return file-position in binary stream after first occurence of
keyword, or nil if the search is unsuccessful.  Return nil if
start-position is explicitly nil."
  (unless (and start-position-p
               (null start-position))
    (unless start-position-p (setf start-position 0))
    (let* ((keyword-size (length keyword))
           (keyword-bytes (map 'vector #'char-code keyword))
           (chunk-max-size 300)
           (chunk (make-array (list (+ chunk-max-size (1- keyword-size)))
                              :element-type '(unsigned-byte 8)))
           (end-position-in-stream (if search-range
                                       (+ start-position search-range)
                                       most-positive-fixnum)))
      (loop
         for chunk-start-in-stream from start-position to end-position-in-stream by chunk-max-size
         for chunk-size = (progn (file-position stream chunk-start-in-stream)
                                 (read-sequence chunk stream))
         for end-in-chunk = (min chunk-size (- end-position-in-stream
                                               chunk-start-in-stream))
         while (plusp chunk-size)
         do (loop
               for i from 0 to end-in-chunk
               for correct-characters = (mismatch keyword-bytes chunk
                                                  :start2 i
                                                  :end2 end-in-chunk)
               do (when (or (null correct-characters)
                            (= correct-characters keyword-size))
                    (return-from find-keyword-in-stream
                      (+ chunk-start-in-stream i keyword-size))))))))

(defun find-keyword-value (path keyword &optional start-position search-range)
  "Return value associated with keyword."
  (let ((start-of-value
         (find-keyword path keyword start-position search-range)))
    (when start-of-value
      (with-open-file (stream path)
        (file-position stream start-of-value)
        (car (read-delimited-list #\; stream))))))

(defun find-keyword (path keyword &optional (start-position 0) search-range)
  "Return file-position after keyword."
  (with-open-file (stream path :element-type 'unsigned-byte)
    (find-keyword-in-stream stream keyword start-position search-range)))

(cffi:defcstruct mem-encode
    (buffer :pointer)
  (size :int))

(defun* send-png (output-stream path start
                                &key (color-raiser #(1 1 1))
                                reversep brightenp
                                &mandatory-key bayer-pattern)
  "Read an image at position start in .pictures file at path and send
it to the binary output-stream.  Return UNIX trigger-time of image.
If brightenp is t, have it brightened up if necessary.  If reversep is
t, turn it upside-down.  Bayer-pattern is applied after turning, which
is a wart."
  ;; TODO: bayer-pattern should be applied to the unturned image
  (let ((blob-start (find-keyword path "PICTUREDATA_BEGIN" start))
        (blob-size (find-keyword-value path "dataSize=" start))
        (image-height (find-keyword-value path "height=" start))
        (image-width (find-keyword-value path "width=" start))
        (compression-mode (find-keyword-value path "compressed=" start))
        (channels (find-keyword-value path "channels=" start))
        (trigger-time (find-keyword-value path "timeTrigger=" start))
        (demosaic-fast t))
    (cffi:with-foreign-objects ((baypat :int 3)
                                (colr-raisr :double 3)
                                (mem-png 'mem-encode))
      (loop
         for i from 0 below (min 4 (first (array-dimensions bayer-pattern))) do
           (setf (cffi:mem-aref baypat :int i) (aref bayer-pattern i)))
      (loop
         for i from 0 to 2 do
           (setf (cffi:mem-aref colr-raisr :double i)
                 (coerce (aref color-raiser i) 'double-float)))
      (let ((png2mem-exit
             (imread:png2mem (namestring path) blob-start blob-size
                             image-width image-height channels
                             baypat demosaic-fast compression-mode
                             mem-png reversep brightenp colr-raisr)))
        (cond ((zerop png2mem-exit)
               (cffi:with-foreign-slots ((buffer size) mem-png mem-encode)
                 (loop
                    for i from 0 below size do
                      (write-byte (cffi:mem-aref buffer :unsigned-char i) output-stream))
                 (unless (cffi:null-pointer-p buffer) (cffi:foreign-free buffer))))
              ((= 1 png2mem-exit)
               (error "Input file ~A not found." path))
              ((or (= 2 png2mem-exit) (= 3 png2mem-exit))
               (error "Don't know how to deal with a bayer-pattern of ~A."
                      bayer-pattern))
              ((= 5 png2mem-exit)
               (error "Unknown compression mode ~A in ~A."
                      compression-mode path))
              ((= 6 png2mem-exit)
               (error "Don't know how to deal with ~D-channel pixels." channels))
              ((= 71 png2mem-exit)
               (error "JPEG decompression error."))
              ((= 72 png2mem-exit)
               (error "JPEG discarded.  It was bigger than expected."))
              ((= 73 png2mem-exit)
               (error "JPEG reversing not implemented."))
              ((= 74 png2mem-exit)
               (error "JPEG brightening not implemented."))
              ((= 75 png2mem-exit)
               (error "Couldn't allocate memory for uncompressed image."))
              ((= 76 png2mem-exit)
               (error "Couldn't allocate buffer for image data input."))
              ((= 11 png2mem-exit)
               (error "PNG error: create_write_struct()."))
              ((= 12 png2mem-exit)
               (error "PNG error: create_info_struct()"))
              ((= 13 png2mem-exit)
               (error "Error during PNG setup."))
              ((= 21 png2mem-exit)
               (error "Error while writing PNG row."))
              ((= 31 png2mem-exit)
               (error "Couldn't allocate memory for huffman table."))
              ((= 32 png2mem-exit)
               (error "Huffman decoder out of step."))
              (t
               (error "Can't unpack image.")))))
    trigger-time))

(defun find-nth-picture (n path)
  "Find file-position of zero-indexed nth picture in in .pictures file
at path."
  (let ((estimated-header-length
         (- (find-keyword path "PICTUREHEADER_END")
            (find-keyword path "PICTUREHEADER_BEGIN")
            *picture-header-length-tolerance*))) ; allow for variation in dataSize and a few other parameters
    (loop
       for i from 0 to n
       for picture-start =
       (find-keyword path "PICTUREHEADER_BEGIN" 0) then
       (find-keyword path "PICTUREHEADER_BEGIN"
                     (+ picture-start picture-length estimated-header-length))
       for picture-length = (find-keyword-value path
                                                "dataSize=" picture-start)
       finally (return (- picture-start (length "PICTUREHEADER_BEGIN"))))))

(defun* send-nth-png (n output-stream path
                     &key (color-raiser #(1 1 1))
                     reversep brightenp
                     &mandatory-key bayer-pattern)
  "Read image number n (zero-indexed) in .pictures file at path and
send it to the binary output-stream.  Return UNIX trigger-time of
image."
  (send-png output-stream path (find-nth-picture n path)
            :bayer-pattern bayer-pattern
            :reversep reversep
            :brightenp brightenp
            :color-raiser color-raiser))


;; TODO: (perhaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect 4 single color pixels into a three-color one
;; enhance contrast of grayscale images
