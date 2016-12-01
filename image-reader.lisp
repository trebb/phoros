;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010, 2011, 2012, 2016 Bert Burgemeister
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

;;; See file phoros.asd for how to choose between three alternative
;;; image creation libraries, zpng cl-png, and phoros's own imread.so.


#+(or phoros-uses-cl-png phoros-uses-zpng)
(deftype image-dimension () '(unsigned-byte 16))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(deftype color () '(unsigned-byte 8))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(deftype channels () '(unsigned-byte 8))

#+phoros-uses-zpng
(deftype image ()
  "We are using zpng."
  '(simple-array color 3))

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

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun read-huffman-table (stream &optional start-position)
  "Return in a hash table a huffman table read from stream.  Start
either at stream's file position or at start-position."
  (let ((huffman-codes-start (if start-position
                                 start-position
                                 (file-position stream))))
    (file-position stream (+ (* 511 4) huffman-codes-start)) ; start of lengths
    (let* ((lengths (make-list 511))
           (huffman-table (make-hash-table :size 1000 :test #'equal)))
      (read-sequence lengths stream)
      (file-position stream huffman-codes-start)
      (loop
         for i from -255 to 255
         for length in lengths
         for key = (make-array (list length) :element-type 'bit)
         for code = (let ((raw (make-array '(4) :element-type 'unsigned-byte))
                          (code-part 0))
                      (read-sequence raw stream)
                      (loop
                         for raw-byte across raw
                         for code-position from 24 downto 0 by 8
                         do (setf code-part (dpb raw-byte
                                                 (byte 8 code-position)
                                                 code-part))
                         finally (return code-part)))
         unless (zerop length)
         do (loop
               for key-index from 0 below length
               for code-index downfrom (1- length)
               do (setf (sbit key key-index)
                        (ldb (byte 1 code-index) code)))
         and
         do (setf (gethash key huffman-table) i))
      huffman-table)))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun read-compressed-picture (stream start-position length)
  "Return a compressed picture in a bit array.  Start either at
start-position or, if that is nil, at stream's file position."
  (when start-position (file-position stream start-position))
  (let ((raw (make-array (list length) :element-type 'unsigned-byte))
        (compressed-picture
         (make-array (list (* 8 length)) :element-type 'bit)))
    (read-sequence raw stream)
    (loop
       for byte across raw
       for byte-position from 0
       do (loop
             for source-bit from 7 downto 0
             for destination-bit from 0 to 7
             do (setf (sbit compressed-picture
                            (+ destination-bit
                               (* 8 byte-position)))
                      (ldb (byte 1 source-bit) byte)))
       finally (return compressed-picture))))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun get-leading-byte (bit-array &optional (start 0) &aux (result 0))
  "Return integer made of eight bits from bit-array."
  (loop
     for bit-array-index from start
     for result-index from 7 downto 0
     for result = (dpb (sbit bit-array bit-array-index)
                       (byte 1 result-index) 0)
     then (dpb (sbit bit-array bit-array-index) (byte 1 result-index) result)
     finally (return result)))

#+phoros-uses-cl-png
(defun uncompress-picture (huffman-table compressed-picture
                           height width channels &key reversep)
  "Return the Bayer pattern extracted from compressed-picture, turned
upside-down if reversep is t, in an (array color (height
width channels)), everything in channel 0."
  (declare (optimize speed)
           (optimize (safety 0))
           (type (unsigned-byte 16) height width)
           (type vector compressed-picture))
  (let* ((uncompressed-image
          (png:make-image height width channels 8))
         (uncompressed-image-vector
          (make-array (list (* height width channels))
                      :element-type 'color
                      :displaced-to uncompressed-image))
         (channel (if reversep
                      (1- channels)     ;becomes 0 by reversal
                      0))
         (compressed-picture-index 0)
         (min-key-length
          (loop
             for code of-type simple-bit-vector being the hash-key in huffman-table
             minimize (length code)))
         (max-key-length
          (loop
             for code of-type simple-bit-vector being the hash-key in huffman-table
             maximize (length code))))
    (declare (type (signed-byte 48) compressed-picture-index)
             (type channels channels))
    (loop
       for row from 0 below height
       do
       (setf (aref uncompressed-image row 0 channel)
             (get-leading-byte compressed-picture
                               (prog1 compressed-picture-index
                                 (incf compressed-picture-index 8))))
       (setf (aref uncompressed-image row 1 channel)
             (get-leading-byte compressed-picture
                               (prog1 compressed-picture-index
                                 (incf compressed-picture-index 8))))
       (loop
          for column from 2 below width
          for try-start of-type (unsigned-byte 48) from compressed-picture-index
          do
          (loop
             for key-length from min-key-length to max-key-length
             for huffman-code = (subseq compressed-picture
                                        try-start (+ try-start key-length))
             for pixel-delta-maybe = (gethash huffman-code huffman-table)
             when pixel-delta-maybe
             do
             (setf (aref uncompressed-image row column channel)
                   (- (aref uncompressed-image row (- column 2) channel)
                      (the fixnum pixel-delta-maybe)))
             and do (incf try-start (1- key-length))
             and return nil
             finally (error
                      "Decoder out of step at row ~S, column ~S.  Giving up."
                      row column))
          finally
          (setf compressed-picture-index (1+ try-start))))
    (when reversep (reverse-displaced-vector uncompressed-image-vector))
    uncompressed-image))

#+phoros-uses-zpng
(defun uncompress-picture (huffman-table compressed-picture
                           height width channels &key reversep)
  "Return the Bayer pattern extracted from compressed-picture, turned
upside-down if reversep is t, in an (array color (height
width channels)), everything in channel 0."
  (declare (optimize speed)
           (optimize (safety 0))
           (type (unsigned-byte 16) height width)
           (type vector compressed-picture))
  (let* ((uncompressed-image
          (make-array (list height width channels)
                      :element-type 'color))
         (uncompressed-image-vector
          (make-array (list (* height width channels))
                      :element-type 'color
                      :displaced-to uncompressed-image))
        
         (channel (if reversep
                      (1- channels)     ;becomes 0 by reversal
                      0))
         (compressed-picture-index 0)
         (min-key-length
          (loop
             for code of-type simple-bit-vector
             being the hash-key in huffman-table
             minimize (length code)))
         (max-key-length
          (loop
             for code of-type simple-bit-vector
             being the hash-key in huffman-table
             maximize (length code))))
    (declare (type (signed-byte 48) compressed-picture-index)
             (type channels channels))
    (loop
       for row from 0 below height
       do
       (setf (aref uncompressed-image row 0 channel)
             (get-leading-byte compressed-picture
                               (prog1 compressed-picture-index
                                 (incf compressed-picture-index 8))))
       (setf (aref uncompressed-image row 1 channel)
             (get-leading-byte compressed-picture
                               (prog1 compressed-picture-index
                                 (incf compressed-picture-index 8))))
       (loop
          for column from 2 below width
          for try-start of-type (unsigned-byte 48) from compressed-picture-index
          do
          (loop
             for key-length from min-key-length to max-key-length
             for huffman-code = (subseq compressed-picture
                                        try-start (+ try-start key-length))
             for pixel-delta-maybe = (gethash huffman-code huffman-table)
             when pixel-delta-maybe
             do
             (setf (aref uncompressed-image row column channel)
                   (- (aref uncompressed-image row (- column 2) channel)
                      (the fixnum pixel-delta-maybe)))
             and do (incf try-start (1- key-length))
             and return nil
             finally (error
                      "Decoder out of step at row ~S, column ~S.  Giving up."
                      row column))
          finally
          (setf compressed-picture-index (1+ try-start))))
    (when reversep (reverse-displaced-vector uncompressed-image-vector))
    uncompressed-image))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun fetch-picture (stream start-position length height width channels
                      &key reversep)
  "Return the Bayer pattern taken from stream in an (array
\(unsigned-byte l8) (height width channels)), everything in color
channel 0.  Start at start-position or, if that is nil, at stream's
file position."
  (when start-position (file-position stream start-position))
  (let* ((image
          (make-array (list height width channels)
                      :element-type 'color))
         (image-vector
          (make-array (list (* height width channels))
                      :element-type 'color
                      :displaced-to image))
         (raw-image
          (make-array (list length) :element-type 'unsigned-byte)))
    (ecase channels
      (1
       (read-sequence image-vector stream))
      (3
       (error "Not implemented: ~
              fetch-picture for (uncompressed) truecolor images")
       ;; (read-sequence raw-image stream)
       ;; (loop
       ;;    for pixel across raw-image and red from 0 by 3 do
       ;;      (setf (svref png-image-data red) pixel))
       ))
    (when reversep (reverse-displaced-vector image-vector))
    image))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun reverse-displaced-vector (vector)
  "Reverse elements of vector of unsigned-byte in-place."
  (loop
     for cell across (reverse vector)
     for i from 0
     do (setf (aref vector i) cell)))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun complete-horizontally (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           #-phoros-uses-cl-png(type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (the color (aref image row (1- column) color))
                  (the color (aref image row (1+ column) color)))
               2)))
  
#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun complete-vertically (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           #-phoros-uses-cl-png(type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (the color (aref image (1- row) column color))
                  (the color (aref image (1+ row) column color)))
               2)))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun complete-squarely (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           #-phoros-uses-cl-png(type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (the color (aref image row (1- column) color))
                  (the color (aref image row (1+ column) color))
                  (the color (aref image (1- row) column color))
                  (the color (aref image (1+ row) column color)))
               4)))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun complete-diagonally (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           #-phoros-uses-cl-png(type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (the color (aref image (1- row) (1- column) color))
                  (the color (aref image (1- row) (1+ column) color))
                  (the color (aref image (1+ row) (1- column) color))
                  (the color (aref image (1+ row) (1+ column) color)))
               4)))

#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun height (image) (array-dimension image 0))
#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun width (image) (array-dimension image 1))
#+(or phoros-uses-cl-png phoros-uses-zpng)
(defun channels (image) (array-dimension image 2))

#+phoros-uses-zpng
(defun demosaic-image (image bayer-pattern color-raiser brightenp)
  "Demosaic color image whose color channel 0 is supposed to be
filled with a Bayer color pattern.  Return demosaiced image.
bayer-pattern is an array of 24-bit RGB values (red occupying the
least significant byte), describing the upper left corner of the
image.  Currently, only pixels 0, 1 on row 0 are taken into account.
And, it's currently not even an array but a vector due to limitations
in postmodern.  For a grayscale image do nothing.  Then, if brightenp
is t and the image is too dark, make it brighter.
We are using zpng."
  (declare (optimize (safety 0))
           (optimize speed)
           (type image image))
  (when (= 3 (channels image))
    (let ((lowest-row (- (height image) 2))
          (rightmost-column (- (width image) 2))
          (bayer-pattern-red #x0000ff)
          (bayer-pattern-green #x00ff00)
          (bayer-pattern-blue #xff0000)
          (red 0) (green 1) (blue 2)  ;color coordinate in IMAGE array
          (color-raiser-red (coerce (elt color-raiser 0)
                                    '(single-float -10.0s0 10.0s0)))
          (color-raiser-green (coerce (elt color-raiser 1)
                                      '(single-float -10.0s0 10.0s0)))
          (color-raiser-blue (coerce (elt color-raiser 2)
                                     '(single-float -10.0s0 10.0s0)))
          (pix-depth 255)     ;may some day become a function argument
          complete-even-row-even-column
          complete-even-row-odd-column
          complete-odd-row-even-column
          complete-odd-row-odd-column
          colorize-even-row-even-column
          colorize-even-row-odd-column
          colorize-odd-row-even-column
          colorize-odd-row-odd-column)
      (declare (type image-dimension lowest-row rightmost-column)
               )
      (flet ((complete-green-on-red-row (row column)
               (complete-horizontally image row column red)
               (complete-vertically image row column blue))
             (complete-green-on-blue-row (row column)
               (complete-horizontally image row column blue)
               (complete-vertically image row column red))
             (complete-red (row column)
               (complete-squarely image row column green)
               (complete-diagonally image row column blue))
             (complete-blue (row column)
               (complete-squarely image row column green)
               (complete-diagonally image row column red))
             (colorize-red (row column)
               (setf (aref image row column red)
                     (min pix-depth
                          (round (* color-raiser-red
                                    (aref image
                                          row column red))))))
             (colorize-green (row column)
               (setf (aref image row column green)
                     (min pix-depth
                          (round (* color-raiser-green
                                    (aref image
                                          row column red))))))
             (colorize-blue (row column)
               (setf (aref image row column blue)
                     (min pix-depth
                          (round (* color-raiser-blue
                                    (aref image
                                          row column red)))))))
        (cond
          ((= (aref bayer-pattern 0) bayer-pattern-red)
           (setf colorize-even-row-even-column #'colorize-red)
           (setf colorize-even-row-odd-column #'colorize-green)
           (setf colorize-odd-row-even-column #'colorize-green)
           (setf colorize-odd-row-odd-column #'colorize-blue)
           (setf complete-even-row-even-column #'complete-red)
           (setf complete-even-row-odd-column #'complete-green-on-red-row)
           (setf complete-odd-row-even-column #'complete-green-on-blue-row)
           (setf complete-odd-row-odd-column #'complete-blue))
          ((= (aref bayer-pattern 0) bayer-pattern-blue)
           (setf colorize-even-row-even-column #'colorize-blue)
           (setf colorize-even-row-odd-column #'colorize-green)
           (setf colorize-odd-row-even-column #'colorize-green)
           (setf colorize-odd-row-odd-column #'colorize-red)
           (setf complete-even-row-even-column #'complete-blue)
           (setf complete-even-row-odd-column #'complete-green-on-blue-row)
           (setf complete-odd-row-even-column #'complete-green-on-red-row)
           (setf complete-odd-row-odd-column #'complete-red))
          ((= (aref bayer-pattern 0) bayer-pattern-green)
           (cond
             ((= (aref bayer-pattern 1) bayer-pattern-red)
              (setf colorize-even-row-even-column #'colorize-green)
              (setf colorize-even-row-odd-column #'colorize-red)
              (setf colorize-odd-row-even-column #'colorize-blue)
              (setf colorize-odd-row-odd-column #'colorize-green)
              (setf complete-even-row-even-column #'complete-green-on-red-row)
              (setf complete-even-row-odd-column #'complete-red)
              (setf complete-odd-row-even-column #'complete-blue)
              (setf complete-odd-row-odd-column #'complete-green-on-blue-row))
             ((= (aref bayer-pattern 1) bayer-pattern-blue)
              (setf colorize-even-row-even-column #'colorize-green)
              (setf colorize-even-row-odd-column #'colorize-blue)
              (setf colorize-odd-row-even-column #'colorize-red)
              (setf colorize-odd-row-odd-column #'colorize-green)
              (setf complete-even-row-even-column #'complete-green-on-blue-row)
              (setf complete-even-row-odd-column #'complete-blue)
              (setf complete-odd-row-even-column #'complete-red)
              (setf complete-odd-row-odd-column #'complete-green-on-red-row))
             (t (error "Don't know how to deal with a bayer-pattern of ~A"
                       bayer-pattern))))
          (t (error "Don't know how to deal with a bayer-pattern of ~A"
                    bayer-pattern)))
        ;; Recover colors (so far everything is in channel 0)
        (loop
           for row from 0 below (the image-dimension (height image)) by 2
           do (loop
                 for column
                 from 0 below (the image-dimension (width image)) by 2
                 do (funcall colorize-even-row-even-column row column))
           (loop
              for column
              from 1 below (the image-dimension (width image)) by 2
              do (funcall colorize-even-row-odd-column row column)))
        (loop
           for row from 1 below (the image-dimension (height image)) by 2
           do (loop
                 for column
                 from 0 below (the image-dimension (width image)) by 2
                 do (funcall colorize-odd-row-even-column row column))
           (loop
              for column
              from 1 below (the image-dimension (width image)) by 2
              do (funcall colorize-odd-row-odd-column row column)))             
        ;; Demosaic
        (loop
           for row from 2 to lowest-row by 2 do
           (loop
              for column from 2 to rightmost-column by 2 do
              (funcall complete-even-row-even-column row column))
           (loop
              for column from 1 to rightmost-column by 2 do
              (funcall complete-even-row-odd-column row column)))
        (loop
           for row from 1 to lowest-row by 2 do
           (loop
              for column from 2 to rightmost-column by 2 do
              (funcall complete-odd-row-even-column row column))
           (loop
              for column from 1 to rightmost-column by 2 do
              (funcall complete-odd-row-odd-column row column))))))
  (when brightenp (brighten-maybe image))
  image)

#+phoros-uses-cl-png
(defun demosaic-image (image bayer-pattern color-raiser brightenp)
  "Demosaic color image whose color channel 0 is supposed to be
filled with a Bayer color pattern.  Return demosaiced image.
bayer-pattern is an array of 24-bit RGB values (red occupying the
least significant byte), describing the upper left corner of the
image.  Currently, only pixels 0, 1 on row 0 are taken into account.
And, it's currently not even an array but a vector due to limitations
in postmodern.  For a grayscale image do nothing.  Then, if brightenp
is t and the image is too dark, make it brighter.
We are using cl-png."
  (declare (optimize speed)
           (optimize (safety 0))
           (optimize speed))
  (when (= 3 (png:image-channels image))
    (let ((lowest-row (- (png:image-height image) 2))
          (rightmost-column (- (png:image-width image) 2))
          (bayer-pattern-red #x0000ff)
          (bayer-pattern-green #x00ff00)
          (bayer-pattern-blue #xff0000)
          (red 0) (green 1) (blue 2)    ;color coordinate in IMAGE array
          (color-raiser-red (coerce (elt color-raiser 0) '(single-float -10.0s0 10.0s0)))
          (color-raiser-green (coerce (elt color-raiser 1) '(single-float -10.0s0 10.0s0)))
          (color-raiser-blue (coerce (elt color-raiser 2) '(single-float -10.0s0 10.0s0)))
          (pix-depth 255)     ;may some day become a function argument
          complete-even-row-even-column
          complete-even-row-odd-column
          complete-odd-row-even-column
          complete-odd-row-odd-column
          colorize-even-row-even-column
          colorize-even-row-odd-column
          colorize-odd-row-even-column
          colorize-odd-row-odd-column)
      (declare (type image-dimension lowest-row rightmost-column))
      (flet ((complete-green-on-red-row (row column)
               (complete-horizontally image row column red)
               (complete-vertically image row column blue))
             (complete-green-on-blue-row (row column)
               (complete-horizontally image row column blue)
               (complete-vertically image row column red))
             (complete-red (row column)
               (complete-squarely image row column green)
               (complete-diagonally image row column blue))
             (complete-blue (row column)
               (complete-squarely image row column green)
               (complete-diagonally image row column red))
             (colorize-red (row column)
               (setf (aref image row column red)
                     (min pix-depth
                          (round (* color-raiser-red
                                    (the color (aref image
                                                      row column red)))))))
             (colorize-green (row column)
               (setf (aref image row column green)
                     (min pix-depth
                          (round (* color-raiser-green
                                    (the color (aref image
                                                      row column red)))))))
             (colorize-blue (row column)
               (setf (aref image row column blue)
                     (min pix-depth
                          (round (* color-raiser-blue
                                    (the color (aref image
                                                      row column red))))))))
        (cond
          ((= (aref bayer-pattern 0) bayer-pattern-red)
           (setf colorize-even-row-even-column #'colorize-red)
           (setf colorize-even-row-odd-column #'colorize-green)
           (setf colorize-odd-row-even-column #'colorize-green)
           (setf colorize-odd-row-odd-column #'colorize-blue)
           (setf complete-even-row-even-column #'complete-red)
           (setf complete-even-row-odd-column #'complete-green-on-red-row)
           (setf complete-odd-row-even-column #'complete-green-on-blue-row)
           (setf complete-odd-row-odd-column #'complete-blue))
          ((= (aref bayer-pattern 0) bayer-pattern-blue)
           (setf colorize-even-row-even-column #'colorize-blue)
           (setf colorize-even-row-odd-column #'colorize-green)
           (setf colorize-odd-row-even-column #'colorize-green)
           (setf colorize-odd-row-odd-column #'colorize-red)
           (setf complete-even-row-even-column #'complete-blue)
           (setf complete-even-row-odd-column #'complete-green-on-blue-row)
           (setf complete-odd-row-even-column #'complete-green-on-red-row)
           (setf complete-odd-row-odd-column #'complete-red))
          ((= (aref bayer-pattern 0) bayer-pattern-green)
           (cond
             ((=(aref bayer-pattern 1) bayer-pattern-red)
              (setf colorize-even-row-even-column #'colorize-green)
              (setf colorize-even-row-odd-column #'colorize-red)
              (setf colorize-odd-row-even-column #'colorize-blue)
              (setf colorize-odd-row-odd-column #'colorize-green)
              (setf complete-even-row-even-column #'complete-green-on-red-row)
              (setf complete-even-row-odd-column #'complete-red)
              (setf complete-odd-row-even-column #'complete-blue)
              (setf complete-odd-row-odd-column #'complete-green-on-blue-row))
             ((=(aref bayer-pattern 1) bayer-pattern-blue)
              (setf colorize-even-row-even-column #'colorize-green)
              (setf colorize-even-row-odd-column #'colorize-blue)
              (setf colorize-odd-row-even-column #'colorize-red)
              (setf colorize-odd-row-odd-column #'colorize-green)
              (setf complete-even-row-even-column #'complete-green-on-blue-row)
              (setf complete-even-row-odd-column #'complete-blue)
              (setf complete-odd-row-even-column #'complete-red)
              (setf complete-odd-row-odd-column #'complete-green-on-red-row))
             (t (error "Don't know how to deal with a bayer-pattern of ~A"
                       bayer-pattern))))
          (t (error "Don't know how to deal with a bayer-pattern of ~A"
                    bayer-pattern)))
        ;; Recover colors (so far everything is in channel 0)
        (loop for row from 0 below (the image-dimension (height image)) by 2
           do (loop for column from 0 below (the image-dimension (width image)) by 2
                 do (funcall colorize-even-row-even-column row column))
           (loop for column from 1 below (the image-dimension (width image)) by 2
              do (funcall colorize-even-row-odd-column row column)))
        (loop for row from 1 below (the image-dimension (height image)) by 2
           do (loop for column from 0 below (the image-dimension (width image)) by 2
                 do (funcall colorize-odd-row-even-column row column))
             (loop for column from 1 below (the image-dimension (width image)) by 2
                do (funcall colorize-odd-row-odd-column row column)))             
        ;; Demosaic
        (loop
           for row from 2 to lowest-row by 2 do
           (loop
              for column from 2 to rightmost-column by 2 do
              (funcall complete-even-row-even-column row column))
           (loop
              for column from 1 to rightmost-column by 2 do
              (funcall complete-even-row-odd-column row column)))
        (loop
           for row from 1 to lowest-row by 2 do
           (loop
              for column from 2 to rightmost-column by 2 do
              (funcall complete-odd-row-even-column row column))
           (loop
              for column from 1 to rightmost-column by 2 do
              (funcall complete-odd-row-odd-column row column))))))
  (when brightenp (brighten-maybe image))
  image)

#+phoros-uses-zpng
(defun brighten-maybe (image)
  "Make image brighter if it is too dark.
We are using zpng."
  (declare (optimize speed)
           (optimize (safety 0))
           (type image image))
  (multiple-value-bind (brightest-value darkest-value)
      (brightness image)
    (declare (type color brightest-value darkest-value))
    (when (< (the color brightest-value) 200)
      (dotimes (y (the image-dimension (height image)))
        (dotimes (x (the image-dimension (width image)))
          (dotimes (c (the channels (channels image)))
            (setf (aref image y x c)
                  (floor (* (the color (- (aref image y x c)
                                          darkest-value))
                            255)
                         (- brightest-value darkest-value)))))))))

#+phoros-uses-cl-png
(defun brighten-maybe (image)
  "Make image brighter if it is too dark.
We are using cl-png."
  (declare (optimize speed)
           (optimize (safety 0)))
  (multiple-value-bind (brightest-value darkest-value)
      (brightness image)
    (declare (type color brightest-value darkest-value))
    (when (< (the color brightest-value) 200)
      (let ((image-vector (make-array (list (* (height image)
                                               (width image)
                                               (channels image)))
                                      :element-type 'color
                                      :displaced-to image)))
        (loop
           for i from 0 below (length image-vector)
           do (setf (aref image-vector i)
                    (floor (* (the color (- (aref image-vector i)
                                            darkest-value))
                              255)
                           (- brightest-value darkest-value))))))))

#+phoros-uses-zpng
(defun brightness (image)
  "Return brightest value and darkest value of image.
We are using zpng."
  (declare (optimize speed)
           (optimize (safety 0))
           (type image image))
  (let ((brightest-value 0)
        (darkest-value 0))
    (declare (type color brightest-value darkest-value))
    (dotimes (y (the image-dimension (height image)))
      (dotimes (x (the image-dimension (width image)))
        (dotimes (c (the channels (channels image)))
          (let ((intensity (aref image y x c)))
            (setf brightest-value (max intensity brightest-value))
            (setf darkest-value (min intensity darkest-value))))))
    (values brightest-value darkest-value)))
        
#+phoros-uses-cl-png
(defun brightness (image)
  "Return brightest value and darkest value of image.  We are using
cl-png."
  (declare (optimize speed))
  (let ((image-vector
         (make-array (list (* (height image) (width image) (channels image)))
                     :element-type 'color
                     :displaced-to image)))
    (loop
       for brightness across image-vector
       maximize brightness into brightest-value
       minimize brightness into darkest-value
       finally (return (values brightest-value
                               darkest-value)))))


#+(or phoros-uses-cl-png phoros-uses-zpng)
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
        (huffman-table-size (* 511 (+ 1 4)))
        (image-height (find-keyword-value path "height=" start))
        (image-width (find-keyword-value path "width=" start))
        (compression-mode (find-keyword-value path "compressed=" start))
        (channels (find-keyword-value path "channels=" start))
        (trigger-time (find-keyword-value path "timeTrigger=" start)))
    (assert (member channels '(1 3)) ()
            "Don't know how to deal with ~D-channel pixels." channels)
    (with-open-file (input-stream path :element-type 'unsigned-byte)
      (let ((image
             (demosaic-image
              (ecase compression-mode
                ((2 1) ;compressed with individual/pre-built huffman table
                 (uncompress-picture (read-huffman-table input-stream
                                                         blob-start)
                                     (read-compressed-picture
                                      input-stream
                                      (+ blob-start huffman-table-size)
                                      (- blob-size huffman-table-size))
                                     image-height image-width channels
                                     :reversep reversep))
                (0                      ;uncompressed
                 (fetch-picture input-stream blob-start blob-size
                                image-height image-width channels
                                :reversep reversep)))
              bayer-pattern
              color-raiser
              brightenp)))
        (write-image image output-stream)))
    trigger-time))

#+phoros-uses-imread.so
(cffi:defcstruct mem-encode
    (buffer :pointer)
  (size :int))

#+phoros-uses-imread.so
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
        (huffman-table-size (* 511 (+ 1 4)))
        (image-height (find-keyword-value path "height=" start))
        (image-width (find-keyword-value path "width=" start))
        (compression-mode (find-keyword-value path "compressed=" start))
        (channels (find-keyword-value path "channels=" start))
        (trigger-time (find-keyword-value path "timeTrigger=" start)))
    (cffi:with-foreign-objects ((baypat :int 3)
                                (colr-raisr :double 3)
                                (mem-png 'mem-encode)
                                (compressed
                                 :unsigned-char (- blob-size huffman-table-size))
                                (uncompressed
                                 :unsigned-char (* image-width image-height)))
      (loop
         for i from 0 below (min 4 (first (array-dimensions bayer-pattern))) do
           (setf (cffi:mem-aref baypat :int i) (aref bayer-pattern i)))
      (loop
         for i from 0 to 2 do
           (setf (cffi:mem-aref colr-raisr :double i)
                 (coerce (aref color-raiser i) 'double-float)))
      (let ((png2mem-exit
             (imread:png2mem (namestring path) blob-start (- blob-size huffman-table-size)
                             image-width image-height channels baypat compression-mode
                             uncompressed compressed mem-png
                             (if reversep 1 0) (if brightenp 1 0) colr-raisr)))
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
              (t
               (error "Can't unpack image.")))))
    trigger-time))

#+phoros-uses-zpng
(defun write-image (image stream)
  "Write image array (height, width, channel) to stream."
  (zpng:write-png-stream
   (zpng:copy-png
    (make-instance 'zpng:png
                   :height (height image)
                   :width (width image)
                   :color-type (getf '(1 :grayscale 3 :truecolor)
                                     (channels image))
                   :image-data (make-array
                                (list (* (height image) (width image)
                                         (channels image)))
                                :element-type 'color
                                :displaced-to image)))
   stream))

#+phoros-uses-cl-png
(defun write-image (image stream)
  "Write image array (height, width, channel) to stream."
  (png:encode image stream))

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
