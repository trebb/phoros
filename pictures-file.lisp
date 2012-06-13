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


(in-package :img)

(defparameter *picture-header-length-tolerance* 20
  "Amount of leeway for the length of a picture header in a .pictures
  file.")

(defun find-keyword-in-stream (stream keyword &optional start-position search-range)
  "Return file-position in binary stream after first occurence of
keyword."
  (unless start-position (setf start-position 0))
  (let ((end-position (if search-range
                          (+ start-position search-range)
                          most-positive-fixnum)))
    (handler-case
        (progn
          (file-position stream start-position)
          (let ((chunk-size (length keyword)))
            (cl:loop
             for next-chunk = (let ((result (make-array
                                             (list chunk-size)
                                             :element-type 'unsigned-byte)))
                                (read-sequence result stream)
                                (coerce (map 'vector #'code-char result)
                                        'string))
             if (string/= next-chunk keyword) do
             (let ((next-position (- (file-position stream) chunk-size -1)))
               (if (< next-position end-position)
                   (file-position stream next-position)
                   (return-from find-keyword-in-stream)))
             else return (file-position stream))))
      (end-of-file () nil))))

(defun find-keyword-value (path keyword &optional start-position search-range)
  "Return value associated with keyword."
  (let ((start-of-value
         (find-keyword path keyword start-position search-range)))
    (when start-of-value
      (with-open-file (stream path)
        (file-position stream start-of-value)
        (car (read-delimited-list #\; stream))))))

(defun find-keyword (path keyword &optional start-position search-range)
  "Return file-position after keyword."
  (with-open-file (stream path :element-type 'unsigned-byte)
    (find-keyword-in-stream stream keyword start-position search-range)))

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

(defun get-leading-byte (bit-array &optional (start 0) &aux (result 0))
  "Return integer made of eight bits from bit-array."
  (loop
     for bit-array-index from start
     for result-index from 7 downto 0
     for result = (dpb (sbit bit-array bit-array-index)
                       (byte 1 result-index) 0)
     then (dpb (sbit bit-array bit-array-index) (byte 1 result-index) result)
     finally (return result)))

(defun uncompress-picture (huffman-table compressed-picture
                           height width channels &key reversep)
  "Return the Bayer pattern extracted from compressed-picture, turned
upside-down if reversep is t, in an (array (unsigned-byte 8) (height
width channels)), everything in channel 0."
  (declare (optimize (safety 0))
           (type (unsigned-byte 16) height width)
           (type vector compressed-picture))
  (let* ((uncompressed-image
          (make-array (list height width channels)
                      :element-type '(unsigned-byte 8)))
         (uncompressed-image-vector
          (make-array (list (* height width channels))
                      :element-type '(unsigned-byte 8)
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
             (type (unsigned-byte 8) channels))
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
    (when reversep (setf uncompressed-image-vector (reverse uncompressed-image-vector)))
    uncompressed-image))

(defun fetch-picture (stream start-position length height width channels
                      &key reversep)
  "Return the Bayer pattern taken from stream in an (array (unsigned-byte l8) (height width channels)),
everything in color channel 0.  Start at start-position or, if that is
nil, at stream's file position."
  (when start-position (file-position stream start-position))
  (let* ((image
          (make-array (list height width channels)
                      :element-type '(unsigned-byte 8)))
         (image-vector
          (make-array (list (* height width channels))
                      :element-type '(unsigned-byte 8)
                      :displaced-to image))
         (raw-image
          (make-array (list length) :element-type 'unsigned-byte)))
    (ecase channels
      (1
       (read-sequence image-vector stream))
      (3
       (error "Not implemented: fetch-picture for (uncompressed) truecolor images")
       ;; (read-sequence raw-image stream)
       ;; (loop
       ;;    for pixel across raw-image and red from 0 by 3 do
       ;;      (setf (svref png-image-data red) pixel))
       ))
    (when reversep (setf image-vector (reverse image-vector)))
    image))

(defun complete-horizontally (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           (type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (aref image row (1- column) color)
                  (aref image row (1+ column) color))
               2)))
  
(defun complete-vertically (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           (type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (aref image (1- row) column color)
                  (aref image (1+ row) column color))
               2)))

(defun complete-squarely (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           (type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (aref image row (1- column) color)
                  (aref image row (1+ column) color)
                  (aref image (1- row) column color)
                  (aref image (1+ row) column color))
               4)))

(defun complete-diagonally (image row column color)
  "Fake a color component of a pixel based its neighbors."
  (declare (optimize (safety 0))
           (optimize speed)
           (type image image)
           (type image-dimension row column))
  (setf (aref image row column color)
        (round (+ (aref image (1- row) (1- column) color)
                  (aref image (1- row) (1+ column) color)
                  (aref image (1+ row) (1- column) color)
                  (aref image (1+ row) (1+ column) color))
               4)))

(deftype image-dimension () '(unsigned-byte 16))
(deftype image () '(simple-array (unsigned-byte 8) 3))

(defun height (image) (array-dimension image 0))
(defun width (image) (array-dimension image 1))
(defun channels (image) (array-dimension image 2))

(defun demosaic-png (png bayer-pattern color-raiser brightenp) ;TODO: s/png/image
  "Demosaic color png whose color channel 0 is supposed to be
filled with a Bayer color pattern.  Return demosaiced png.
bayer-pattern is an array of 24-bit RGB values (red occupying the
least significant byte), describing the upper left corner of the
image.  Currently, only pixels 0, 1 on row 0 are taken into account.
And, it's currently not even an array but a vector due to limitations
in postmodern.  For a grayscale image do nothing.  Then, if brightenp
is t and the image is too dark, make it brighter."
  (declare (optimize (safety 0))
           (optimize speed)
           (type image png))
  (when (= 3 (channels png))
    (let ((lowest-row (- (height png) 2))
          (rightmost-column (- (width png) 2))
          (bayer-pattern-red #x0000ff)
          (bayer-pattern-green #x00ff00)
          (bayer-pattern-blue #xff0000)
          (red 0) (green 1) (blue 2)    ;color coordinate in PNG array
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
      (declare (type image-dimension lowest-row rightmost-column)
               )
      (flet ((complete-green-on-red-row (row column)
               (complete-horizontally png row column red)
               (complete-vertically png row column blue))
             (complete-green-on-blue-row (row column)
               (complete-horizontally png row column blue)
               (complete-vertically png row column red))
             (complete-red (row column)
               (complete-squarely png row column green)
               (complete-diagonally png row column blue))
             (complete-blue (row column)
               (complete-squarely png row column green)
               (complete-diagonally png row column red))
             (colorize-red (row column)
               (setf (aref png row column red)
                     (min pix-depth
                          (round (* color-raiser-red
                                    (aref png
                                          row column red))))))
             (colorize-green (row column)
               (setf (aref png row column green)
                     (min pix-depth
                          (round (* color-raiser-green
                                    (aref png
                                          row column red))))))
             (colorize-blue (row column)
               (setf (aref png row column blue)
                     (min pix-depth
                          (round (* color-raiser-blue
                                    (aref png
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
        (loop for row from 0 below (the image-dimension (height png)) by 2
           do (loop for column from 0 below (the image-dimension (width png)) by 2
                 do (funcall colorize-even-row-even-column row column))
           (loop for column from 1 below (the image-dimension (width png)) by 2
              do (funcall colorize-even-row-odd-column row column)))
        (loop for row from 1 below (the image-dimension (height png)) by 2
           do (loop for column from 0 below (the image-dimension (width png)) by 2
                 do (funcall colorize-odd-row-even-column row column))
             (loop for column from 1 below (the image-dimension (width png)) by 2
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
  (when brightenp (brighten-maybe png))
  png)

(defun brighten-maybe (png)             ;TODO s/png/image-or-something/
  "Make png brighter if it is too dark."
  (multiple-value-bind (brightest-value darkest-value)
      (brightness png)
    (when (< brightest-value 200)
      (let ((image (make-array (list (* (height png) (width png) (channels png))) :element-type '(unsigned-byte 8) :displaced-to png)))
        (loop
           for i from 0 below (length image)
           do (setf (aref image i)
                    (floor (* (- (aref image i) darkest-value)
                              (/ 255 (- brightest-value darkest-value))))))))))

(defun brightness (png)
  "Return brightest value and darkest value of png." ;TODO: s/png/image/
  (let ((image (make-array (list (* (height png) (width png) (channels png)))
                           :element-type '(unsigned-byte 8)
                           :displaced-to png)))
    (loop
       for brightness across image
       maximize brightness into brightest-value
       minimize brightness into darkest-value
       finally (return (values brightest-value
                               darkest-value)))))

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
      (let* ((image (demosaic-png
                     (ecase compression-mode
                       ((2 1) ;compressed with individual/pre-built huffman table
                        (uncompress-picture (read-huffman-table input-stream blob-start)
                                            (read-compressed-picture
                                             input-stream
                                             (+ blob-start huffman-table-size)
                                             (- blob-size huffman-table-size))
                                            image-height image-width channels
                                            :reversep reversep))
                       (0               ;uncompressed
                        (fetch-picture input-stream blob-start blob-size
                                       image-height image-width channels
                                       :reversep reversep)))
                     bayer-pattern
                     color-raiser
                     brightenp)))
        (zpng:write-png-stream            ;TODO: generalize
         (zpng:copy-png
          (make-instance 'zpng:png
                         :height (height image)
                         :width (width image)
                         :color-type (getf '(1 :grayscale 3 :truecolor)
                                           (channels image))
                         :image-data (make-array
                                      (list (* (height image) (width image)
                                               (channels image)))
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image)))
         output-stream)))
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
                     &mandatory-key bayer-pattern)
  "Read image number n (zero-indexed) in .pictures file at path and
send it to the binary output-stream.  Return UNIX trigger-time of
image."
  (send-png output-stream path (find-nth-picture n path)
            :bayer-pattern bayer-pattern :color-raiser color-raiser))


;; TODO: (perhaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect 4 single color pixels into a three-color one
;; enhance contrast of grayscale images
