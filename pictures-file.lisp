(in-package :phoros)

(defparameter *picture-header-length-tolerance* 20
  "Amount of leeway for the length of a picture header in a .pictures file.")

(defun find-keyword-in-stream (stream keyword &optional (start-position 0))
  "Return file-position in binary stream after first occurence of keyword."
  (handler-case
      (progn
        (file-position stream start-position)
        (let ((chunk-size (length keyword)))
          (cl:loop
           for next-chunk = (let ((result (make-array (list chunk-size) :fill-pointer 0)))
                              (dotimes (i chunk-size (coerce result 'string))
                                (vector-push-extend (code-char (read-byte stream)) result))) ; TODO: try read-sequence
           if (string/= next-chunk keyword) 
           do (file-position stream (- (file-position stream) chunk-size -1))
           else return (file-position stream))))
    (end-of-file () nil)))

(defun find-keyword-value (path keyword &optional (start-position 0))
  "Return value associated with keyword."
  (let ((start-of-value
         (find-keyword path keyword start-position)))
    (with-open-file (stream path)
      (file-position stream start-of-value)
      (car (read-delimited-list #\; stream)))))

(defun find-keyword (path keyword &optional (start-position 0))
  "Return file-position after keyword."
  (with-open-file (stream path :element-type 'unsigned-byte)
    (find-keyword-in-stream stream keyword start-position)))

(defun read-huffman-table (stream &optional start-position)
  "Return in a hash table a huffman table read from stream.  Start either at stream's file position or at start-position."
  (let* ((huffman-codes-start
          (if start-position
              start-position
              (file-position stream))))
    (file-position stream (+ (* 511 4) huffman-codes-start)) ; start of lengths
    (let* ((lengths (loop
                       repeat 511
                       for length = (read-byte stream) ; TODO: try read-sequence
                       collect length))
           (huffman-table (make-hash-table :size 1000 :test #'equal)))
      (file-position stream huffman-codes-start)
      (loop
         for i from -255 to 255
         for length in lengths
         for key = (make-array (list length) :element-type 'bit)
         for code = (let ((code-part 0))
                      (setf code-part (dpb (read-byte stream) (byte 8 24) code-part))
                      (setf code-part (dpb (read-byte stream) (byte 8 16) code-part))
                      (setf code-part (dpb (read-byte stream) (byte 8 8) code-part))
                      (dpb (read-byte stream) (byte 8 0) code-part)) ; TODO: try read-sequence
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
  "Return a compressed picture in a bit array.  Start either at start-position or, if that is nil, at stream's file position."
  (when start-position (file-position stream start-position))
  (let ((compressed-picture (make-array (list (* 8 length)) :element-type 'bit)))
    (loop
       for byte-position from 0 below length
       for byte = (read-byte stream) ; TODO: try read-sequence
       do (loop
             for source-bit from 7 downto 0
             for destination-bit from 0 to 7
             do (setf (sbit compressed-picture (+ destination-bit (* 8 byte-position)))
                      (ldb (byte 1 source-bit) byte))))
    compressed-picture))

(defun get-leading-byte (bit-array &optional (start 0) &aux (result 0))
  "Return integer made of eight bits from bit-array."
  (loop
     for bit-array-index from start
     for result-index from 7 downto 0
     for result = (dpb (sbit bit-array bit-array-index) (byte 1 result-index) 0)
     then (dpb (sbit bit-array bit-array-index) (byte 1 result-index) result)
     finally (return result)))

(defun uncompress-picture (huffman-table compressed-picture height width color-type)
  "Return the Bayer pattern extracted from compressed-picture in a zpng:png image, everything in color channel 0."
  (let* ((png (make-instance 'zpng:png
                             :color-type color-type
                             :width width :height height))
         (uncompressed-picture (zpng:data-array png))
         (compressed-picture-index 0)
         (min-key-length (loop
                            for code being the hash-key in huffman-table
                            minimize (length code)))
         (max-key-length (loop
                            for code being the hash-key in huffman-table
                            maximize (length code))))
    (loop
       for row from 0 below height
       do
         (setf (aref uncompressed-picture row 0 0)
               (get-leading-byte compressed-picture (prog1 compressed-picture-index (incf compressed-picture-index 8))))
         (setf (aref uncompressed-picture row 1 0)
               (get-leading-byte compressed-picture (prog1 compressed-picture-index (incf compressed-picture-index 8))))
         (loop
            for column from 2 below width
            for try-start from compressed-picture-index
            do
            (loop
               for key-length from min-key-length to max-key-length
               for huffman-code = (subseq compressed-picture try-start (+ try-start key-length))
               for pixel-delta-maybe = (gethash huffman-code huffman-table)
               when pixel-delta-maybe
               do
                 (setf (aref uncompressed-picture row column 0)
                       (- (aref uncompressed-picture row (- column 2) 0)
                          pixel-delta-maybe))
               and do (incf try-start (1- key-length))
               and return nil
               finally (error "Decoder out of step at row ~S, column ~S.  Giving up." row column))
            finally
            (setf compressed-picture-index (1+ try-start))
            ;;(print compressed-picture-index)
            ))
      png))

(defun complete-horizontally (png row column color)
  "Fake a color component of a pixel based its neighbors."
  (let ((data-array (zpng:data-array png)))
    (setf (aref data-array row column color)
          (round (+ (aref data-array row (1- column) color)
                    (aref data-array row (1+ column) color))
                 2))))
  
(defun complete-vertically (png row column color)
  "Fake a color component of a pixel based its neighbors."
  (let ((data-array (zpng:data-array png)))
    (setf (aref data-array row column color)
          (round (+ (aref data-array (1- row) column color)
                    (aref data-array (1+ row) column color))
                 2))))

(defun complete-squarely (png row column color)
  "Fake a color component of a pixel based its neighbors."
  (let ((data-array (zpng:data-array png)))
    (setf (aref data-array row column color)
          (round (+ (aref data-array row (1- column) color)
                    (aref data-array row (1+ column) color)
                    (aref data-array (1- row) column color)
                    (aref data-array (1+ row) column color))
                 4))))

(defun complete-diagonally (png row column color)
  "Fake a color component of a pixel based its neighbors."
  (let ((data-array (zpng:data-array png)))
    (setf (aref data-array row column color)
          (round (+ (aref data-array (1- row) (1- column) color)
                    (aref data-array (1- row) (1+ column) color)
                    (aref data-array (1+ row) (1- column) color)
                    (aref data-array (1+ row) (1+ column) color))
                 4))))

(defun demosaic-png (png bayer-pattern)
  "Demosaic color png in-place whose color channel 0 is supposed to be filled with a Bayer color pattern.  Return demosaiced png.
bayer-pattern is an array of 24-bit RGB values (red occupying the least significant byte), describing the upper left corner of the image.  Currently, only pixels 0, 1 on row 0 are taken into account.
For a grayscale image do nothing."
  (when (eq (zpng:color-type png) :truecolor)
    (let ((lowest-row (- (zpng:height png) 2))
          (rightmost-column (- (zpng:width png) 2))
          (bayer-pattern-red #x0000ff)
          (bayer-pattern-green #x00ff00)
          (bayer-pattern-blue #xff0000)
          (red 0) (green 1) (blue 2)    ;color coordinate in PNG array
          complete-even-row-even-column
          complete-even-row-odd-column
          complete-odd-row-even-column
          complete-odd-row-odd-column
          colorize-even-row-even-column
          colorize-even-row-odd-column
          colorize-odd-row-even-column
          colorize-odd-row-odd-column)
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
             (colorize-red (row column) (declare (ignore row column)))
             (colorize-green (row column)
               (setf (aref (zpng:data-array png) row column green)
                     (aref (zpng:data-array png) row column red)))
             (colorize-blue (row column)
               (setf (aref (zpng:data-array png) row column blue)
                     (aref (zpng:data-array png) row column red))))
        (cond
          ((= (aref bayer-pattern 0 0) bayer-pattern-red)
           (setf colorize-even-row-even-column #'colorize-red)
           (setf colorize-even-row-odd-column #'colorize-green)
           (setf colorize-odd-row-even-column #'colorize-green)
           (setf colorize-odd-row-odd-column #'colorize-blue)
           (setf complete-even-row-even-column #'complete-red)
           (setf complete-even-row-odd-column #'complete-green-on-red-row)
           (setf complete-odd-row-even-column #'complete-green-on-blue-row)
           (setf complete-odd-row-odd-column #'complete-blue))
          ((= (aref bayer-pattern 0 0) bayer-pattern-blue)
           (setf colorize-even-row-even-column #'colorize-blue)
           (setf colorize-even-row-odd-column #'colorize-green)
           (setf colorize-odd-row-even-column #'colorize-green)
           (setf colorize-odd-row-odd-column #'colorize-red)
           (setf complete-even-row-even-column #'complete-blue)
           (setf complete-even-row-odd-column #'complete-green-on-blue-row)
           (setf complete-odd-row-even-column #'complete-green-on-red-row)
           (setf complete-odd-row-odd-column #'complete-red))
          ((= (aref bayer-pattern 0 0) bayer-pattern-green)
           (cond
             ((=(aref bayer-pattern 0 1) bayer-pattern-red)
              (setf colorize-even-row-even-column #'colorize-green)
              (setf colorize-even-row-odd-column #'colorize-red)
              (setf colorize-odd-row-even-column #'colorize-blue)
              (setf colorize-odd-row-odd-column #'colorize-green)
              (setf complete-even-row-even-column #'complete-green-on-red-row)
              (setf complete-even-row-odd-column #'complete-red)
              (setf complete-odd-row-even-column #'complete-blue)
              (setf complete-odd-row-odd-column #'complete-green-on-blue-row))
             ((=(aref bayer-pattern 0 1) bayer-pattern-blue)
              (setf colorize-even-row-even-column #'colorize-green)
              (setf colorize-even-row-odd-column #'colorize-blue)
              (setf colorize-odd-row-even-column #'colorize-red)
              (setf colorize-odd-row-odd-column #'colorize-green)
              (setf complete-even-row-even-column #'complete-green-on-blue-row)
              (setf complete-even-row-odd-column #'complete-blue)
              (setf complete-odd-row-even-column #'complete-red)
              (setf complete-odd-row-odd-column #'complete-green-on-red-row))
             (t (error "Don't know how to deal with a bayer-pattern of ~A" bayer-pattern))))
          (t (error "Don't know how to deal with a bayer-pattern of ~A" bayer-pattern)))
        ;; Recover colors (so far everything is in channel 0)
        (loop for row from 0 below (zpng:height png) by 2
           do (loop for column from 0 below (zpng:width png) by 2
                 do (funcall colorize-even-row-even-column row column))
           (loop for column from 1 below (zpng:width png) by 2
              do (funcall colorize-even-row-odd-column row column)))
        (loop for row from 1 below (zpng:height png) by 2
           do (loop for column from 0 below (zpng:width png) by 2
                 do (funcall colorize-odd-row-even-column row column))
           (loop for column from 1 below (zpng:width png) by 2
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
  png)
                            
(defun send-png (output-stream path start &key (bayer-pattern (error "bayer-pattern needed.")))
  "Read an image at position start in .pictures file at path and send it to the binary output-stream."
  (let ((blob-start (find-keyword path "PICTUREDATA_BEGIN" start))
        (blob-size (find-keyword-value path "dataSize=" start))
        (huffman-table-size (* 511 (+ 1 4)))
        (image-height (find-keyword-value path "height=" start))
        (image-width (find-keyword-value path "width=" start))
        (compression-mode (find-keyword-value path "compressed=" start))
        (color-type (ecase (find-keyword-value path "channels=" start)
                      (1 :grayscale)
                      (3 :truecolor))))
    (assert (or (= 2 compression-mode) ; compressed with individual huffman table
                (= 1 compression-mode))) ; compressed with pre-built huffman table
    (with-open-file (input-stream path :element-type 'unsigned-byte)
      (zpng:write-png-stream
       (demosaic-png
        (uncompress-picture (read-huffman-table input-stream blob-start)
                            (read-compressed-picture input-stream
                                                     (+ blob-start huffman-table-size)
                                                     (- blob-size huffman-table-size))
                            image-height image-width color-type)
        bayer-pattern)
       output-stream))))

;;(time (with-open-file (s "png.png" :element-type 'unsigned-byte :direction :output :if-exists :supersede) 
;;                (send-png s "singlepic" 0)))

(defun find-nth-picture (n path)
  "Find file-position of zero-indexed nth picture in in .pictures file at path."
  (let ((estimated-header-length
         (- (find-keyword path "PICTUREHEADER_END")
            (find-keyword path "PICTUREHEADER_BEGIN")
            *picture-header-length-tolerance*))) ; allow for variation in dataSize and a few other parameters
    (loop
       for i from 0 to n
       for picture-start =
       (find-keyword path "PICTUREHEADER_BEGIN" 0) then
       (find-keyword path "PICTUREHEADER_BEGIN" (+ picture-start picture-length estimated-header-length))
       for picture-length = (find-keyword-value path "dataSize=" picture-start)
       finally (return (- picture-start (length "PICTUREHEADER_BEGIN"))))))

(defun send-nth-png (n output-stream path &key (bayer-pattern (error "bayer-pattern needed.")))
  "Read image number n (zero-indexed) in .pictures file at path and send it to the binary output-stream."
  (send-png output-stream path (find-nth-picture n path) :bayer-pattern bayer-pattern))

;;(defstruct picture-header               ; TODO: perhaps not needed
;;  "Information for one image from a .pictures file."
;;  trigger-time fake-trigger-time-p camera-timestamp recorded-device-id path file-position)


;; TODO: (perhaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; raise red values by .2
;; allow for alternative bayer patterns
;; collect 4 single color pixels into a three-color one
;; enhance contrast of grayscale images
