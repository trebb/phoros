(in-package :phoros)

;(with-open-file (s "/home/bertb/phoros-testdata/mnt/data0/Rohdaten/mittelsachsen_0002/einzelbilder/mitsa005_CCD_Front_PULNIX_10.pictures" :element-type 'unsigned-byte) (print (find-keyword s "PICTUREDATA_BEGIN")) (print (find-keyword s "PICTUREDATA_BEGIN")) (print (find-keyword s "PICTUREDATA_BEGIN")))


(defun find-keyword-in-stream (stream keyword &optional (start-position 0))
  "Return file-position in binary stream after first occurence of keyword."
  (file-position stream start-position)
  (let ((chunk-size (length keyword)))
    (cl:loop
     for next-chunk = (let ((result (make-array '(0) :fill-pointer t)))
                        (dotimes (i chunk-size (coerce result 'string))
                          (vector-push-extend (code-char (read-byte stream)) result)))
     if (string/= next-chunk keyword) 
     do (file-position stream (- (file-position stream) chunk-size -1))
     else return (file-position stream))))

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
                       for length = (read-byte stream)
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
                      (dpb (read-byte stream) (byte 8 0) code-part))
         unless (zerop length)
         do (loop
               for key-index from 0 below length
               for code-index downfrom (1- length)
               do (setf (sbit key key-index)
                        (ldb (byte 1 code-index) code)))
         and
         do (setf (gethash key huffman-table) i))
      huffman-table)))

(defun read-compressed-picture (stream length padding-length &optional start-position)
  "Return a compressed picture in a bit array.  Start either at stream's file position or at start-position."
  (let ((picture-start (if start-position
                           start-position
                           (file-position stream)))
        (compressed-picture (make-array (list (+ (* 8 length) padding-length)) :element-type 'bit)))
    (when start-position (file-position stream start-position))
    (loop
       for byte-position from 0 below length
       for byte = (read-byte stream)
       do (loop
             for destination-bit from 0 to 7
             for source-bit from 7 downto 0
             do (setf (sbit compressed-picture (+ destination-bit (* 8 byte-position) padding-length))
                      (ldb (byte 1 source-bit) byte))))
    compressed-picture))

(defun get-leading-byte (bit-array &optional (start 0) &aux (result 0))
  (loop
     for bit-array-index from start
     for result-index from 7 downto 0
     for result = (dpb (sbit bit-array bit-array-index) (byte 1 result-index) 0)
     then (dpb (sbit bit-array bit-array-index) (byte 1 result-index) result)
     finally (return result)))

(defun uncompress-picture (huffman-table compressed-picture width height)
  (let* ((png (make-instance 'zpng:png
                             :color-type :truecolor
                             :width width :height height))
         (uncompressed-picture (zpng:data-array png))
         ;;(uncompressed-picture (make-array (list height width 1)))
         (compressed-picture-index 0)
         (max-key-length (loop
                            for code being the hash-key in huffman-table
                            maximize (length code)))
         (red 0) (green 1) (blue 2))
    (loop
       for row from 0 below height
       do
         (if (evenp row)
             (progn
               (setf (aref uncompressed-picture row 0 green)
                     (get-leading-byte compressed-picture (prog1 compressed-picture-index (incf compressed-picture-index 8))))
                    (setf (aref uncompressed-picture row 1 blue)
                          (get-leading-byte compressed-picture (prog1 compressed-picture-index (incf compressed-picture-index 8)))))
             (progn
               (setf (aref uncompressed-picture row 0 red)
                     (get-leading-byte compressed-picture (prog1 compressed-picture-index (incf compressed-picture-index 8))))
                    (setf (aref uncompressed-picture row 1 green)
                          (get-leading-byte compressed-picture (prog1 compressed-picture-index (incf compressed-picture-index 8))))))
         (loop
            for column from 2 below width
            for try-start from compressed-picture-index
            do
              (loop
                 for i from 1 to max-key-length
                 for huffman-code = (subseq compressed-picture try-start (+ try-start i))
                 for pixel-delta-maybe = (gethash huffman-code huffman-table)
                 ;;do (format t "~% try-start: ~A; huffman-code: ~A " try-start huffman-code)
                 when pixel-delta-maybe
                 ;;do (format t "~% row: ~A; column: ~A; even: ~A; odd: ~A; pixel-delta: ~A; huffman-code: ~A;  " row column (aref uncompressed-picture row 0 0) (aref uncompressed-picture row 1 0) pixel-delta-maybe huffman-code)
                 ;;and
                 do
                   (if (evenp row)
                       (if (evenp column)
                           (setf (aref uncompressed-picture row column green)
                                 (- (aref uncompressed-picture row (- column 2) green)
                                    pixel-delta-maybe))
                           (setf (aref uncompressed-picture row column blue)
                                 (- (aref uncompressed-picture row (- column 2) blue)
                                    pixel-delta-maybe)))
                       (if (evenp column)
                           (setf (aref uncompressed-picture row column red)
                                 (- (aref uncompressed-picture row (- column 2) red)
                                    pixel-delta-maybe))
                           (setf (aref uncompressed-picture row column green)
                                 (- (aref uncompressed-picture row (- column 2) green)
                                    pixel-delta-maybe))))
                 and do (incf try-start (1- i))
                 and return nil
                 finally (error "Decoder out of step at row ~S, column ~S.  Giving up." row column))
            finally
              (setf compressed-picture-index (1+ try-start))
              ;;(print compressed-picture-index)
              ))
    png))
    ;;uncompressed-picture))
