;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2012 Bert Burgemeister
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

(defun indent-json (text &key (indentation 0) suppress-indentation)
  "Improve readability of JSON text.  String delimiter is `\"'."
  (flet ((leftmost-position (string char-bag)
           (let ((positions (mapcar #'(lambda (x) (position x string))
                                    char-bag)))
             (when (some #'identity positions)
               (loop for p in positions when p minimize it))))
         (newline-and-spaces (n)
           (concatenate 'string
                        (string #\Newline)
                        (make-string n :initial-element #\Space))))
    (setf text (string-left-trim " 	" text)) ;Tab and Space
    (let* ((quoted-start (leftmost-position text '(#\")))
           (quoted-end (when quoted-start
                         (+ quoted-start
                            2
                            (leftmost-position (subseq text (1+ quoted-start))
                                               '(#\")))))
           (open-position (leftmost-position text '(#\{ #\[)))
           (close-position (leftmost-position text '(#\} #\])))
           (delimiter-position (leftmost-position text '(#\, #\:))))
      (cond
        ((and delimiter-position        ;delimiter at position 0
              (zerop delimiter-position))
         (concatenate 'string
                      (subseq text 0 (1+ delimiter-position))
                      (unless suppress-indentation
                        (newline-and-spaces indentation))
                      (indent-json (subseq text (1+ delimiter-position))
                                   :indentation indentation
                                   :suppress-indentation t)))
        ((and close-position            ;closer at position 0
              (zerop close-position))
         (concatenate 'string
                      (subseq text 0 (1+ close-position))
                      (indent-json (subseq text (1+ close-position))
                                   :indentation (1- indentation))))
            
        ((and open-position             ;opener at position 0
              (zerop open-position))
         (concatenate 'string
                      (newline-and-spaces indentation)
                      (subseq text open-position (1+ open-position))
                      (indent-json (subseq text (1+ open-position))
                                   :indentation (1+ indentation)
                                   :suppress-indentation t)))
        ((and quoted-start              ;quote comes first
              (or (not open-position)
                  (< quoted-start open-position))
              (or (not close-position)
                  (< quoted-start close-position))
              (or (not delimiter-position)
                  (< quoted-start delimiter-position)))
         (concatenate 'string
                      (unless suppress-indentation
                        (newline-and-spaces indentation))
                      (subseq text 0 quoted-end)
                      (indent-json (subseq text quoted-end)
                                   :indentation indentation
                                   :suppress-indentation t)))
        ((and close-position            ;closer > 0 and comes first
              (or (not open-position)
                  (< close-position open-position)))
         (concatenate 'string
                      (unless suppress-indentation
                        (newline-and-spaces indentation))
                      (subseq text 0 (1+ close-position))
                      (indent-json (subseq text (1+ close-position))
                                   :indentation (1- indentation))))
        ((and delimiter-position
              (or (not open-position)
                  (< delimiter-position open-position)))
         (concatenate 'string
                      (subseq text 0 (1+ delimiter-position))
                      (indent-json (subseq text (1+ delimiter-position))
                                   :indentation indentation
                                   :suppress-indentation t)))
        (open-position                  ;opener > 0 and comes first
         (concatenate 'string
                      (unless suppress-indentation
                        (newline-and-spaces indentation))
                      (subseq text 0 open-position)
                      (newline-and-spaces indentation)
                      (subseq text open-position (1+ open-position))
                      (indent-json (subseq text (1+ open-position))
                                   :indentation (1+ indentation)
                                   :suppress-indentation t)))
        (t
         (concatenate 'string
                      (newline-and-spaces indentation)
                      text))))))