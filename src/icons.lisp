(in-package :cl-beads)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun file-encode-base64 (filename)
    (with-output-to-string (output)
      (with-open-file (input filename :element-type '(unsigned-byte 8))
        (s-base64:encode-base64 input output)))))

(defun read-encoded-png (string)
  "Read a base64-encoded png image"
  (flexi:with-input-from-sequence
      (input-bytes
       (with-input-from-string (input string)
         (s-base64:decode-base64-bytes input)))
  (opticl:read-png-stream input-bytes)))

;; Converted from this file:
;; https://commons.wikimedia.org/wiki/File:Antu_document-edit-sign.svg
(defparameter *pencil-icon*
  (read-encoded-png
   #.(file-encode-base64
      (asdf:system-relative-pathname :cl-beads "pencil.png")))
  "24x24 icon depicting a pencil")

;; Converted from this file:
;; https://commons.wikimedia.org/wiki/File:Draw_-_The_Noun_Project.svg
(defparameter *line-icon*
  (read-encoded-png
   #.(file-encode-base64
      (asdf:system-relative-pathname :cl-beads "draw.png")))
  "24x24 icon depicting a pencil drawing a curve")
