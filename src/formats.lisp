(in-package :cl-beads)

(defpackage cl-beads-format-symbols
  (:nicknames #:clb-sym))

(define-condition cl-beads-error (error)
  ((pathname    :reader  invalid-file-pathname
                :initarg :pathname)
   (description :reader  invalid-file-description
                :initarg :description))
  (:documentation "Generic cl-beads error"))

(define-condition invalid-file (cl-beads-error)
  ()
  (:report
   (lambda (c s)
     (format s "Invalid input file ~a (~a)"
             (invalid-file-pathname    c)
             (invalid-file-description c))))
  (:documentation "An error which occures during a read operation on invalid file"))

(define-condition output-error (cl-beads-error)
  ()
  (:report
   (lambda (c s)
     (format s "Error occured while writing to ~a (~a)"
             (invalid-file-pathname    c)
             (invalid-file-description c))))
  (:documentation "An error which occures while writing a document to the disk"))

(define-condition wrong-format (cl-beads-error)
  ()
  (:report
   (lambda (c s)
     (format s "Unsupported format for this document: ~a"
             (invalid-file-pathname c)))))

(deftype file-format () '(member :jbb :clb))

(sera:-> guess-format ((or string pathname))
         (values file-format &optional))
(defun guess-format (pathname)
  (let* ((file-type (pathname-type (pathname pathname)))
         (format (intern (string-upcase file-type)
                         (find-package :keyword))))
    (if (typep format 'file-format)
        format
        (error 'wrong-format :pathname pathname))))

(defgeneric read-document (pathname format)
  (:documentation "Read a document from a file with path PATHNAME of format FORMAT"))

(defmethod read-document :around (pathname format)
  (declare (ignore format))
    (handler-case
      (call-next-method)
    ;; Resignal all errors as invalid-file
    ((and error (not invalid-file)) (c)
      (error 'invalid-file
             :pathname pathname
             :description (with-output-to-string (out)
                            (princ c out))))))

(defgeneric write-document (document pathname format)
  (:documentation "Write a document to a file with path PATHNAME of format FORMAT"))

(defmethod write-document :around ((document document) pathname format)
  (declare (ignore format))
  (handler-case
      (call-next-method)
    ;; Resignal all errors as output-error
    ((and error (not wrong-format)) (c)
      (error 'output-error
             :pathname pathname
             :description (with-output-to-string (out)
                            (princ c out))))))

(defmethod write-document ((document document) pathname format)
  (declare (ignore format))
  (error 'wrong-format :pathname pathname))
