(in-package :cl-beads)

(defpackage cl-beads-jbb)

(define-condition cl-beads-error (error)
  ((pathname    :reader  invalid-file-pathname
                :initarg :pathname)
   (description :reader  invalid-file-description
                :initarg :description)))

(define-condition invalid-file (cl-beads-error)
  ()
  (:report
   (lambda (c s)
     (format s "Invalid input file ~a (~a)"
             (invalid-file-pathname    c)
             (invalid-file-description c)))))

(define-condition output-error (cl-beads-error)
  ()
  (:report
   (lambda (c s)
     (format s "Error occured while writing to ~a (~a)"
             (invalid-file-pathname    c)
             (invalid-file-description c)))))

(sera:-> parse-color (list)
         (values color &optional))
(defun parse-color (color)
  (destructuring-bind (r g b a) (cdr color)
    (declare (ignore a))
    (color (/ r 255d0)
           (/ g 255d0)
           (/ b 255d0))))

(sera:-> %read-jbb ((or pathname string))
         (values document-rope &optional))
(defun %read-jbb (pathname)
  "Read a document from JBead file format."
  (let ((content (uiop:safe-read-file-form
                  pathname
                  :package :cl-beads-jbb)))
    (unless (and (listp content)
                 (eq (car content) 'cl-beads-jbb::jbb))
      (error 'invalid-file :pathname pathname :description "Not a JBead file"))
    (let ((content (cdr content)))
      ;; Check version
      (let ((version (assoc 'cl-beads-jbb::version content)))
        (unless (= (second version) 1)
          (error 'invalid-file
                 :pathname pathname
                 :description "Wrong version")))
      (make-instance 'document-rope
                     :palette
                     (let ((colors (mapcar #'parse-color
                                    (alex:assoc-value content 'cl-beads-jbb::colors))))
                       (make-array (length colors)
                                   :element-type 'color
                                   :initial-contents colors))
                     :palette-idx
                     (let* ((view  (alex:assoc-value content 'cl-beads-jbb::view))
                            (color (alex:assoc-value view    'cl-beads-jbb::selected-color)))
                       (car color))
                     :outline-color
                     (let* ((view  (alex:assoc-value content 'cl-beads-jbb::view))
                            (color (alex:assoc-value view    'cl-beads-jbb::outline-color)))
                       (if color (parse-color (car color)) *default-outline-color*))
                     :scheme
                     (let ((model (mapcar #'cdr (alex:assoc-value content 'cl-beads-jbb::model))))
                       (make-array (list (length model)
                                         (length (first model)))
                                   :element-type 'unsigned-byte
                                   :initial-contents model))))))

(sera:-> read-jbb ((or pathname string))
         (values document-rope &optional))
(defun read-jbb (pathname)
  (handler-case
      (%read-jbb pathname)
    ;; Resignal all errors as invalid-file
    ((and error (not invalid-file)) (c)
      (error 'invalid-file
             :pathname pathname
             :description (with-output-to-string (out)
                            (princ c out))))))

(sera:-> %write-jbb (document (or pathname string))
         (values list &optional))
(defun %write-jbb (document pathname)
  "Write a document to JBead file format"
  (let ((form `(cl-beads-jbb::jbb
                (cl-beads-jbb::version 1)
                (cl-beads-jbb::author       ,(document-author       document))
                (cl-beads-jbb::organization ,(document-organization document))
                (cl-beads-jbb::notes        ,(document-notes        document))
                (cl-beads-jbb::colors
                 ,@(map 'list
                        (lambda (color)
                          `(cl-beads-jbb::rgb
                            ,(floor (* 255 (color-r color)))
                            ,(floor (* 255 (color-g color)))
                            ,(floor (* 255 (color-b color)))
                            255))
                        (document-palette document)))
                (cl-beads-jbb::view
                 (cl-beads-jbb::draft-visible cl-beads-jbb::true)
                 (cl-beads-jbb::corrected-visible cl-beads-jbb::true)
                 (cl-beads-jbb::simulation-visible cl-beads-jbb::true)
                 (cl-beads-jbb::report-visible cl-beads-jbb::true)
                 (cl-beads-jbb::selected-tool "pencil")
                 (cl-beads-jbb::selected-color ,(document-palette-idx document))
                 (cl-beads-jbb::zoom 2)
                 (cl-beads-jbb::scroll 0)
                 (cl-beads-jbb::shift 0)
                 (cl-beads-jbb::draw-colors true)
                 (cl-beads-jbb::draw-symbols false)
                 (cl-beads-jbb::symbols "·abcdefghijklmnopqrstuvwxyz+-/\\*")
                 ;; This one is not original to JBead program, but
                 ;; JBead just ignores unknown fields
                 ,(let ((color (document-outline-color document)))
                    `(cl-beads-jbb::outline-color
                      (cl-beads-jbb::rgb
                       ,(floor (* 255 (color-r color)))
                       ,(floor (* 255 (color-g color)))
                       ,(floor (* 255 (color-b color)))
                       255))))
                (cl-beads-jbb::model
                 ,@(loop for i below (document-height document) collect
                         (cons 'cl-beads-jbb::row
                               (loop for j below (document-width document) collect
                                     (aref (document-scheme document) i j))))))))
    (with-open-file (output pathname
                            :direction         :output
                            :if-exists         :supersede
                            :if-does-not-exist :create)
      (let ((*package* (find-package :cl-beads-jbb)))
        (write form
               :stream output
               :case   :downcase)))
    form))

(sera:-> write-jbb (document (or pathname string))
         (values list &optional))
(defun write-jbb (document pathname)
  (handler-case
      (%write-jbb document pathname)
    ;; Resignal all errors as output-error
    (error (c) (error 'output-error
                      :pathname pathname
                      :description (with-output-to-string (out)
                                     (princ c out))))))
