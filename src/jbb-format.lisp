(in-package :cl-beads)

(defpackage cl-beads-jbb)

(define-condition invalid-file (error)
  ((pathname :reader  invalid-file-pathname
             :initarg :pathname))
  (:report
   (lambda (c s)
     (format s "Invalid input file ~a"
             (invalid-file-pathname c)))))

(sera:-> read-jbb ((or pathname string))
         (values document &optional))
(defun read-jbb (pathname)
  (let ((content (uiop:safe-read-file-form
                  pathname
                  :package :cl-beads-jbb)))
    (unless (and (listp content)
                 (eq (car content) 'cl-beads-jbb::jbb))
      (error 'invalid-file :pathname pathname))
    (let ((content (cdr content)))
      ;; Check version
      (let ((version (assoc 'cl-beads-jbb::version content)))
        (unless (= (second version) 1)
          (error 'invalid-file :pathname pathname)))
      (make-instance 'document
                     :palette
                     (let ((colors (mapcar
                                    (lambda (color)
                                      (destructuring-bind (r g b a) (cdr color)
                                        (declare (ignore a))
                                        (color (/ r 255d0)
                                               (/ g 255d0)
                                               (/ b 255d0))))
                                    (alex:assoc-value content 'cl-beads-jbb::colors))))
                       (make-array (length colors)
                                   :element-type 'color
                                   :initial-contents colors))
                     :palette-idx
                     (let* ((view  (alex:assoc-value content 'cl-beads-jbb::view))
                            (color (alex:assoc-value view    'cl-beads-jbb::selected-color)))
                       (car color))
                     :scheme
                     (let ((model (mapcar #'cdr (alex:assoc-value content 'cl-beads-jbb::model))))
                       (make-array (list (length model)
                                         (length (first model)))
                                   :element-type 'unsigned-byte
                                   :initial-contents model))))))
