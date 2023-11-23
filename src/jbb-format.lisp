(in-package :cl-beads)

(sera:-> parse-color (list)
         (values color &optional))
(defun parse-color (color)
  (destructuring-bind (r g b a) (cdr color)
    (declare (ignore a))
    (color (/ r 255d0)
           (/ g 255d0)
           (/ b 255d0))))

(defmethod read-document (pathname (format (eql :jbb)))
  (let ((content (uiop:safe-read-file-form
                  pathname :package :clb-sym)))
    (unless (and (listp content)
                 (eq (car content) 'clb-sym::jbb))
      (error 'invalid-file :pathname pathname :description "Not a JBead file"))
    (let ((content (cdr content)))
      ;; Check version
      (let ((version (assoc 'clb-sym::version content)))
        (unless (= (second version) 1)
          (error 'invalid-file
                 :pathname pathname
                 :description "Wrong version")))
      (make-instance 'document-rope
                     :palette
                     (let ((colors (mapcar #'parse-color
                                    (alex:assoc-value content 'clb-sym::colors))))
                       (make-array (length colors)
                                   :element-type 'color
                                   :initial-contents colors))
                     :palette-idx
                     (let* ((view  (alex:assoc-value content 'clb-sym::view))
                            (color (alex:assoc-value view    'clb-sym::selected-color)))
                       (car color))
                     :outline-color
                     (let* ((view  (alex:assoc-value content 'clb-sym::view))
                            (color (alex:assoc-value view    'clb-sym::outline-color)))
                       (if color (parse-color (car color)) *default-outline-color*))
                     :reading-line-position
                     (let* ((view     (alex:assoc-value content 'clb-sym::view))
                            (position (alex:assoc-value view    'clb-sym::reading-line-position)))
                       (or (car position) 0))
                     :scheme
                     (let ((model (mapcar #'cdr (alex:assoc-value content 'clb-sym::model))))
                       (make-array (list (length model)
                                         (length (first model)))
                                   :element-type 'unsigned-byte
                                   :initial-contents model))))))

(defmethod write-document ((document jbb-format-mixin) pathname (format (eql :jbb)))
  (let ((form `(clb-sym::jbb
                (clb-sym::version 1)
                (clb-sym::author       ,(document-author       document))
                (clb-sym::organization ,(document-organization document))
                (clb-sym::notes        ,(document-notes        document))
                (clb-sym::colors
                 ,@(map 'list
                        (lambda (color)
                          `(clb-sym::rgb
                            ,(floor (* 255 (color-r color)))
                            ,(floor (* 255 (color-g color)))
                            ,(floor (* 255 (color-b color)))
                            255))
                        (document-palette document)))
                (clb-sym::view
                 (clb-sym::draft-visible clb-sym::true)
                 (clb-sym::corrected-visible clb-sym::true)
                 (clb-sym::simulation-visible clb-sym::true)
                 (clb-sym::report-visible clb-sym::true)
                 (clb-sym::selected-tool "pencil")
                 (clb-sym::selected-color ,(document-palette-idx document))
                 (clb-sym::zoom 2)
                 (clb-sym::scroll 0)
                 (clb-sym::shift 0)
                 (clb-sym::draw-colors true)
                 (clb-sym::draw-symbols false)
                 (clb-sym::symbols "·abcdefghijklmnopqrstuvwxyz+-/\\*")
                 ;; This one is not original to JBead program, but
                 ;; JBead just ignores unknown fields
                 ,(let ((color (document-outline-color document)))
                    `(clb-sym::outline-color
                      (clb-sym::rgb
                       ,(floor (* 255 (color-r color)))
                       ,(floor (* 255 (color-g color)))
                       ,(floor (* 255 (color-b color)))
                       255)))
                 (clb-sym::reading-line-position
                  ,(document-reading-line-position document)))
                (clb-sym::model
                 ,@(loop for i below (document-height document) collect
                         (cons 'clb-sym::row
                               (loop for j below (document-width document) collect
                                     (aref (document-scheme document) i j))))))))
    (with-open-file (output pathname
                            :direction         :output
                            :if-exists         :supersede
                            :if-does-not-exist :create)
      (let ((*package* (find-package :clb-sym)))
        (write form
               :stream output
               :case   :downcase))))
    document)

(defmethod supported-formats append ((document jbb-format-mixin))
  '((:jbb . "JBead format")))
