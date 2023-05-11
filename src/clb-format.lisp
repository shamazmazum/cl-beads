(in-package :cl-beads)

(sera:-> parse-clb-color (list)
         (values color &optional))
(defun parse-clb-color (color)
  (apply #'color (cdr color)))

(defmethod read-document (pathname (format (eql :clb)))
  (let ((content (uiop:safe-read-file-form
                  pathname :package :clb-sym)))
    (unless (and (listp content)
                 (eq (car content) 'clb-sym::clb))
      (error 'invalid-file :pathname pathname :description "Not a cl-beads file"))
    (let ((content (cdr content)))
      ;; Check version
      (let ((version (alex:assoc-value content 'clb-sym::version)))
        (unless (and (integerp version)
                     (= version 1))
          (error 'invalid-file
                 :pathname pathname
                 :description "Wrong version")))
      (make-instance
       (let ((class (find-class (alex:assoc-value content 'clb-sym::class) nil)))
         (unless (subtypep class (find-class 'document))
           (error 'invalid-file
                  :pathname pathname
                  :description "Invalid document type"))
         class)
       :palette
       (let ((colors (mapcar #'parse-clb-color
                             (alex:assoc-value content 'clb-sym::colors))))
         (make-array (length colors)
                     :element-type 'color
                     :initial-contents colors))
       :palette-idx
       (let ((view (alex:assoc-value content 'clb-sym::view)))
         (alex:assoc-value view 'clb-sym::selected-color))
       :outline-color
       (let ((view (alex:assoc-value content 'clb-sym::view)))
         (parse-clb-color (car (alex:assoc-value view 'clb-sym::outline-color))))
       :scheme
       (let ((model (mapcar #'cdr (alex:assoc-value content 'clb-sym::model))))
         (make-array (list (length model)
                           (length (first model)))
                     :element-type 'unsigned-byte
                     :initial-contents model))))))

(defmethod write-document ((document document) pathname (format (eql :clb)))
  (let ((form `(clb-sym::clb
                (clb-sym::version . 1)
                (clb-sym::author       . ,(document-author       document))
                (clb-sym::organization . ,(document-organization document))
                (clb-sym::notes        . ,(document-notes        document))
                (clb-sym::class        . ,(class-name (class-of document)))
                (clb-sym::colors
                 ,@(map 'list
                        (lambda (color)
                          `(clb-sym::rgb
                            ,(color-r color)
                            ,(color-g color)
                            ,(color-b color)))
                        (document-palette document)))
                (clb-sym::view
                 (clb-sym::selected-color . ,(document-palette-idx document))
                 ,(let ((color (document-outline-color document)))
                    `(clb-sym::outline-color
                      (clb-sym::rgb
                       ,(color-r color)
                       ,(color-g color)
                       ,(color-b color)))))
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
        (write form :stream output))))
    document)
