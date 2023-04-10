(in-package :cl-beads)

(declaim (type double-float *maximum-bead-size*))
(defparameter *maximum-bead-size* 8d-2
  "Bead size cannot be bigger than this parameter. 1 means the whole
width of a drawing area.")

(defclass scheme-model ()
  ((document :type     document
             :reader   scheme-model-document
             :initarg  :document
             :initform (error "Specify document"))))

(defclass draft-model (scheme-model)
  ())

(sera:defconstructor rect
  (x      double-float)
  (y      double-float)
  (width  double-float)
  (height double-float))

(defgeneric beads-iterator (model)
  (:documentation "Return iterator which iterates over beads in the
document returning a pair (RECT . COLOR)."))

(defmethod beads-iterator ((model draft-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (height (document-height document))
         (scheme (document-scheme document))
         (bead-size (min *maximum-bead-size*
                         (float (/ width) 0d0)))
         (offset (/ (- 1 (* bead-size width)) 2)))
    (si:imap
     (lambda (coord)
       (destructuring-bind (i . j) coord
         (cons
          (rect (+ offset (* j bead-size))
                (+ 0      (* i bead-size))
                bead-size bead-size)
          (palette-color
           document (aref scheme i j)))))
     (si:product (si:range 0 height)
                 (si:range 0 width)))))

(defgeneric estimate-height (model)
  (:documentation "Get estimated height of a scheme in user corrdinate
system."))

(defmethod estimate-height ((model draft-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (height (document-height document))
         (bead-size (min *maximum-bead-size*
                         (float (/ width) 0d0))))
    (* bead-size height)))
