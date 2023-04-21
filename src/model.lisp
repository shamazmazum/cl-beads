(in-package :cl-beads)

(declaim (type double-float *maximum-bead-size*))
(defparameter *maximum-bead-size* 6.5d-2
  "Bead size cannot be bigger than this parameter. 1 means the whole
width of a drawing area.")

(defclass scheme-model ()
  ((document :type     document
             :reader   scheme-model-document
             :initarg  :document
             :initform (error "Specify document"))))

(sera:defconstructor rect
  (x      double-float)
  (y      double-float)
  (width  double-float)
  (height double-float))

(defgeneric beads-iterator (model)
  (:documentation "Return iterator which iterates over beads in the
document returning a pair (RECT . COLOR)."))

(defgeneric estimate-height (model)
  (:documentation "Get estimated height of a scheme in user corrdinate
system."))

(defun bead-size (width)
  (min *maximum-bead-size*
       (float (/ width) 0d0)))

(defmethod estimate-height ((model scheme-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (height (document-height document))
         (bead-size (bead-size width)))
    (* bead-size height)))

;; Draft
(defclass draft-model (scheme-model)
  ())

(defmethod beads-iterator ((model draft-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (height (document-height document))
         (scheme (document-scheme document))
         (bead-size (bead-size width))
         (offset (/ (- 1 (* bead-size width)) 2)))
    (si:imap
     (lambda (coord)
       (destructuring-bind (i . j) coord
         (cons
          (rect (+ offset (* j bead-size))
                (+        (* i bead-size))
                bead-size bead-size)
          (palette-color
           document (aref scheme i j)))))
     (si:product (si:range 0 height)
                 (si:range 0 width)))))

;; Dummy
(defclass dummy-model (scheme-model)
  ())

(defmethod beads-iterator ((model dummy-model))
  (si:list->iterator nil))

(defmethod estimate-height ((model dummy-model))
  0d0)

;; Corrected
(defclass corrected-model (scheme-model)
  ())

(defmethod beads-iterator ((model corrected-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (scheme (document-scheme document))
         (bead-size (bead-size (1+ width)))
         (offset (/ (- 1 (* bead-size (1+ width))) 2)))
    (si:imap
     (lambda (idx)
       (multiple-value-bind (q r)
           (floor idx (1+ (* width 2)))
         (cons
          (if (< r width)
              ;; Short row
              (rect (+ offset (* bead-size (+ r 0.5)))
                    (+        (* bead-size q 2))
                    bead-size bead-size)
              ;; Long row
              (rect (+ offset    (* bead-size (- r width)))
                    (+ bead-size (* bead-size q 2))
                    bead-size bead-size))
          (palette-color
           document (row-major-aref scheme idx)))))
     (si:range 0 (array-total-size scheme)))))
