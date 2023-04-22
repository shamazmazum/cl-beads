(in-package :cl-beads)

(declaim (type double-float *maximum-bead-size*))
(defparameter *maximum-bead-size* 6.5d-2
  "Bead size cannot be bigger than this parameter. 1 means the whole
width of a drawing area.")

(defclass scheme-model ()
  ((document :type     document
             :reader   scheme-model-document
             :initarg  :document
             :initform (error "Specify document")))
  (:documentation "A generic class for scheme models (things which
control how a scheme is represented in SCHEME-AREA widget)."))

(sera:defconstructor rect
  (x      double-float)
  (y      double-float)
  (width  double-float)
  (height double-float))

(defgeneric beads-iterator (model)
  (:documentation "Return iterator which iterates over beads in the
document returning a pair (RECT . COLOR)."))

(defgeneric estimate-height (model)
  (:documentation "Get estimated height of a scheme in the user
corrdinate system. The user coordinate system spans from left (0) to
right (1) and from bottom (0) to top (x) such as a bead is represented
as a square."))

(defgeneric bead-size (model)
  (:documentation "Get size of a bead for this model in user coordinates."))

(defmethod bead-size ((model scheme-model))
  (let ((document (scheme-model-document model)))
    (min *maximum-bead-size*
         (float (/ (document-width document)) 0d0))))

(defmethod estimate-height ((model scheme-model))
  (let ((document (scheme-model-document model)))
    (* (bead-size model)
       (document-height document))))

;; Draft
(defclass draft-model (scheme-model)
  ()
  (:documentation "A model where each row has the same number of beads."))

(defmethod beads-iterator ((model draft-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (height (document-height document))
         (scheme (document-scheme document))
         (bead-size (bead-size model))
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
  ()
  (:documentation "This model does not show any beads at all."))

(defmethod beads-iterator ((model dummy-model))
  (si:list->iterator nil))

(defmethod estimate-height ((model dummy-model))
  0d0)

;; Corrected
(defclass corrected-model (scheme-model)
  ()
  (:documentation "This model represents what you must actually weave
with a crochet (I suppose. I never used that technique.) or a needle."))

(defmethod beads-iterator ((model corrected-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (scheme (document-scheme document))
         (bead-size (bead-size model))
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

(defmethod bead-size ((model corrected-model))
  (let ((document (scheme-model-document model)))
    (min *maximum-bead-size*
         (float (/ (1+ (document-width document))) 0d0))))

;; TODO: Simulated
(defclass simulated-model (dummy-model)
  ()
  (:documentation "This is what the result looks like."))
