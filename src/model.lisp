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

(sera:defconstructor bead-spec
  (rect         rect)
  (color        color)
  (linear-index unsigned-byte))

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
         (bead-spec
          (rect (+ offset (* j bead-size))
                (+        (* i bead-size))
                bead-size bead-size)
          (palette-color document (aref scheme i j))
          (array-row-major-index scheme i j))))
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
         (bead-spec
          (if (< r width)
              ;; Short row
              (rect (+ offset (* bead-size (+ r 0.5)))
                    (+        (* bead-size q 2))
                    bead-size bead-size)
              ;; Long row
              (rect (+ offset    (* bead-size (- r width)))
                    (+ bead-size (* bead-size q 2))
                    bead-size bead-size))
          (palette-color document (row-major-aref scheme idx))
          idx)))
     (si:range 0 (array-total-size scheme)))))

(defmethod bead-size ((model corrected-model))
  (let ((document (scheme-model-document model)))
    (min *maximum-bead-size*
         (float (/ (1+ (document-width document))) 0d0))))

;; Simulated
(defclass simulated-model (scheme-model)
  ((rotation :initform 0
             :initarg  :rotation
             :type     integer
             :accessor simulated-model-rotation))
  (:documentation "This is what the result looks like."))

(defmethod beads-iterator ((model simulated-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (scheme (document-scheme document))
         (bead-size (bead-size model))
         (bead-size/2 (/ bead-size 2))
         (row-length (ceiling width 2))
         (offset (/ (- 1 (* bead-size (- row-length 0.5))) 2)))
    (flet ((bead-rect (y x)
             ;; Rows have a different "shift"
             (if (evenp y)
                 ;; Even row
                 (rect (+ offset (if (zerop x) 0 (* bead-size (- x 0.5))))
                       (* y bead-size)
                       (if (zerop x) bead-size/2 bead-size)
                       bead-size)
                 ;; Odd row
                 (rect (+ offset (* x bead-size))
                       (+        (* y bead-size))
                       (if (= (1+ x) row-length)
                           bead-size/2 bead-size)
                       bead-size))))
      (si:filter
       #'identity
       (si:imap
        (lambda (idx)
          (alex:when-let
              ((rect
                (multiple-value-bind (q r)
                    (floor (+ (mod (simulated-model-rotation model) width) idx)
                           (1+ (* 2 width)))
                  (cond
                    ;; Even row, first bead's width is half the normal width
                    ((< r (1- row-length))
                     (bead-rect (* 2 q) (1+ r)))
                    ;; Odd row, last bead's width is half the normal width
                    ((< (1- width) r (+ width row-length))
                     (bead-rect (1+ (* 2 q)) (- r width)))
                    ;; First bead in the next even row
                    ((= r (* 2 width))
                     (bead-rect (* 2 (1+ q)) 0))
                    (t
                     ;; Invisible bead
                     nil)))))
            (bead-spec rect
                       (palette-color document (row-major-aref scheme idx))
                       idx)))
        (si:range 0 (array-total-size scheme)))))))

;; Rings in mosaic technique
(defclass ring-model (scheme-model)
  ()
  (:documentation "A model for rings in mosaic technique"))

(defmethod beads-iterator ((model ring-model))
  (let* ((document (scheme-model-document model))
         (width  (document-width  document))
         (height (document-height document))
         (scheme (document-scheme document))
         (bead-size (bead-size model))
         (offset (/ (- 1 (* bead-size (+ 5d-1 width))) 2)))
    (si:imap
     (lambda (coord)
       (destructuring-bind (i . j) coord
         (bead-spec
          (rect (+ offset
                   (if (evenp i) (* 5d-1  bead-size) 0)
                   (* j bead-size))
                (+ (* i bead-size))
                bead-size bead-size)
          (palette-color document (aref scheme i j))
          (array-row-major-index scheme i j))))
     (si:product (si:range 0 height)
                 (si:range 0 width)))))

(defmethod bead-size ((model ring-model))
  (let ((document (scheme-model-document model)))
    (min *maximum-bead-size*
         (/ (+ 5d-1 (document-width document))))))
