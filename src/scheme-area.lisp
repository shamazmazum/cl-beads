(in-package :cl-beads)

(defparameter *outline-width* 1d-2
  "Default outline width for a bead.")

(defparameter *ruler-spacing* 10
  "Default ruler spacing value (in number of beads)")

(defclass scheme-area (gtk-drawing-area)
  ((model         :initform (error "Specify model")
                  :initarg  :model
                  :type     scheme-model
                  :reader   scheme-area-model)
   (outline-width :initform *outline-width*
                  :initarg  :outline-width
                  :type     double-float
                  :accessor scheme-area-outline-width)
   (ruler-spacing :initform *ruler-spacing*
                  :initarg  :ruler-spacing
                  :type     unsigned-byte
                  :accessor scheme-area-ruler-spacing)
   (position      :initform 0d0
                  :initarg  :position
                  :type     double-float
                  :accessor scheme-area-position)
   (coord-trans   :initform (cairo-matrix-init-identity)
                  :type     list
                  :accessor scheme-area-coord-trans))
  (:metaclass gobject-class)
  (:documentation "Widget which shows a scheme (via a model) and
allows drawing on it."))

(defclass ruler-mixin (gtk-widget)
  ()
  (:metaclass gobject-class)
  (:documentation "Scheme area with a ruler"))

(defgeneric draw-scheme (widget ctx)
  (:documentation "Draw a scheme on a scheme-area widget.")
  (:method-combination progn))

(defun draw-bead (ctx rect color outline-color)
  "Draw a bead with position RECT and color COLOR on a cairo context
CTX."
  (flet ((draw-rect ()
           (cairo-rectangle
            ctx
            (rect-x      rect)
            (rect-y      rect)
            (rect-width  rect)
            (rect-height rect))))
    (cairo-set-source-rgb
     ctx
     (color-r color)
     (color-g color)
     (color-b color))
    (draw-rect)
    (cairo-fill ctx)
    (cairo-set-source-rgb
     ctx
     (color-r outline-color)
     (color-g outline-color)
     (color-b outline-color))
    (draw-rect)
    (cairo-stroke ctx)))

;; Bring this function from GTK4
(defun gdk-rectangle-contains-point (rect x y)
  "Test if a point (X, Y) is contained in a rectangle RECT."
  (let ((rect-x      (gdk-rectangle-x      rect))
        (rect-y      (gdk-rectangle-y      rect))
        (rect-width  (gdk-rectangle-width  rect))
        (rect-height (gdk-rectangle-height rect)))
    (and
     (<= rect-x x (+ rect-x rect-width))
     (<= rect-y y (+ rect-y rect-height)))))

;; Is it really useful?
(defun point-visible-p (ctx allocation x y)
  (multiple-value-bind (x-dev y-dev)
      (cairo-user-to-device ctx x y)
    (gdk-rectangle-contains-point allocation x-dev y-dev)))

(defun filter-visible-beads (ctx allocation iterator)
  (flet ((bead-visible-p (bead-spec)
           (let ((rect (bead-spec-rect bead-spec)))
             (point-visible-p ctx allocation
                              (rect-x rect)
                              (rect-y rect)))))
    (si:take-while
     #'bead-visible-p
     (si:drop-while
      (alex:compose #'not #'bead-visible-p)
      iterator))))

(defun setup-coordinate-system (ctx scheme-area scheme-height)
  "Setup user to screen coordinate system transform."
  (let* ((allocation (gtk-widget-get-allocation scheme-area))
         (width  (float (gdk-rectangle-width  allocation) 0d0))
         (height (float (gdk-rectangle-height allocation) 0d0))
         (maxy (/ height width)))
    ;; X spans from 0 to 1
    ;; Y spans from 0 to maxy
    (cairo-translate ctx 0 height)
    (cairo-scale ctx width (- width))
    ;; Setup scrolling offset
    (cairo-translate
     ctx 0
     (- (* (max (- scheme-height maxy) 0)
           (scheme-area-position scheme-area))))))

(defmethod draw-scheme progn ((widget scheme-area) ctx)
  "Draw a scheme on a scheme-area widget."
  (let ((ctx (pointer ctx))
        (allocation (gtk-widget-get-allocation widget))
        (model (scheme-area-model widget)))
    ;; Fill with background
    (gtk-render-background
     (gtk-widget-get-style-context widget)
     ctx 0 0
     (gdk-rectangle-width  allocation)
     (gdk-rectangle-height allocation))
    ;; Set coordinate system
    (setup-coordinate-system
     ctx widget
     (estimate-height model))
    ;; Store current transform
    (setf (scheme-area-coord-trans widget)
          (cairo-matrix-invert
           (cairo-get-matrix ctx)))
    ;; Set line width
    (cairo-set-line-width ctx (scheme-area-outline-width widget))

    ;; Draw beads
    (si:do-iterator (bead-spec (filter-visible-beads ctx allocation (beads-iterator model)))
      (draw-bead ctx
                 (bead-spec-rect  bead-spec)
                 (bead-spec-color bead-spec)
                 (document-outline-color (scheme-model-document model))))))

(defmethod draw-scheme progn ((widget ruler-mixin) ctx)
  "Draw a scheme on a scheme-area widget."
  (let ((ctx (pointer ctx))
        (allocation (gtk-widget-get-allocation widget))
        (model (scheme-area-model widget)))
    (let ((color (document-outline-color (scheme-model-document model))))
      (cairo-set-source-rgb
       ctx
       (color-r color)
       (color-g color)
       (color-b color)))
        ;; Emphasize rows
    (cairo-set-line-width ctx (* 2 (scheme-area-outline-width widget)))
    (loop for y from 0d0 below (estimate-height model)
          by (* (scheme-area-ruler-spacing widget) (bead-size model))
          when (point-visible-p ctx allocation 0 y) do
          (cairo-move-to ctx 0 y)
          (cairo-line-to ctx 1 y)
          (cairo-stroke ctx))))

(defun button-clicked (widget event)
  "Handle button click on a scheme-area. If clicked on a bead, reemit
as \"my-bead-clicked\"."
  (when (= (gdk-event-button-button event) 1)
    (let ((allocation (gtk-widget-get-allocation widget))
          (model (scheme-area-model widget)))
      (multiple-value-bind (x y)
          (cairo-matrix-transform-point
           (scheme-area-coord-trans widget)
           (+ (gdk-event-button-x event)
              (gdk-rectangle-x allocation))
           (+ (gdk-rectangle-y allocation)
              (gdk-event-button-y event)))
        (alex:when-let
            ((bead-spec
              (si:consume-one
               (si:drop-while
                (lambda (bead-spec)
                  (let ((rect (bead-spec-rect bead-spec)))
                    (not
                     (and
                      (<= (rect-x rect) x (+ (rect-x rect) (rect-width rect)))
                      (<= (rect-y rect) y (+ (rect-y rect) (rect-height rect)))))))
                (beads-iterator model)))))
          (g-signal-emit widget
                         "my-bead-clicked"
                         (bead-spec-linear-index bead-spec)))))))

(defmethod initialize-instance :after ((scheme-area scheme-area) &rest args)
  (declare (ignore args))
  (g-signal-connect scheme-area "draw" #'draw-scheme)
  (g-signal-connect scheme-area "button-press-event" #'button-clicked)
  (gtk-widget-add-events scheme-area '(:button-press-mask)))

(defmethod (setf scheme-area-position) :after (value (scheme-area scheme-area))
  (declare (ignore value))
  (gtk-widget-queue-draw scheme-area))
