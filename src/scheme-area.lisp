(in-package :cl-beads)

(defparameter *outline-width* 1d-2
  "Default outline width for a bead.")

(defclass scheme-area (gtk-drawing-area)
  ((model          :initform (error "Specify model")
                   :initarg  :model
                   :type     scheme-model
                   :reader   scheme-area-model)
   (outline-width  :initform *outline-width*
                   :initarg  :outline-width
                   :type     double-float
                   :accessor scheme-area-outline-width)
   (position       :initform 0d0
                   :initarg  :position
                   :type     double-float
                   :accessor scheme-area-position)
   (coord-trans    :initform (cairo-matrix-init-identity)
                   :type     list
                   :accessor scheme-area-coord-trans)
   (free-drawing-p :initform nil
                   :initarg  :free-drawing-p
                   :type     boolean
                   :accessor scheme-area-free-drawing-p
                   :documentation "When T \"my-bead-clicked\" signal
is emitted when the mouse pointer is moved and the left mouse button is held."))
  (:metaclass gobject-class)
  (:documentation "Widget which shows a scheme (via a model) and
allows drawing on it."))

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
     ctx
     (gdk-rectangle-x      allocation)
     (gdk-rectangle-y      allocation)
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
                 (document-outline-color (scheme-model-document model)))))
  nil)

(defun maybe-emit-bead-clicked (area x y)
  "If (x, y) is a coordinate of a bead emit \"my-bead-clicked\" signal."
  (let ((allocation (gtk-widget-get-allocation area))
        (model (scheme-area-model area)))
    (multiple-value-bind (x y)
        (cairo-matrix-transform-point
         (scheme-area-coord-trans area)
         (+ x (gdk-rectangle-x allocation))
         (+ y (gdk-rectangle-y allocation)))
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
        (g-signal-emit area
                       "my-bead-clicked"
                       (bead-spec-linear-index bead-spec))))))

(defun button-clicked (widget event)
  "Handle button click on a scheme-area."
  (when (and (= (gdk-event-button-button event) 1)
             (not (scheme-area-free-drawing-p widget)))
    ;; Left mouse button
    (maybe-emit-bead-clicked
     widget
     (gdk-event-button-x event)
     (gdk-event-button-y event)))
  nil)

(defun pointer-moved (widget event)
  (when (and (member :button1-mask (gdk-event-motion-state event))
             (scheme-area-free-drawing-p widget))
    (maybe-emit-bead-clicked
     widget
     (gdk-event-motion-x event)
     (gdk-event-motion-y event)))
  nil)

(defmethod initialize-instance :after ((scheme-area scheme-area) &rest args)
  (declare (ignore args))
  (g-signal-connect scheme-area "draw" #'draw-scheme)
  (g-signal-connect scheme-area "button-press-event"  #'button-clicked)
  (g-signal-connect scheme-area "motion-notify-event" #'pointer-moved)
  (gtk-widget-add-events scheme-area '(:button-press-mask
                                       :pointer-motion-mask)))

(defmethod (setf scheme-area-position) :after (value (scheme-area scheme-area))
  (declare (ignore value))
  (gtk-widget-queue-draw scheme-area))

;; Ruler

(defparameter *ruler-spacing* 10
  "Default ruler spacing value (in number of beads)")

(defclass ruler-mixin (gtk-widget)
  ((ruler-spacing   :initform *ruler-spacing*
                    :initarg  :ruler-spacing
                    :type     unsigned-byte
                    :accessor scheme-area-ruler-spacing)
   (show-markings-p :initform nil
                    :initarg  :show-markings-p
                    :accessor scheme-area-show-markings-p
                    :type     boolean))
  (:metaclass gobject-class)
  (:documentation "A ruler for scheme-area object"))

(defmethod draw-scheme progn ((widget ruler-mixin) ctx)
  "Draw a scheme on a scheme-area widget."
  (let ((ctx (pointer ctx))
        (allocation (gtk-widget-get-allocation widget))
        (model (scheme-area-model widget)))
    ;; Set color
    (let ((color (document-outline-color (scheme-model-document model))))
      (cairo-set-source-rgb
       ctx
       (color-r color)
       (color-g color)
       (color-b color)))

    ;; Set font
    (when (scheme-area-show-markings-p widget)
      (cairo-select-font-face ctx "DejaVu Sans" :normal :bold)
      (cairo-set-font-size ctx (* 2 (bead-size model))))

    ;; Emphasize rows
    (cairo-set-line-width ctx (* 2 (scheme-area-outline-width widget)))
    (loop for row from 0 below (document-height
                                (scheme-model-document model))
          by (scheme-area-ruler-spacing widget)
          for y = (* (bead-size model) row)
          when (point-visible-p ctx allocation 0 y) do
          (cairo-move-to ctx 0 y)
          (cairo-line-to ctx 1 y)
          (cairo-stroke ctx)
          (when (scheme-area-show-markings-p widget)
            (let* ((text (format nil "~d" row))
                   (extents (cairo-text-extents ctx text)))
              (cairo-move-to
               ctx
               (+ (bead-size model) (cairo-text-extents-t-x-bearing extents))
               (+ (bead-size model) y))
              (cairo-save ctx)
              (cairo-rotate ctx pi)
              (cairo-scale ctx -1 1)
              (cairo-show-text ctx text)
              (cairo-restore ctx)))))
  nil)

;; Reader line

(defclass reader-line-mixin (gtk-widget)
  ((line-position :initform 0
                  :initarg  :line-position
                  :type     unsigned-byte
                  :accessor scheme-area-line-position)
   (show-line-p   :initform nil
                  :initarg  :show-line-p
                  :type     boolean
                  :accessor scheme-area-show-reader-line-p))
  (:metaclass gobject-class)
  (:documentation "A reading line for scheme-area object"))

;; Preferably must be called last for proper color mixing
(defmethod draw-scheme progn ((widget reader-line-mixin) ctx)
  (let* ((ctx (pointer ctx))
         (allocation (gtk-widget-get-allocation widget))
         (model (scheme-area-model widget))
         (color (document-outline-color (scheme-model-document model)))
         (bead-size (bead-size model))
         (position (* bead-size
                      (+ (scheme-area-line-position widget) 5d-1))))

    ;; Draw focus if needed
    (when (gtk-widget-has-focus widget)
      (cairo-save ctx)
      (cairo-identity-matrix ctx)
      (gtk-fixed:gtk-render-focus
       (gtk-widget-get-style-context widget)
       ctx
       (gdk-rectangle-x      allocation)
       (gdk-rectangle-y      allocation)
       (gdk-rectangle-width  allocation)
       (gdk-rectangle-height allocation))
      (cairo-restore ctx))

    (when (scheme-area-show-reader-line-p widget)
      (cairo-set-source-rgba
       ctx
       (color-r color)
       (color-g color)
       (color-b color)
       5d-1)
      (cairo-set-line-width ctx (* 2 (scheme-area-outline-width widget)))
      (cairo-move-to ctx 0 position)
      (cairo-line-to ctx 1 position)
      (cairo-stroke ctx)))
  nil)

(defun key-pressed (widget event)
  (when (scheme-area-show-reader-line-p widget)
    (let ((document (scheme-model-document
                     (scheme-area-model widget))))
      (with-accessors ((position scheme-area-line-position))
          widget
        (case (gdk-event-key-keyval event)
          (#xff52
           (setf position (min (1+ position)
                               (document-height document)))
           (gtk-widget-queue-draw widget))
          (#xff54
           (setf position (max (1- position) 0))
           (gtk-widget-queue-draw widget)))))
    ;; Prevent focus from leaving the area if the line is currently
    ;; shown
    t))

(defmethod initialize-instance :after ((widget reader-line-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (gtk-widget-focus-on-click widget) nil
        (gtk-widget-can-focus      widget) t)
  (g-signal-connect widget "key-press-event" #'key-pressed)
  (gtk-widget-add-events widget '(:key-press-mask)))
