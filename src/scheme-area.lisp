(in-package :cl-beads)

(defparameter *outline-width* 1d-2
  "Default outline width for a bead.")

(defclass scheme-area (gtk-drawing-area)
  ((model         :initform (error "Specify model")
                  :initarg  :model
                  :type     scheme-model
                  :reader   scheme-area-model)
   (outline-width :initform *outline-width*
                  :initarg  :outline-width
                  :type     double-float
                  :accessor scheme-area-outline-width)
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

(defun draw-bead (ctx rect color)
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
    (cairo-set-source-rgb ctx 0.0 0.0 0.0)
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
(defun filter-visible (ctx allocation model)
  (flet ((visible (bead)
           (destructuring-bind (rect . color) bead
             (declare (ignore color))
             (multiple-value-bind (x-start y-start)
                 (cairo-user-to-device ctx (rect-x rect) (rect-y rect))
               (gdk-rectangle-contains-point allocation x-start y-start)))))
    (si:take-while
     #'visible
     (si:drop-while
      (alex:compose #'not #'visible)
      model))))

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

(defun draw-scheme (widget ctx)
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

    (si:do-iterator (bead (filter-visible ctx allocation (beads-iterator model)))
      (destructuring-bind (rect . color) bead
        (draw-bead ctx rect color)))))

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
        (let ((bead-idx
               (car
                (si:consume-one
                 (si:drop-while
                  (lambda (bead)
                    (destructuring-bind (n rect . color) bead
                      (declare (ignore color)
                               (ignorable n rect))
                      (not
                       (and
                        (<= (rect-x rect) x (+ (rect-x rect) (rect-width rect)))
                        (<= (rect-y rect) y (+ (rect-y rect) (rect-height rect)))))))
                  (si:enumerate (beads-iterator model)))))))
          (when bead-idx
            (g-signal-emit widget "my-bead-clicked" bead-idx)))))))

(defmethod initialize-instance :after ((scheme-area scheme-area) &rest args)
  (declare (ignore args))
  (g-signal-connect scheme-area "draw" #'draw-scheme)
  (g-signal-connect scheme-area "button-press-event" #'button-clicked)
  (gtk-widget-add-events scheme-area '(:button-press-mask)))

(defmethod (setf scheme-area-position) :after (value (scheme-area scheme-area))
  (declare (ignore value))
  (gtk-widget-queue-draw scheme-area))
