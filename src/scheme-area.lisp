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
                  :accessor scheme-area-position))
  (:metaclass gobject-class))

(defun draw-bead (ctx rect color)
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

(defun gdk-rectangle-contains-point (rect x y)
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

(defun draw-scheme (widget ctx)
  "Stub for drawing the scheme"
  (let* ((ctx (pointer ctx))
         (allocation (gtk-widget-get-allocation widget))
         (width  (float (gdk-rectangle-width  allocation) 0d0))
         (height (float (gdk-rectangle-height allocation) 0d0))
         (maxy (/ height width))
         (model (scheme-area-model widget))
         (scheme-height (estimate-height model)))
    ;; Fill with background
    (gtk-render-background
     (gtk-widget-get-style-context widget)
     ctx 0 0 width height)
    ;; Set coordinate system
    ;; X spans from 0 to 1
    ;; Y spans from 0 to maxy
    (cairo-translate ctx 0 height)
    (cairo-scale ctx width (- width))
    ;; Setup scrolling offset
    (cairo-translate
     ctx 0
     (- (* (max (- scheme-height maxy) 0)
           (scheme-area-position widget))))
    ;; Set line width
    (cairo-set-line-width ctx (scheme-area-outline-width widget))

    (si:do-iterator (bead (filter-visible ctx allocation (beads-iterator model)))
      (destructuring-bind (rect . color) bead
        (draw-bead ctx rect color)))))
                                        
(defun button-clicked (widget event)
  (declare (ignore widget))
  (format t "X=~f Y=~f Button=~d~%"
          (gdk-event-button-x      event)
          (gdk-event-button-y      event)
          (gdk-event-button-button event)))

(defmethod initialize-instance :after ((scheme-area scheme-area) &rest args)
  (declare (ignore args))
  (g-signal-connect scheme-area "draw" #'draw-scheme)
  (g-signal-connect scheme-area "button-press-event" #'button-clicked)
  (gtk-widget-add-events scheme-area '(:button-press-mask)))

(defmethod (setf scheme-area-position) :after (value (scheme-area scheme-area))
  (declare (ignore value))
  (gtk-widget-queue-draw scheme-area))
