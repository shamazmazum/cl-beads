(in-package :cl-beads)

(defclass palette-button (gtk-button)
  ((color        :initarg  :color
                 :initform (color 1d0 1d0 1d0)
                 :type     color
                 :accessor palette-button-color)
   (css-provider :initform (gtk-css-provider-new)
                 :reader palette-button-css-provider))
  (:metaclass gobject-class))

(sera:-> button-css (color)
         (values string &optional))
(defun button-css (color)
  (let ((css-color (format nil "#~@{~2,'0x~}"
                           (floor (* 255 (color-r color)))
                           (floor (* 255 (color-g color)))
                           (floor (* 255 (color-b color))))))
    (cl-css:css `((button :background ,css-color)))))

(sera:-> update-css (palette-button) t)
(defun update-css (button)
  (gtk-css-provider-load-from-data
   (palette-button-css-provider button)
   (button-css (palette-button-color button))))

(sera:-> choose-color (palette-button) t)
(defun choose-color (button)
  (let* ((color-chooser (make-instance 'gtk-color-chooser-dialog
                                       :title "Pick a color"
                                       :rgba (color->gdk-rgba (palette-button-color button))
                                       :use-alpha nil))
         (response (gtk-dialog-run color-chooser)))
    (when (eq response :ok)
      (let ((color (gdk-rgba->color (gtk-color-chooser-get-rgba color-chooser))))
        (setf (palette-button-color button) color)
        (g-signal-emit button "my-color-set")))
    (gtk-widget-destroy color-chooser)))

(defmethod initialize-instance :after ((button palette-button) &rest args)
  (declare (ignore args))
  (let ((context (gtk-widget-get-style-context button)))
    (gtk-style-context-add-provider
     context (palette-button-css-provider button)
     +gtk-style-provider-priority-user+))
  (update-css button)

  ;; Color choser dialog
  (g-signal-connect
   button "button-press-event"
   (lambda (widget event)
     (declare (ignore widget))
     ;; FIXME: Is there any enum for it?
     (when (= (gdk-event-button-button event) 3)
       (let ((context-menu (gtk-menu-new))
             (choose-color   (make-instance 'gtk-menu-item :label "Choose color"))
             (set-background (make-instance 'gtk-menu-item :label "Set as background")))

         (g-signal-connect
          choose-color "activate"
          (lambda (widget)
            (declare (ignore widget))
            (choose-color button)))
         (g-signal-connect
          set-background "activate"
          (lambda (widget)
            (declare (ignore widget))
            (g-signal-emit button "my-background-change-request")))

         (gtk-menu-shell-append context-menu choose-color)
         (gtk-menu-shell-append context-menu set-background)
         (gtk-widget-show-all context-menu)
         (gtk-menu-popup context-menu
                         :button (gdk-event-button-button event)
                         :activate-time (gdk-event-get-time event)))))))

(defmethod (setf palette-button-color) :after (value (button palette-button))
  (declare (ignore value))
  (update-css button))

(flet ((trivial-new-signal (name)
         (g-signal-newv
          name
          +g-type-object+
          '(:run-last)
          (cffi:null-pointer)
          (cffi:null-pointer)
          (cffi:null-pointer)
          (cffi:null-pointer)
          +g-type-none+ 0
          (cffi:null-pointer))))
  (mapc #'trivial-new-signal
        '("my-color-set" "my-background-change-request")))
