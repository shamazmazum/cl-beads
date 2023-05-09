(in-package :cl-beads)

(defclass document-frame (gtk-frame)
  ((document      :initarg  :document
                  :initform (error ":document must be specified")
                  :reader   frame-document)
   (pathname      :initarg  :pathname
                  :initform nil
                  :type     (or null pathname string)
                  :accessor frame-pathname)
   (active-tool   :initform :pencil
                  :type     (member :pencil :line :color-picker)
                  :accessor frame-active-tool)
   (dirty-state-p :initform nil
                  :type     boolean
                  :accessor frame-dirty-state-p
                  :documentation "If the document is edited but not saved")
   (scheme-areas  :initarg  :scheme-areas
                  :accessor frame-scheme-areas))
  (:metaclass gobject-class)
  (:documentation "Generic class for a frame with scheme areas"))

(defun redraw-scheme-areas (document-frame)
  (mapc #'gtk-widget-queue-draw
        (frame-scheme-areas document-frame)))

(defclass rope-frame (document-frame)
  ()
  (:metaclass gobject-class)
  (:documentation "Document frame for a beaded rope"))

(defmethod initialize-instance :after ((frame rope-frame) &rest initargs)
  (declare (ignore initargs))
  (let ((document  (frame-document frame))
        (frame-box (make-instance 'gtk-hbox)))
    (setf (frame-scheme-areas frame)
          (list (make-instance (closer-mop:ensure-class
                                'scheme-area-with-ruler
                                :metaclass 'gobject-class
                                :direct-superclasses (mapcar #'find-class '(scheme-area ruler-mixin)))
                               :width-request 200
                               :valign        :fill
                               :vexpand       t
                               :model         (make-instance 'draft-model :document document))
                (make-instance 'scheme-area
                               :width-request 200
                               :valign        :fill
                               :vexpand       t
                               :model         (make-instance 'corrected-model :document document))
                (make-instance 'scheme-area
                               :width-request 200
                               :valign        :fill
                               :vexpand       t
                               :model         (make-instance 'simulated-model :document document))))

    ;; Add widgets to the frame box
    ;; A scrollbar comes first
    (let ((adjustment (make-instance 'gtk-adjustment
                                     :lower 0.0
                                     :value 1.0
                                     :upper 1.0
                                     :step-increment 0.02
                                     :page-increment 0.2)))
      (g-signal-connect
       adjustment "value-changed"
       (lambda (object)
         (declare (ignore object))
         (dolist (area (frame-scheme-areas frame))
           (setf (scheme-area-position area)
                 (- 1 (gtk-adjustment-value adjustment))))))

      (gtk-box-pack-start
       frame-box
       (make-instance 'gtk-scrollbar
                      :orientation :vertical
                      :adjustment  adjustment)
       :expand nil))

    ;; Three drawing areas
    (let ((grid (make-instance 'gtk-grid)))
      (setf (gtk-grid-column-spacing grid) 20)
      (loop for area in (frame-scheme-areas frame)
            for x from 0 by 1 do
            (gtk-grid-attach grid area x 0 1 1))
      (loop for label in '("Draft" "Corrected" "Simulation")
            for x from 0 by 1 do
            (gtk-grid-attach grid (make-instance 'gtk-label :label label)
                             x 1 1 1))
      (gtk-box-pack-start frame-box grid :expand nil))

    ;; Add frame box to the frame
    (gtk-container-add frame frame-box)))
