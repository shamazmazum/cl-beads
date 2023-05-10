(in-package :cl-beads)

(deftype orientation () '(member :horizontal :vertical))

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

(defgeneric make-document-frame (document &key pathname)
  (:documentation "Make a frame which contains all widgets necessary
to edit the document. Different document types may result in different
frame types."))

(defgeneric simulation-area (frame)
  (:method ((frame document-frame))
    nil)
  (:documentation "Get a scheme area with simulation model (the one
which can be rotated and looks like a finished product) if there is
such area."))

(defgeneric preferred-orientation (frame)
  (:documentation "Preferred orientation of the frame.
Either :HORIZONTAL or :VERTICAL."))

(defun redraw-scheme-areas (document-frame)
  (mapc #'gtk-widget-queue-draw
        (frame-scheme-areas document-frame)))

;; ROPE-FRAME
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

(defmethod simulation-area ((frame rope-frame))
  (third (frame-scheme-areas frame)))

(defmethod make-document-frame ((document document-rope) &key pathname)
  (make-instance 'rope-frame
                 :document document
                 :pathname pathname))

(defmethod preferred-orientation ((frame rope-frame))
  :vertical)

;; RING-FRAME
(defclass ring-frame (document-frame)
  ()
  (:metaclass gobject-class)
  (:documentation "Document frame for a ring in mosaic technique"))

(defmethod initialize-instance :after ((frame ring-frame) &rest initargs)
  (declare (ignore initargs))
  (let ((document (frame-document frame)))
    (setf (frame-scheme-areas frame)
          (list (make-instance 'scheme-area
                               ;; FIXME: Must not specify this by hand
                               :outline-width  1d-3
                               :height-request 400
                               :halign         :fill
                               :hexpand        t
                               :model          (make-instance 'ring-model :document document))))

    ;; Add widgets to the frame box
    ;; The drawing area
    (gtk-container-add frame (first (frame-scheme-areas frame)))))

(defmethod make-document-frame ((document document-ring) &key pathname)
  (make-instance 'ring-frame
                 :document document
                 :pathname pathname))

(defmethod preferred-orientation ((frame ring-frame))
  :horizontal)

(sera:-> make-preferred-box (orientation)
         (values gtk-box &optional))
(defun make-preferred-box (orientation)
  (ecase orientation
    (:vertical   (make-instance 'gtk-hbox))
    (:horizontal (make-instance 'gtk-vbox))))
