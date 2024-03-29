(in-package :cl-beads)

;; FIXME: Invent a consistency mechanism for the checkbutton
(sera:defconstructor menu-item
  (label      string)
  (checkbox-p boolean)
  (callback   function))

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

(defgeneric preferred-orientation (frame)
  (:documentation "Preferred orientation of the frame.
Either :HORIZONTAL or :VERTICAL."))

(defgeneric additional-tools (frame)
  (:documentation "A list of additional edit tools which are specific
to this document frame and added to the Edit menu.

Each tool is a cons `(name . (lambda (parent-window frame) ...))`.")
  (:method-combination append))

(defun redraw-scheme-areas (document-frame)
  (mapc #'gtk-widget-queue-draw
        (frame-scheme-areas document-frame)))

(defun simulation-area (frame)
  "Get a scheme area with simulation model (the one which can be
rotated and looks like a finished product) if there is such area."
  (find-if (alex:rcurry #'typep 'rotatable-scheme-model)
           (frame-scheme-areas frame)
           :key #'scheme-area-model))

;; Add a reading line menu items if one of the scheme areas support it
(defun find-area-with-reading-line (frame)
  (find-if (alex:rcurry #'typep 'reading-line-mixin)
           (frame-scheme-areas frame)))

(defun set-focus-on-corrected-area (parent frame)
  (declare (ignore parent))
  (gtk-widget-grab-focus
   (find-area-with-reading-line frame)))

(defun reset-reading-line (parent frame)
  (declare (ignore parent))
  (setf (document-reading-line-position (frame-document frame)) 0
        (frame-dirty-state-p frame) t)
  (gtk-widget-queue-draw
   (find-area-with-reading-line frame)))

(defun toggle-reading-line (parent frame active)
  (declare (ignore parent))
  (let ((area (find-area-with-reading-line frame)))
    (setf (scheme-area-show-reading-line-p area) active)
    (gtk-widget-queue-draw area)))

(defmethod additional-tools append ((frame document-frame))
  (alex:when-let ((reading-line-scheme (find-area-with-reading-line frame)))
    (list
     (menu-item "_Reading line" t #'toggle-reading-line)
     (menu-item "Re_set reading line" nil #'reset-reading-line)
     (menu-item "Set _focus on the scheme" nil #'set-focus-on-corrected-area))))

;; Scheme area with reading line mixin
(defclass scheme-area-with-reading-line (scheme-area reading-line-mixin)
  ()
  (:metaclass gobject-class))

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
                                :direct-superclasses (mapcar #'find-class
                                                             '(scheme-area ruler-mixin)))
                               :width-request 200
                               :valign        :fill
                               :vexpand       t
                               :model         (make-instance 'draft-model :document document))
                (make-instance 'scheme-area-with-reading-line
                               :width-request 200
                               :valign        :fill
                               :vexpand       t
                               :orientation   :horizontal
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

(defmethod make-document-frame ((document document-rope) &key pathname)
  (make-instance 'rope-frame
                 :document document
                 :pathname pathname))

(defmethod preferred-orientation ((frame rope-frame))
  :vertical)

(defun clone-dialog (parent)
  "Run dialog which performs cloning of rows"
  (let ((frame (window-document-frame parent)))
    (flet ((%make-spin-button ()
             (make-spin-button
              (make-instance 'gtk-adjustment
                             :value          0
                             :lower          0
                             :upper          (document-height (frame-document frame))
                             :step-increment 1
                             :page-increment 5
                             :page-size      0))))
      (let ((dialog (gtk-dialog-new-with-buttons
                     "Row cloning tool"
                     parent nil
                     "gtk-ok"     :ok
                     "gtk-cancel" :cancel))
            (from-button (%make-spin-button))
            (to-button   (%make-spin-button))
            (box (make-instance 'gtk-vbox)))

        (gtk-box-pack-start
         box (make-instance 'gtk-label
                            :label "Replicate chosen rows infinitely to the top of the scheme.
There is no undo operation yet. Do not forget to save your document before cloning!"))
        (let ((%box (make-instance 'gtk-hbox)))
          (gtk-box-pack-start %box (make-instance 'gtk-label :label "From (including this row):"))
          (gtk-box-pack-end   %box from-button)
          (gtk-box-pack-start  box %box))
        (let ((%box (make-instance 'gtk-hbox)))
          (gtk-box-pack-start %box (make-instance 'gtk-label :label "To (excluding this row):"))
          (gtk-box-pack-end   %box to-button)
          (gtk-box-pack-start  box %box))

        (gtk-container-add (gtk-dialog-get-content-area dialog) box)
        (gtk-widget-show-all dialog)
        (let ((response (gtk-dialog-run dialog))
              (from (floor (gtk-spin-button-value from-button)))
              (to   (floor (gtk-spin-button-value to-button))))
          ;; TODO: Show an error if from >= to
          (when (and (eq response :ok) (< from to))
            (setf (frame-dirty-state-p frame) t)
            (clone-rows-up (frame-document frame) from to))
          (gtk-widget-destroy dialog)
          (eq response :ok))))))

(defun clone-rows (parent frame)
  (let ((draft-area (first (frame-scheme-areas frame))))
    (setf (scheme-area-show-markings-p draft-area) t)
    (gtk-widget-queue-draw draft-area)
    (when (clone-dialog parent)
      (mapc #'gtk-widget-queue-draw (cdr (frame-scheme-areas frame))))
    (setf (scheme-area-show-markings-p draft-area) nil)
    (gtk-widget-queue-draw draft-area)))

(defmethod additional-tools append ((frame rope-frame))
  (list
   (menu-item "_Clone rows" nil #'clone-rows)))

;; RING-FRAME
(defclass ring-frame (document-frame)
  ()
  (:metaclass gobject-class)
  (:documentation "Document frame for a ring in mosaic technique"))

(defmethod initialize-instance :after ((frame ring-frame) &rest initargs)
  (declare (ignore initargs))
  (let ((document (frame-document frame)))
    (setf (frame-scheme-areas frame)
          (list (make-instance 'scheme-area-with-reading-line
                               ;; FIXME: Must not specify this by hand
                               :outline-width  1d-3
                               :height-request 400
                               :halign         :fill
                               :hexpand        t
                               :orientation    :vertical
                               :step-scale     2
                               :model          (make-instance 'ring-model :document document))
                (make-instance 'scheme-area
                               :outline-width  1d-3
                               :height-request 400
                               :halign         :fill
                               :hexpand        t
                               :model          (make-instance 'ring-simulated-model :document document))))

    ;; Add widgets to the frame box
    (let ((box (make-instance 'gtk-vbox)))
      (loop for scheme-area in (frame-scheme-areas frame) do
            (gtk-box-pack-start box scheme-area :expand nil))
      (gtk-container-add frame box))))

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
