(in-package :cl-beads)

(defclass document-window (gtk-window)
  ((pathname      :initarg  :pathname
                  :initform nil
                  :type     (or null pathname string)
                  :accessor window-document-pathname)
   (active-tool   :initform :pencil
                  :type     (member :pencil :line :color-picker)
                  :accessor window-active-tool)
   (dirty-state-p :initform nil
                  :type     boolean
                  :accessor window-dirty-state-p
                  :documentation "If the document is edited but not saved")
   (callback      :initarg  :callback
                  :reader   window-callback
                  :documentation "Callback called on window destruction and creation"))
  (:metaclass gobject-class)
  (:documentation "GTK window for editing crochet rope models"))

;; Widget helpers
(defun make-spin-button (adjustment)
  "Make a spin button for dialogs"
  (make-instance 'gtk-spin-button
                 :orientation :horizontal
                 :adjustment  adjustment
                 :climb-rate  1
                 :digits      0))

(defun make-stock-button (stock-id &rest args)
  "Make a button with stock image"
  (apply #'make-instance 'gtk-button
         :image (gtk-image-new-from-icon-name stock-id :large-toolbar)
         args))

(defun make-menu-entry (label &rest args &key (stockp t) &allow-other-keys)
  "Make a menu entry, maybe with stock label"
  (let ((args (alex:remove-from-plist args :stockp)))
    (apply
     #'make-instance 'gtk-image-menu-item
     :label         label
     :use-underline t
     :use-stock     stockp
     args)))

(defun set-window-title (window)
  "Set window title according to the loaded document."
  (setf (gtk-window-title window)
        (format nil "cl-beads~@[: ~a~]"
                (window-document-pathname window))))

;; Dialogs
(defun error-dialog (parent situation condition)
  "Print a report about abnormal operation. SITUATION is a string
which descripts a situation. CONDITION is printed in addition to
SITUATION."
  (let ((dialog (gtk-dialog-new-with-buttons "Error" parent nil
                                             "gtk-ok" :none))
        (box (make-instance 'gtk-vbox)))
    (gtk-container-add (gtk-dialog-get-content-area dialog) box)
    (gtk-box-pack-start box (make-instance 'gtk-label :label situation))
    (gtk-box-pack-start box (make-instance 'gtk-label
                                           :label (with-output-to-string (out)
                                                    (princ condition out))))
    (gtk-widget-show-all dialog)
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))

(defun open-dialog (parent)
  "Run 'Open Document' dialog and maybe load a document. Return the
document and its pathname."
  (let ((dialog (gtk-file-chooser-dialog-new
                 "Open Document"
                 parent :open
                 "gtk-cancel" :cancel
                 "gtk-open"   :ok))
        (filter (make-instance 'gtk-file-filter)))
    (gtk-file-filter-add-pattern filter "*.jbb")
    (gtk-file-filter-set-name    filter "JBead file")
    (gtk-file-chooser-add-filter dialog filter)
    (unwind-protect
        (when (eq (gtk-dialog-run dialog) :ok)
          (let ((pathname (gtk-file-chooser-get-filename dialog)))
            (values (read-jbb pathname) pathname)))
      (gtk-widget-destroy dialog))))

(defun save-dialog (parent document)
  "Run 'Save Document' dialog and maybe save the document."
  (let ((dialog (gtk-file-chooser-dialog-new
                 "Save Document"
                 parent :save
                 "gtk-cancel" :cancel
                 "gtk-save"   :ok))
        (filter (make-instance 'gtk-file-filter)))
    (gtk-file-chooser-set-do-overwrite-confirmation dialog t)
    (let ((pathname (window-document-pathname parent)))
      (if pathname
          (gtk-file-chooser-set-filename dialog pathname)
          (gtk-file-chooser-set-current-name dialog "Untitled.jbb")))
    (gtk-file-filter-add-pattern filter "*.jbb")
    (gtk-file-filter-set-name    filter "JBead file")
    (gtk-file-chooser-add-filter dialog filter)
    (unwind-protect
        (when (eq (gtk-dialog-run dialog) :ok)
          (let ((filename (gtk-file-chooser-get-filename dialog)))
            (write-jbb document filename)
            filename))
      (gtk-widget-destroy dialog))))

(defun clone-dialog (parent document)
  "Run dialog which performs cloning of rows"
  (flet ((%make-spin-button ()
           (make-spin-button
            (make-instance 'gtk-adjustment
                           :value          0
                           :lower          0
                           :upper          (document-height document)
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
          (setf (window-dirty-state-p parent) t)
          (clone-rows-up document from to))
      (gtk-widget-destroy dialog)
      (eq response :ok)))))

(defun settings-dialog (parent document)
  "Run settings dialog and update the document if needed."
  (let ((dialog (gtk-dialog-new-with-buttons
                 "Document settings"
                 parent nil
                 "gtk-ok"     :ok
                 "gtk-cancel" :cancel))
        (width-button (make-spin-button
                       (make-instance 'gtk-adjustment
                                      :value          (document-width document)
                                      :lower          6
                                      :upper          20
                                      :step-increment 1
                                      :page-increment 5
                                      :page-size      0)))
        (height-button (make-spin-button
                        (make-instance 'gtk-adjustment
                                       :value          (document-height document)
                                       :lower          100
                                       :upper          5000
                                       :step-increment 10
                                       :page-increment 50
                                       :page-size      0)))
        (outline-color (make-instance 'gtk-color-button
                                      :rgba (color->gdk-rgba
                                             (document-outline-color document))))
        (box (make-instance 'gtk-vbox)))

    (let ((%box (make-instance 'gtk-hbox)))
      (gtk-box-pack-start %box (make-instance 'gtk-label :label "Width"))
      (gtk-box-pack-end   %box width-button)
      (gtk-box-pack-start box %box))
    (let ((%box (make-instance 'gtk-hbox)))
      (gtk-box-pack-start %box (make-instance 'gtk-label :label "Height"))
      (gtk-box-pack-end   %box height-button)
      (gtk-box-pack-start box %box))
    (let ((%box (make-instance 'gtk-hbox)))
      (gtk-box-pack-start %box (make-instance 'gtk-label :label "Bead outline color"))
      (gtk-box-pack-end   %box outline-color)
      (gtk-box-pack-start box %box))
    (gtk-container-add (gtk-dialog-get-content-area dialog) box)
    (gtk-widget-show-all dialog)
    (let ((response (gtk-dialog-run dialog))
          (width  (floor (gtk-spin-button-value width-button)))
          (height (floor (gtk-spin-button-value height-button))))
      (when (eq response :ok)
        (setf (window-dirty-state-p parent) t
              (document-outline-color document)
              (gdk-rgba->color (gtk-color-button-rgba outline-color)))
        (update-scheme document width height))
      (gtk-widget-destroy dialog)
      (eq response :ok))))

(defun quit-dialog (parent)
  "Ask if we should close the window. Return T if we should."
  (let* ((dialog (gtk-dialog-new-with-buttons
                  "Are you sure?"
                  parent nil
                  "gtk-ok"     :ok
                  "gtk-cancel" :cancel))
         (content-area (gtk-dialog-get-content-area dialog)))
    (gtk-container-add
     content-area
     (make-instance 'gtk-label
                    :label  "There are unsaved changes. Are you sure you want to quit?"))
    (setf (gtk-container-border-width content-area) 12)
    (gtk-widget-show-all content-area)
    (prog1
        (eq (gtk-dialog-run dialog) :ok)
    (gtk-widget-destroy dialog))))

;; Button and menu entries handlers
(defun new-handler (parent widget)
  (declare (ignore widget))
  (open-document (make-instance 'document)
                 (window-callback parent)))

(defun open-in-new-window-handler (parent widget)
  (declare (ignore widget))
  (handler-case
      (multiple-value-bind (document pathname)
          (open-dialog parent)
        (when document
          (set-window-title
           (open-document document (window-callback parent) :pathname pathname))))
    (invalid-file (c)
      (error-dialog parent "Cannot open a file" c)
      ())))

(defun open-handler (parent widget)
  (when (and (open-in-new-window-handler parent widget)
             (not (window-dirty-state-p parent)))
    ;; Close the existing window if an associated document is saved
    (gtk-widget-destroy parent)))

(defun save-as-handler (parent document widget)
  (declare (ignore widget))
  (let ((pathname (save-dialog parent document)))
    (when pathname
      (setf (window-document-pathname parent) pathname
            (window-dirty-state-p     parent) nil)
      (set-window-title parent))))

(defun save-handler (parent document widget)
  (cond
    ((window-document-pathname parent)
     (write-jbb document (window-document-pathname parent))
     (setf (window-dirty-state-p parent) nil))
    (t
     (save-as-handler parent document widget))))

(defun safe-save-handler (handler)
  "Invoke a save handler HANDLER which always finishes normally."
  (lambda (parent document widget)
    (handler-case
        (funcall handler parent document widget)
      (output-error (c)
        (error-dialog parent "Save error" c)))))

;; Main stuff
(defun open-document (document window-callback &key pathname)
  "Open a document in a new window. PATHNAME is a path associated with
the document (if exists, i.e. the document is not a new
document). WINDOW-CALLBACK is a callback which is called when the
document's window is created or destroyed."
  (let ((window (make-instance 'document-window
                               :title          "cl-beads"
                               :default-width  600
                               :default-height 800
                               :pathname pathname
                               :callback window-callback))

        (menu-bar      (make-instance 'gtk-menu-bar))
        (workspace-box (make-instance 'gtk-hbox))
        (toolbar-box   (make-instance 'gtk-hbox))
        (main-box      (make-instance 'gtk-vbox))

        (scheme-areas
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
        (current-color (make-instance 'palette-button :sensitive nil)))

    ;; Handle click on a bead
    (dolist (area scheme-areas)
      (g-signal-connect
       area "my-bead-clicked"
       (lambda (widget bead-idx)
         (declare (ignore widget))
         (when (< bead-idx (array-total-size (document-scheme document)))
           ;; How can it be otherwise?
           (symbol-macrolet ((bead-color (row-major-aref (document-scheme document) bead-idx)))
             (ecase (window-active-tool window)
               (:pencil
                (let ((current-color (document-palette-idx document)))
                  (setf bead-color (if (= bead-color current-color) 0 current-color)))
                ;; TODO: Redraw only a small area
                (mapc #'gtk-widget-queue-draw scheme-areas)
                (setf (window-dirty-state-p window) t))
               (:line
                (let ((current-color (document-palette-idx document)))
                  (unless (= current-color bead-color)
                    (setf bead-color current-color)
                    ;; TODO: Redraw only a small area
                    (mapc #'gtk-widget-queue-draw scheme-areas)
                    (setf (window-dirty-state-p window) t))))
               (:color-picker
                (setf (document-palette-idx document) bead-color
                      (palette-button-color current-color)
                      (current-color document)))))))))

    ;; Ask for a confirmation when closing a window in dirty state
    (g-signal-connect
     window "delete-event"
     (lambda (widget event)
       (declare (ignore widget event))
       (when (window-dirty-state-p window)
         (not (quit-dialog window)))))

    ;; Call WINDOW-CALLBACK when the window is closed
    (g-signal-connect window "destroy" (alex:curry window-callback :close))

    ;; A scroll bar
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
         (dolist (area scheme-areas)
           (setf (scheme-area-position area)
                 (- 1 (gtk-adjustment-value adjustment))))))

      (gtk-box-pack-start
       workspace-box
       (make-instance 'gtk-scrollbar
                      :orientation :vertical
                      :adjustment  adjustment)
       :expand nil))

    ;; A frame with three drawing areas
    (let ((frame-grid    (make-instance 'gtk-grid))
          (frame         (make-instance 'gtk-frame)))
      (setf (gtk-grid-column-spacing frame-grid) 20)
      (loop for area in scheme-areas
            for x from 0 by 1 do
            (gtk-grid-attach frame-grid area x 0 1 1))
      (loop for label in '("Draft" "Corrected" "Simulation")
            for x from 0 by 1 do
            (gtk-grid-attach frame-grid (make-instance 'gtk-label :label label)
                             x 1 1 1))
      (gtk-container-add frame frame-grid)
      (gtk-box-pack-start workspace-box frame :expand nil))

    ;; New / Open / Save buttons 
    (let ((document-box (make-instance 'gtk-hbox))
          (new-button  (make-stock-button "document-new"))
          (open-button (make-stock-button "document-open"))
          (save-button (make-stock-button "document-save")))
      (gtk-box-pack-start document-box new-button  :expand nil)
      (gtk-box-pack-start document-box open-button :expand nil)
      (gtk-box-pack-start document-box save-button :expand nil)
      (gtk-box-pack-start toolbar-box document-box :expand nil :padding 5)

      (g-signal-connect
       new-button "clicked"
       (alex:curry #'new-handler window))
      (g-signal-connect
       open-button "clicked"
       (alex:curry #'open-handler window))
      (g-signal-connect
       save-button "clicked"
       (alex:curry (safe-save-handler #'save-handler) window document)))

    ;; Undo / Redo buttons (currently not functional)
    (let ((edit-box (make-instance 'gtk-hbox))
          (undo-button (make-stock-button "edit-undo" :sensitive nil))
          (redo-button (make-stock-button "edit-redo" :sensitive nil)))
      (gtk-box-pack-start edit-box undo-button :expand nil)
      (gtk-box-pack-start edit-box redo-button :expand nil)
      (gtk-box-pack-start toolbar-box edit-box :expand nil :padding 5))

    ;; Tools (Pencil / Color pick / Rotation) buttons
    (let* ((tools-box    (make-instance 'gtk-hbox))
           (pencil       (gtk-radio-button-new nil))
           (line         (gtk-radio-button-new-from-widget pencil))
           (color-picker (gtk-radio-button-new-from-widget pencil))
           (rotate-left  (make-stock-button "go-previous"))
           (rotate-right (make-stock-button "go-next")))
      (setf (gtk-button-image pencil)
            (gtk-image-new-from-pixbuf (gdk-pixbuf-new-from-data *pencil-icon*))
            (gtk-button-image line)
            (gtk-image-new-from-pixbuf (gdk-pixbuf-new-from-data *line-icon*))
            (gtk-button-image color-picker)
            (gtk-image-new-from-icon-name "gtk-color-picker" :large-toolbar))

      (flet ((set-free-drawing (area enabledp)
               (setf (scheme-area-free-drawing-p area) enabledp)))
        (dolist (button (list pencil line color-picker))
          (gtk-toggle-button-set-mode button nil)
          (g-signal-connect
           button "toggled"
           (lambda (widget)
             (when (gtk-toggle-button-active widget)
               (cond
                 ((eq widget pencil)
                  (setf (window-active-tool window) :pencil)
                  (mapc (alex:rcurry #'set-free-drawing nil) scheme-areas))
                 ((eq widget line)
                  (setf (window-active-tool window) :line)
                  (mapc (alex:rcurry #'set-free-drawing t) scheme-areas))
                 ((eq widget color-picker)
                  (setf (window-active-tool window) :color-picker))))))))

      (let* ((simulated-area (third scheme-areas))
             (simulated-model (scheme-area-model simulated-area)))
        (g-signal-connect
         rotate-left "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (decf (simulated-model-rotation simulated-model))
           (gtk-widget-queue-draw simulated-area)))
        (g-signal-connect
         rotate-right "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (incf (simulated-model-rotation simulated-model))
           (gtk-widget-queue-draw simulated-area))))

      (gtk-box-pack-start tools-box pencil       :expand nil)
      (gtk-box-pack-start tools-box line         :expand nil)
      (gtk-box-pack-start tools-box color-picker :expand nil)
      (gtk-box-pack-start tools-box rotate-left  :expand nil)
      (gtk-box-pack-start tools-box rotate-right :expand nil)
      (gtk-box-pack-start toolbar-box tools-box  :expand nil))

    ;; Settings button
    (let ((settings-box (make-instance 'gtk-hbox))
          (pref-button (make-stock-button "gtk-preferences")))

      (g-signal-connect
       pref-button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (when (settings-dialog window document)
           (mapc #'gtk-widget-queue-draw scheme-areas))))

      (gtk-box-pack-start settings-box pref-button :expand nil)
      (gtk-box-pack-start toolbar-box settings-box :expand nil :padding 5))

    ;; Palette
    (let ((frame (make-instance 'gtk-frame))
          (grid  (make-instance 'gtk-grid
                                :row-homogeneous    t
                                :column-homogeneous t
                                :valign             :end
                                :halign             :end)))
      (si:foldl
       (lambda (background-button n)
         (let* ((color-button (make-instance 'palette-button
                                             :color (palette-color document n)))
                (background-button (or background-button color-button)))
           (multiple-value-bind (q r)
               (floor n 2)
             (gtk-grid-attach grid color-button q r 1 1))

           (g-signal-connect
            color-button "clicked"
            (lambda (widget)
              (declare (ignore widget))
              (setf (document-palette-idx document) n
                    (palette-button-color current-color)
                    (current-color document))))

           (g-signal-connect
            color-button "my-background-change-request"
            (lambda (widget)
              (declare (ignore widget))
              ;; Swap colors in the palette
              (psetf (palette-color document n)
                     (palette-color document 0)
                     (palette-color document 0)
                     (palette-color document n))
              ;; Redraw buttons
              (psetf (palette-button-color color-button)
                     (palette-button-color background-button)
                     (palette-button-color background-button)
                     (palette-button-color color-button))
              ;; Redraw areas
              (mapc #'gtk-widget-queue-draw scheme-areas)
              ;; Set dirty state
              (setf (window-dirty-state-p window) t)))

           (g-signal-connect
            color-button "my-color-set"
            (lambda (widget)
              (declare (ignore widget))
              (setf (palette-color document n)
                    (palette-button-color color-button))
              (when (= (document-palette-idx document) n)
                (setf (palette-button-color current-color)
                      (current-color document)))
              ;; Redraw areas
              (mapc #'gtk-widget-queue-draw scheme-areas)
              ;; Set dirty state
              (setf (window-dirty-state-p window) t)))
           background-button))
       nil (si:range 0 (palette-length document)))

      (setf (palette-button-color current-color)
            (current-color document))
      (gtk-box-pack-end toolbar-box current-color :expand nil)
      (gtk-container-add frame grid)
      (gtk-box-pack-end workspace-box frame))

    ;; Menu bar
    ;; File menu
    (let ((item-file (make-instance 'gtk-menu-item
                                    :label "_File"
                                    :use-underline t))
          (submenu (make-instance 'gtk-menu))
          (item-file-new      (make-menu-entry "gtk-new"))
          (item-file-open     (make-menu-entry "gtk-open"))
          (item-file-open-new (make-menu-entry "Open in new _window" :stockp nil))
          (item-file-save     (make-menu-entry "gtk-save"))
          (item-file-save-as  (make-menu-entry "gtk-save-as"))
          (item-file-quit     (make-menu-entry "gtk-quit")))
      (gtk-menu-shell-append submenu item-file-new)
      (gtk-menu-shell-append submenu item-file-open)
      (gtk-menu-shell-append submenu item-file-open-new)
      (gtk-menu-shell-append submenu item-file-save)
      (gtk-menu-shell-append submenu item-file-save-as)
      (gtk-menu-shell-append submenu item-file-quit)

      ;; Handlers
      (g-signal-connect
       item-file-new "activate"
       (alex:curry #'new-handler window))
      (g-signal-connect
       item-file-open "activate"
       (alex:curry #'open-handler window))
      (g-signal-connect
       item-file-open-new "activate"
       (alex:curry #'open-in-new-window-handler window))
      (g-signal-connect
       item-file-save "activate"
       (alex:curry (safe-save-handler #'save-handler) window document))
      (g-signal-connect
       item-file-save-as "activate"
       (alex:curry (safe-save-handler #'save-as-handler) window document))
      (g-signal-connect
       item-file-quit "activate"
       (lambda (widget)
         (declare (ignore widget))
         (gtk-window-close window)))

      (setf (gtk-menu-item-submenu item-file) submenu)
      (gtk-menu-shell-append menu-bar item-file))

    ;; Edit menu
    (let ((item-edit (make-instance 'gtk-menu-item
                                    :label "_Edit"
                                    :use-underline t))
          (submenu (make-instance 'gtk-menu))
          (item-edit-undo  (make-menu-entry "gtk-undo"    :sensitive nil))
          (item-edit-redo  (make-menu-entry "gtk-redo"    :sensitive nil))
          (item-edit-clone (make-menu-entry "_Clone rows" :stockp nil)))

      (g-signal-connect
       item-edit-clone "activate"
       (lambda (widget)
         (declare (ignore widget))
         (let ((draft-area (first scheme-areas)))
           (setf (scheme-area-show-markings-p draft-area) t)
           (gtk-widget-queue-draw draft-area)
           (when (clone-dialog window document)
             (mapc #'gtk-widget-queue-draw (cdr scheme-areas)))
           (setf (scheme-area-show-markings-p draft-area) nil)
           (gtk-widget-queue-draw draft-area))))

      (gtk-menu-shell-append submenu item-edit-undo)
      (gtk-menu-shell-append submenu item-edit-redo)
      (gtk-menu-shell-append submenu item-edit-clone)
      (setf (gtk-menu-item-submenu item-edit) submenu)
      (gtk-menu-shell-append menu-bar item-edit))

    ;; Setting menu
    (let ((item-settings (make-instance 'gtk-menu-item
                                        :label "_Settings"
                                        :use-underline t))
          (submenu (make-instance 'gtk-menu))
          (item-settings-pref (make-menu-entry "gtk-preferences")))

      (g-signal-connect
       item-settings-pref "activate"
       (lambda (widget)
         (declare (ignore widget))
         (when (settings-dialog window document)
           (mapc #'gtk-widget-queue-draw scheme-areas))))

      (gtk-menu-shell-append submenu item-settings-pref)
      (setf (gtk-menu-item-submenu item-settings) submenu)
      (gtk-menu-shell-append menu-bar item-settings))

    ;; Pack everyting and call the callback
    (gtk-box-pack-start main-box menu-bar :expand nil)
    (gtk-box-pack-start main-box toolbar-box :expand nil)
    (gtk-box-pack-end   main-box workspace-box :padding 2)

    (gtk-container-add window main-box)
    (funcall window-callback :open window)
    window))

(defun run ()
  "Run cl-beads. Use this function from the REPL."
  (add-new-signals-once)
  (within-main-loop
    (let (windows)
      (flet ((callback (action window)
               (ecase action
                 (:close
                  (setq windows (remove window windows))
                  (unless windows
                    (leave-gtk-main)))
                 (:open
                  (push window windows)
                  (gtk-widget-show-all window)))))
        (open-document (make-instance 'document) #'callback)))))

(defun main ()
  "Entry point for a stand-alone application"
  (run)
  (join-gtk-main))
