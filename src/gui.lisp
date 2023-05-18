(in-package :cl-beads)

(defclass document-window (gtk-window)
  ((callback       :initarg  :callback
                   :initform (error "Specify :callback")
                   :reader   window-callback
                   :documentation "Callback called on window destruction and creation")
   (document-frame :initarg  :document-frame
                   :initform (error "Specify :document-frame")
                   :type     document-frame
                   :reader   window-document-frame))
  (:metaclass gobject-class)
  (:documentation "Class for main window in cl-beads app"))

;; Widget helpers
(defun add-file-filters (dialog &optional (document (make-instance 'universal-document)))
  (let ((supported-formats (supported-formats document)))
    (dolist (format supported-formats)
      (let ((filter (make-instance 'gtk-file-filter)))
        (destructuring-bind (format . description) format
          (gtk-file-filter-add-pattern filter (format->filter format))
          (gtk-file-filter-set-name    filter description))
        (gtk-file-chooser-add-filter dialog filter)))
    (when (> (length supported-formats) 1)
      (let ((filter (make-instance 'gtk-file-filter)))
        (dolist (format supported-formats)
          (gtk-file-filter-add-pattern filter (format->filter (car format))))
        (gtk-file-filter-set-name filter "All supported formats")
        (gtk-file-chooser-add-filter dialog filter)))))

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

(defun make-menu-entry (label &rest args
                        &key (stockp t) (checkboxp nil) &allow-other-keys)
  "Make a menu entry, maybe with stock label"
   (apply
    ;; FIXME: check-menu-item cannot use stock labels,
    ;; gtk-image-menu-item is deprecated
    #'make-instance (if checkboxp 'gtk-check-menu-item 'gtk-image-menu-item)
    :label         label
    :use-underline t
    :use-stock     stockp
    (alex:remove-from-plist args :stockp :checkboxp)))

(defun set-window-title (window)
  "Set window title according to the loaded document."
  (setf (gtk-window-title window)
        (format nil "cl-beads~@[: ~a~]"
                (frame-pathname
                 (window-document-frame window)))))

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
                 "gtk-open"   :ok)))
    (add-file-filters dialog)
    (unwind-protect
        (when (eq (gtk-dialog-run dialog) :ok)
          (let ((pathname (gtk-file-chooser-get-filename dialog)))
            (values (read-document pathname (guess-format pathname))
                    pathname)))
      (gtk-widget-destroy dialog))))

(defun save-dialog (parent)
  "Run 'Save Document' dialog and maybe save the document."
  (let* ((dialog (gtk-file-chooser-dialog-new
                  "Save Document"
                  parent :save
                  "gtk-cancel" :cancel
                  "gtk-save"   :ok))
         (frame (window-document-frame parent))
         (document (frame-document frame)))
    (add-file-filters dialog document)
    (gtk-file-chooser-set-do-overwrite-confirmation dialog t)
    (let ((pathname (frame-pathname frame)))
      (if pathname
          (gtk-file-chooser-set-filename dialog pathname)
          (gtk-file-chooser-set-current-name
           dialog
           (namestring
            (make-pathname :name "Untitled"
                           :type (format->file-type
                                  (caar (supported-formats document))))))))
    (unwind-protect
         (when (eq (gtk-dialog-run dialog) :ok)
           (let ((filename (gtk-file-chooser-get-filename dialog)))
             (write-document document filename (guess-format filename))
             filename))
      (gtk-widget-destroy dialog))))

(defun adjustments-for-scheme-dimensions (document)
  "Make two gtk-adjustment objects for spin buttons which control
width and height of a scheme."
  (labels ((range-big-p (min max)
             (> (- max min) 100))
           (make-adjustment (current min max)
             (make-instance 'gtk-adjustment
                            :value          current
                            :lower          min
                            :upper          max
                            :step-increment (if (range-big-p min max) 10 1)
                            :page-increment (if (range-big-p min max) 50 5)
                            :page-size      0)))
    (let ((bounds (sensible-bounds document)))
      (values
       (make-adjustment
        (document-width document)
        (scheme-bounds-minimal-width bounds)
        (scheme-bounds-maximal-width bounds))
       (make-adjustment
        (document-height document)
        (scheme-bounds-minimal-height bounds)
        (scheme-bounds-maximal-height bounds))))))

(defun settings-dialog (parent)
  "Run settings dialog and update the document if needed."
  (let* ((frame (window-document-frame parent))
         (document (frame-document frame))
         (dialog (gtk-dialog-new-with-buttons
                  "Document settings"
                  parent nil
                  "gtk-ok"     :ok
                  "gtk-cancel" :cancel))
         (outline-color (make-instance 'gtk-color-button
                                       :rgba (color->gdk-rgba
                                              (document-outline-color document))))
         (box (make-instance 'gtk-vbox)))
    (multiple-value-bind (width-adjustment height-adjustment)
        (adjustments-for-scheme-dimensions document)
      (let ((width-button  (make-spin-button width-adjustment))
            (height-button (make-spin-button height-adjustment)))
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
            (setf (frame-dirty-state-p frame) t
                  (document-outline-color document)
                  (gdk-rgba->color (gtk-color-button-rgba outline-color)))
            (update-scheme document width height))
          (gtk-widget-destroy dialog)
          (eq response :ok))))))

(defun quit-dialog (parent)
  "Ask if we should close the window. Return T if we should."
  (let ((dialog (gtk-dialog-new-with-buttons
                 "Are you sure?"
                 parent nil
                 "gtk-ok"     :ok
                 "gtk-cancel" :cancel)))
    (gtk-container-add
     (gtk-dialog-get-content-area dialog)
     (make-instance 'gtk-label
                    :label          "There are unsaved changes. Are you sure you want to quit?"
                    :height-request 40
                    :valign         :center))
    (gtk-widget-show-all dialog)
    (prog1
        (eq (gtk-dialog-run dialog) :ok)
    (gtk-widget-destroy dialog))))

;; Button and menu entries handlers
(defun new-handler (parent widget)
  (declare (ignore widget))
  (open-document (make-instance 'document-rope)
                 (window-callback parent)))

(defun new-ring-handler (parent widget)
  (declare (ignore widget))
  (open-document (make-instance 'document-ring)
                 (window-callback parent)))

(defun open-in-new-window-handler (parent widget)
  (declare (ignore widget))
  (handler-case
      (multiple-value-bind (document pathname)
          (open-dialog parent)
        (when document
          (set-window-title
           (open-document document (window-callback parent) :pathname pathname))))
    ((or invalid-file wrong-format) (c)
      (error-dialog parent "Cannot open a file" c)
      ())))

(defun open-handler (parent widget)
  (when (and (open-in-new-window-handler parent widget)
             (not (frame-dirty-state-p
                   (window-document-frame parent))))
    ;; Close the existing window if an associated document is saved
    (gtk-widget-destroy parent)))

(defun save-as-handler (parent widget)
  (declare (ignore widget))
  (let ((frame (window-document-frame parent))
        (pathname (save-dialog parent)))
    (when pathname
      (setf (frame-pathname      frame) pathname
            (frame-dirty-state-p frame) nil)
      (set-window-title parent))))

(defun save-handler (parent widget)
  (let* ((frame (window-document-frame parent))
         (pathname (frame-pathname frame)))
    (cond
      (pathname
       (write-document (frame-document frame) pathname
                       (guess-format pathname))
       (setf (frame-dirty-state-p frame) nil))
      (t
       (save-as-handler parent widget)))))

(defun safe-save-handler (handler)
  "Invoke a save handler HANDLER which always finishes normally."
  (lambda (parent widget)
    (handler-case
        (funcall handler parent widget)
      ((or output-error wrong-format) (c)
        (error-dialog parent "Save error" c)))))

;; Main stuff
(defun open-document (document window-callback &key pathname)
  "Open a document in a new window. PATHNAME is a path associated with
the document (if exists, i.e. the document is not a new
document). WINDOW-CALLBACK is a callback which is called when the
document's window is created or destroyed."
  (let* ((document-frame (make-document-frame document :pathname pathname))
         (window (make-instance 'document-window
                                :title          "cl-beads"
                                :callback       window-callback
                                :document-frame document-frame
                                :default-width  600
                                :default-height 800))
         (menu-bar       (make-instance 'gtk-menu-bar))
         (workspace-box  (make-preferred-box (preferred-orientation document-frame)))
         (toolbar-box    (make-instance 'gtk-hbox))
         (main-box       (make-instance 'gtk-vbox))
         (current-color  (make-instance 'palette-button :sensitive nil)))

    ;; Handle click on a bead
    (dolist (area (frame-scheme-areas document-frame))
      (g-signal-connect
       area "my-bead-clicked"
       (lambda (widget bead-idx)
         (declare (ignore widget))
         (when (< bead-idx (array-total-size (document-scheme document)))
           ;; How can it be otherwise?
           (symbol-macrolet ((bead-color (row-major-aref (document-scheme document) bead-idx)))
             (ecase (frame-active-tool document-frame)
               (:pencil
                (let ((current-color (document-palette-idx document)))
                  (setf bead-color (if (= bead-color current-color) 0 current-color)))
                ;; TODO: Redraw only a small area
                (redraw-scheme-areas document-frame)
                (setf (frame-dirty-state-p document-frame) t))
               (:line
                (let ((current-color (document-palette-idx document)))
                  (unless (= current-color bead-color)
                    (setf bead-color current-color)
                    ;; TODO: Redraw only a small area
                    (redraw-scheme-areas document-frame)
                    (setf (frame-dirty-state-p document-frame) t))))
               (:color-picker
                (setf (document-palette-idx document) bead-color
                      (palette-button-color current-color)
                      (current-color document)))))))))

    ;; Ask for a confirmation when closing a window in dirty state
    (g-signal-connect
     window "delete-event"
     (lambda (widget event)
       (declare (ignore widget event))
       (when (frame-dirty-state-p document-frame)
         (not (quit-dialog window)))))

    ;; Call WINDOW-CALLBACK when the window is closed
    (g-signal-connect window "destroy" (alex:curry window-callback :close))

    (gtk-box-pack-start workspace-box document-frame :expand nil)

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
       (alex:curry (safe-save-handler #'save-handler) window)))

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
                  (setf (frame-active-tool document-frame) :pencil)
                  (mapc (alex:rcurry #'set-free-drawing nil)
                        (frame-scheme-areas document-frame)))
                 ((eq widget line)
                  (setf (frame-active-tool document-frame) :line)
                  (mapc (alex:rcurry #'set-free-drawing t)
                        (frame-scheme-areas document-frame)))
                 ((eq widget color-picker)
                  (setf (frame-active-tool document-frame) :color-picker))))))))

      (alex:when-let ((simulation-area (simulation-area document-frame)))
        (let ((simulation-model (scheme-area-model simulation-area)))
          (g-signal-connect
           rotate-left "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (decf (simulated-model-rotation simulation-model))
             (gtk-widget-queue-draw simulation-area)))
          (g-signal-connect
           rotate-right "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (incf (simulated-model-rotation simulation-model))
             (gtk-widget-queue-draw simulation-area)))))

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
         (when (settings-dialog window)
           (redraw-scheme-areas document-frame))))

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
              (redraw-scheme-areas document-frame)
              ;; Set dirty state
              (setf (frame-dirty-state-p document-frame) t)))

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
              (redraw-scheme-areas document-frame)
              ;; Set dirty state
              (setf (frame-dirty-state-p document-frame) t)))
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
          (item-file-new-ring (make-menu-entry "New _ring" :stockp nil))
          (item-file-open     (make-menu-entry "gtk-open"))
          (item-file-open-new (make-menu-entry "Open in new _window" :stockp nil))
          (item-file-save     (make-menu-entry "gtk-save"))
          (item-file-save-as  (make-menu-entry "gtk-save-as"))
          (item-file-quit     (make-menu-entry "gtk-quit")))
      (gtk-menu-shell-append submenu item-file-new)
      (gtk-menu-shell-append submenu item-file-new-ring)
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
       item-file-new-ring "activate"
       (alex:curry #'new-ring-handler window))
      (g-signal-connect
       item-file-open "activate"
       (alex:curry #'open-handler window))
      (g-signal-connect
       item-file-open-new "activate"
       (alex:curry #'open-in-new-window-handler window))
      (g-signal-connect
       item-file-save "activate"
       (alex:curry (safe-save-handler #'save-handler) window))
      (g-signal-connect
       item-file-save-as "activate"
       (alex:curry (safe-save-handler #'save-as-handler) window))
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
          (item-edit-undo  (make-menu-entry "gtk-undo" :sensitive nil))
          (item-edit-redo  (make-menu-entry "gtk-redo" :sensitive nil)))
      (gtk-menu-shell-append submenu item-edit-undo)
      (gtk-menu-shell-append submenu item-edit-redo)
      (setf (gtk-menu-item-submenu item-edit) submenu)
      (gtk-menu-shell-append menu-bar item-edit))

    ;; Tools menu
    (alex:when-let ((additional-items (additional-tools document-frame)))
      (let ((item-tools (make-instance 'gtk-menu-item
                                       :label "_Tools"
                                       :use-underline t))
            (submenu (make-instance 'gtk-menu)))
        (dolist (item additional-items)
          (let ((menu-entry (make-menu-entry (menu-item-label item)
                                             :stockp    nil
                                             :checkboxp (menu-item-checkbox-p item))))
            (gtk-menu-shell-append submenu menu-entry)
            (g-signal-connect
             menu-entry "activate"
             (lambda (widget)
               (declare (ignore widget))
               (apply #'funcall (menu-item-callback item)
                      window document-frame
                      (if (menu-item-checkbox-p item)
                          (list (gtk-check-menu-item-active menu-entry))))))))
        (setf (gtk-menu-item-submenu item-tools) submenu)
        (gtk-menu-shell-append menu-bar item-tools)))

    ;; Settings menu
    (let ((item-settings (make-instance 'gtk-menu-item
                                        :label "_Settings"
                                        :use-underline t))
          (submenu (make-instance 'gtk-menu))
          (item-settings-pref (make-menu-entry "gtk-preferences")))
      (g-signal-connect
       item-settings-pref "activate"
       (lambda (widget)
         (declare (ignore widget))
         (when (settings-dialog window)
           (redraw-scheme-areas document-frame))))
      (gtk-menu-shell-append submenu item-settings-pref)
      (setf (gtk-menu-item-submenu item-settings) submenu)
      (gtk-menu-shell-append menu-bar item-settings))

    ;; Pack everyting and call the callback
    (gtk-box-pack-start main-box menu-bar      :expand nil)
    (gtk-box-pack-start main-box toolbar-box   :expand nil)
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
        (open-document (make-instance 'document-rope) #'callback)))))

(defun main ()
  "Entry point for a stand-alone application"
  (run)
  (join-gtk-main))
