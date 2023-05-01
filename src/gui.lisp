(in-package :cl-beads)

(defun make-stock-button (stock-id)
  (make-instance 'gtk-button
                 :image (gtk-image-new-from-icon-name stock-id :large-toolbar)))

(defun make-menu-entry (label &key (stockp t))
  (make-instance 'gtk-image-menu-item
                 :label         label
                 :use-underline t
                 :use-stock     stockp))

(defun open-dialog (parent)
  "Run 'Open Document' dialog and maybe load a document. Return the
document and its pathname."
  (let ((dialog (gtk-file-chooser-dialog-new
                 "Open Document"
                 parent
                 :open
                 "gtk-cancel" :cancel
                 "gtk-open"   :accept))
        (filter (make-instance 'gtk-file-filter)))
    (gtk-file-filter-add-pattern filter "*.jbb")
    (gtk-file-filter-set-name    filter "JBead file")
    (gtk-file-chooser-add-filter dialog filter)
    (let ((pathname
           (prog1
               (when (eq (gtk-dialog-run dialog) :accept)
                 (gtk-file-chooser-get-filename dialog))
             (gtk-widget-destroy dialog))))
      (when pathname
        (handler-case
            (values
             (read-jbb pathname)
             pathname)
          (error (c)
            ;; TODO: Do something
            (princ c)
            nil))))))

(defun save-dialog (parent document)
  "Run 'Save Document' dialog and maybe save the document."
  (let ((dialog (gtk-file-chooser-dialog-new
                 "Save Document"
                 parent
                 :save
                 "gtk-cancel" :cancel
                 "gtk-save" :accept))
        (filter (make-instance 'gtk-file-filter)))
    (gtk-file-chooser-set-do-overwrite-confirmation dialog t)
    (let ((pathname (window-document-pathname parent)))
      (if pathname
          (gtk-file-chooser-set-filename dialog pathname)
          (gtk-file-chooser-set-current-name dialog "Untitled.jbb")))
    (gtk-file-filter-add-pattern filter "*.jbb")
    (gtk-file-filter-set-name    filter "JBead file")
    (gtk-file-chooser-add-filter dialog filter)

    (prog1
        (when (eq (gtk-dialog-run dialog) :accept)
          (let ((filename (gtk-file-chooser-get-filename dialog)))
            (write-jbb document filename)
            filename))
      (gtk-widget-destroy dialog))))

(defun settings-dialog (parent document areas)
"Run settings dialog and update the document (and redraw SCHEME-AREAs)
if needed."
  (flet ((make-spin-button (adjustment)
           (make-instance 'gtk-spin-button
                          :orientation :horizontal
                          :adjustment  adjustment
                          :climb-rate  1
                          :digits      0)))
    (let ((dialog (make-instance 'gtk-dialog
                                 :title "Document settings"
                                 :transient-for parent
                                 :has-separator t))
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
      (gtk-dialog-add-button dialog "gtk-ok" :ok)
      (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
      (gtk-box-pack-start (gtk-dialog-get-content-area dialog) box)
      (gtk-widget-show-all dialog)
      (let ((response (gtk-dialog-run dialog))
            (width  (floor (gtk-spin-button-value width-button)))
            (height (floor (gtk-spin-button-value height-button))))
        (when (eq response :ok)
          (setf (window-dirty-state-p parent) t
                (document-outline-color document)
                (gdk-rgba->color (gtk-color-button-rgba outline-color)))
          (update-scheme document width height)
          (mapc #'gtk-widget-queue-draw areas))
        (gtk-widget-destroy dialog)))))

(defclass document-window (gtk-window)
  ((pathname      :initarg  :pathname
                  :initform nil
                  :type     (or null pathname string)
                  :accessor window-document-pathname)
   (active-tool   :initform :pencil
                  :type     (member :pencil :color-picker)
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

(defun set-window-title (window)
  (setf (gtk-window-title window)
        (format nil "cl-beads~@[: ~a~]"
                (window-document-pathname window))))

(defun new-handler (parent widget)
  (declare (ignore widget))
  (open-document (make-instance 'document)
                 (window-callback parent)))

(defun open-in-new-window-handler (parent widget)
  (declare (ignore widget))
  (multiple-value-bind (document pathname)
      (open-dialog parent)
    (when document
      (set-window-title
       (open-document document (window-callback parent) :pathname pathname)))))

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

(defun quit-dialog (parent)
  "Ask if we should close the window. Return T if we should."
  (let* ((dialog (gtk-dialog-new-with-buttons
                  "Are you sure?"
                  parent
                  nil
                  "gtk-ok" :accept
                  "gtk-cancel" :reject))
         (content-area (gtk-dialog-get-content-area dialog)))
    (gtk-container-add
     content-area
     (make-instance 'gtk-label
                    :label  "There are unsaved changes. Are you sure you want to quit?"))
    (setf (gtk-container-border-width content-area) 12)
    (gtk-widget-show-all content-area)
    (prog1
        (eq (gtk-dialog-run dialog) :accept)
    (gtk-widget-destroy dialog))))

(defclass scheme-area-with-ruler (scheme-area ruler-mixin)
  ()
  (:metaclass gobject-class))

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
         (list (make-instance 'scheme-area-with-ruler
                              :width-request 200
                              :valign        :fill
                              :vexpand       t
                              :model         (make-instance 'draft-model :document document))
               (make-instance 'scheme-area
                              :width-request 200
                              :valign        :fill
                              :vexpand       t
                              :model         (make-instance 'corrected-model :document document))
               ;; FIXME: There are problems with editing using this view
               (make-instance 'scheme-area
                              :width-request 200
                              :valign        :fill
                              :vexpand       t
                              :sensitive     nil
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
       (alex:curry #'save-handler window document)))

    ;; Undo / Redo buttons (currently not functional)
    (let ((edit-box (make-instance 'gtk-hbox))
          (undo-button (make-stock-button "edit-undo"))
          (redo-button (make-stock-button "edit-redo")))
      (gtk-box-pack-start edit-box undo-button :expand nil)
      (gtk-box-pack-start edit-box redo-button :expand nil)
      (gtk-box-pack-start toolbar-box edit-box :expand nil :padding 5))

    ;; Tools (Pencil / Color pick / Rotation) buttons
    (let* ((tools-box (make-instance 'gtk-hbox))
           (pencil (gtk-radio-button-new nil))
           (color-picker (gtk-radio-button-new-from-widget pencil))
           (rotate-left  (make-stock-button "go-previous"))
           (rotate-right (make-stock-button "go-next")))
      (setf (gtk-button-image pencil)
            ;; Converted from this file:
            ;; https://commons.wikimedia.org/wiki/File:Antu_document-edit-sign.svg
            (gtk-image-new-from-file
             (namestring (asdf:system-relative-pathname :cl-beads "pencil.png")))
            (gtk-button-image color-picker)
            (gtk-image-new-from-icon-name "gtk-color-picker" :large-toolbar))

      (dolist (button (list pencil color-picker))
        (gtk-toggle-button-set-mode button nil)
        (g-signal-connect
         button "toggled"
         (lambda (widget)
           (when (gtk-toggle-button-active widget)
             (cond
               ((eq widget pencil)
                (setf (window-active-tool window) :pencil))
               ((eq widget color-picker)
                (setf (window-active-tool window) :color-picker)))))))

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
         (settings-dialog window document scheme-areas)))

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
       (alex:curry #'save-handler window document))
      (g-signal-connect
       item-file-save-as "activate"
       (alex:curry #'save-as-handler window document))
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
          (item-edit-undo (make-menu-entry "gtk-undo"))
          (item-edit-redo (make-menu-entry "gtk-redo")))
      (gtk-menu-shell-append submenu item-edit-undo)
      (gtk-menu-shell-append submenu item-edit-redo)
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
         (settings-dialog window document scheme-areas)))

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
