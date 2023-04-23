(in-package :cl-beads)

(defun make-stock-button (stock-id)
  (make-instance 'gtk-button
                 :image (gtk-image-new-from-icon-name stock-id :large-toolbar)))

(defun make-menu-entry (stock-id)
  (make-instance 'gtk-image-menu-item
                 :label stock-id
                 :use-underline t
                 :use-stock     t))

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

(defun save-dialog (parent document &optional filename)
  "Run 'Save Document' dialog and maybe save the document. If the
document's pathname is known, it can be passed as an optional
argument."
  (let ((dialog (gtk-file-chooser-dialog-new
                 "Save Document"
                 parent
                 :save
                 "gtk-cancel" :cancel
                 "gtk-save" :accept))
        (filter (make-instance 'gtk-file-filter)))
    (gtk-file-chooser-set-do-overwrite-confirmation dialog t)
    (if filename
        (gtk-file-chooser-set-filename dialog filename)
        (gtk-file-chooser-set-current-name dialog "Untitled.jbb"))
    (gtk-file-filter-add-pattern filter "*.jbb")
    (gtk-file-filter-set-name    filter "JBead file")
    (gtk-file-chooser-add-filter dialog filter)

    (prog1
        (when (eq (gtk-dialog-run dialog) :accept)
          (let ((filename (gtk-file-chooser-get-filename dialog)))
            (write-jbb document filename)
            filename))
      (gtk-widget-destroy dialog))))

(sera:-> settings-dialog
         (gtk-window unsigned-byte unsigned-byte)
         (values unsigned-byte unsigned-byte boolean &optional))
(defun settings-dialog (parent width height)
  "Run settings dialog and return the new width and height of the
scheme. The third returned value is a boolean indicating if settings
were changed."
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
                                        :value          width
                                        :lower          6
                                        :upper          20
                                        :step-increment 1
                                        :page-increment 5
                                        :page-size      0)))
          (height-button (make-spin-button
                          (make-instance 'gtk-adjustment
                                         :value          height
                                         :lower          100
                                         :upper          5000
                                         :step-increment 10
                                         :page-increment 50
                                         :page-size      0)))
          (box (make-instance 'gtk-vbox)))

      (let ((%box (make-instance 'gtk-hbox)))
        (gtk-box-pack-start %box (make-instance 'gtk-label :label "Width"))
        (gtk-box-pack-end   %box width-button)
        (gtk-box-pack-start box %box))
      (let ((%box (make-instance 'gtk-hbox)))
        (gtk-box-pack-start %box (make-instance 'gtk-label :label "Height"))
        (gtk-box-pack-end   %box height-button)
        (gtk-box-pack-start box %box))
      (gtk-dialog-add-button dialog "gtk-ok" :ok)
      (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
      (gtk-box-pack-start (gtk-dialog-get-content-area dialog) box)
      (gtk-widget-show-all dialog)
      (let ((response (gtk-dialog-run dialog))
            (width  (floor (gtk-spin-button-value width-button)))
            (height (floor (gtk-spin-button-value height-button))))
        (gtk-widget-destroy dialog)
      (values width height (eq response :ok))))))

(sera:-> exec-settings-and-update (gtk-window document list)
         (values &optional))
(defun exec-settings-and-update (parent document areas)
  "Run settings dialog and update the document (and redraw
SCHEME-AREAs) if needed."
  (multiple-value-bind (width height changedp)
      (settings-dialog
       parent
       (document-width  document)
       (document-height document))
    (when changedp
      (update-scheme document width height)
      (mapc #'gtk-widget-queue-draw areas)))
  (values))

(defclass document-state ()
  ((pathname    :initarg  :pathname
                :initform nil
                :type     (or null pathname string)
                :accessor state-pathname)
   (active-tool :initform :pencil
                :type     (member :pencil :color-picker)
                :accessor state-active-tool)
   (callback    :initarg  :callback
                :reader   state-callback
                :documentation "Callback called on window destruction and creation")))

(defun set-window-title (window pathname)
  (setf (gtk-window-title window)
        (format nil "cl-beads~@[: ~a~]" pathname)))

(defun new-handler (state widget)
  (declare (ignore widget))
  (open-document (make-instance 'document)
                 (state-callback state)))

(defun open-handler (parent state widget)
  (declare (ignore widget))
  (multiple-value-bind (document pathname)
      (open-dialog parent)
    (when document
      (set-window-title
       (open-document document (state-callback state) :pathname pathname)
       pathname))))

(defun save-as-handler (parent state document widget)
  (declare (ignore widget))
  (let ((pathname (save-dialog
                   parent document
                   (state-pathname state))))
    (when pathname
      (setf (state-pathname state) pathname)
      (set-window-title parent pathname))))

(defun save-handler (parent state document widget)
  (if (state-pathname state)
      (write-jbb document (state-pathname state))
      (save-as-handler parent state document widget)))

(defun open-document (document window-callback &key pathname)
  "Open a document in a new window. PATHNAME is a path associated with
the document (if exists, i.e. the document is not a new
document). WINDOW-CALLBACK is a callback which is called when the
document's window is created or destroyed."
  (let ((window (make-instance 'gtk-window
                               :title          "cl-beads"
                               :default-width  600
                               :default-height 800))

        (menu-bar      (make-instance 'gtk-menu-bar))
        (workspace-box (make-instance 'gtk-hbox))
        (toolbar-box   (make-instance 'gtk-hbox))
        (main-box      (make-instance 'gtk-vbox))

        (scheme-areas
         (list (make-instance 'scheme-area
                              :width-request 200
                              :valign        :fill
                              :vexpand       t
                              :draw-ruler-p  t
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
        (current-color (make-instance 'palette-button :sensitive nil))
        (state (make-instance 'document-state
                              :pathname pathname
                              :callback window-callback)))

    ;; Clicking on a bead handling
    (dolist (area scheme-areas)
      (g-signal-connect
       area "my-bead-clicked"
       (lambda (widget bead-idx)
         (declare (ignore widget))
         (when (< bead-idx (array-total-size (document-scheme document)))
           ;; How can it be otherwise?
           (symbol-macrolet ((bead-color (row-major-aref (document-scheme document) bead-idx)))
             (ecase (state-active-tool state)
               (:pencil
                (let ((current-color (document-palette-idx document)))
                  (setf bead-color (if (= bead-color current-color) 0 current-color)))
                ;; TODO: Redraw only a small area
                (mapc #'gtk-widget-queue-draw scheme-areas))
               (:color-picker
                (setf (document-palette-idx document) bead-color
                      (palette-button-color current-color)
                      (current-color document)))))))))

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
       (alex:curry #'new-handler state))
      (g-signal-connect
       open-button "clicked"
       (alex:curry #'open-handler window state))
      (g-signal-connect
       save-button "clicked"
       (alex:curry #'save-handler window state document)))

    ;; Undo / Redo buttons (currently not functional)
    (let ((edit-box (make-instance 'gtk-hbox))
          (undo-button (make-stock-button "edit-undo"))
          (redo-button (make-stock-button "edit-redo")))
      (gtk-box-pack-start edit-box undo-button :expand nil)
      (gtk-box-pack-start edit-box redo-button :expand nil)
      (gtk-box-pack-start toolbar-box edit-box :expand nil :padding 5))

    ;; Tools (Pencil / Color pick) buttons
    (let* ((tools-box (make-instance 'gtk-hbox))
           (pencil (gtk-radio-button-new nil))
           (color-picker (gtk-radio-button-new-from-widget pencil)))
      (gtk-toggle-button-set-mode pencil       nil)
      (gtk-toggle-button-set-mode color-picker nil)
      (setf (gtk-button-image pencil)
            ;; Converted from this file:
            ;; https://commons.wikimedia.org/wiki/File:Antu_document-edit-sign.svg
            (gtk-image-new-from-file
             (namestring (asdf:system-relative-pathname :cl-beads "pencil.png")))
            (gtk-button-image color-picker)
            (gtk-image-new-from-icon-name "gtk-color-picker" :large-toolbar))

      (dolist (button (list pencil color-picker))
        (g-signal-connect
         button "toggled"
         (lambda (widget)
           (when (gtk-toggle-button-active widget)
             (cond
               ((eq widget pencil)
                (setf (state-active-tool state) :pencil))
               ((eq widget color-picker)
                (setf (state-active-tool state) :color-picker)))))))

      (gtk-box-pack-start tools-box pencil       :expand nil)
      (gtk-box-pack-start tools-box color-picker :expand nil)
      (gtk-box-pack-start toolbar-box tools-box  :expand nil))

    ;; Settings button
    (let ((settings-box (make-instance 'gtk-hbox))
          (pref-button (make-stock-button "gtk-preferences")))

      (g-signal-connect
       pref-button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (exec-settings-and-update window document scheme-areas)))

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
              (mapc #'gtk-widget-queue-draw scheme-areas)))

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
              (mapc #'gtk-widget-queue-draw scheme-areas)))
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
          (item-file-new     (make-menu-entry "gtk-new"))
          (item-file-open    (make-menu-entry "gtk-open"))
          (item-file-save    (make-menu-entry "gtk-save"))
          (item-file-save-as (make-menu-entry "gtk-save-as"))
          (item-file-quit    (make-menu-entry "gtk-quit")))
      (gtk-menu-shell-append submenu item-file-new)
      (gtk-menu-shell-append submenu item-file-open)
      (gtk-menu-shell-append submenu item-file-save)
      (gtk-menu-shell-append submenu item-file-save-as)
      (gtk-menu-shell-append submenu item-file-quit)

      ;; Handlers
      (g-signal-connect
       item-file-new "activate"
       (alex:curry #'new-handler state))
      (g-signal-connect
       item-file-open "activate"
       (alex:curry #'open-handler window state))
      (g-signal-connect
       item-file-save "activate"
       (alex:curry #'save-handler window state document))
      (g-signal-connect
       item-file-save-as "activate"
       (alex:curry #'save-as-handler window state document))
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
         (exec-settings-and-update window document scheme-areas)))

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
