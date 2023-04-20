(in-package :cl-beads)

(defun make-stock-button (stock-id)
  (make-instance 'gtk-button
                 :image (gtk-image-new-from-icon-name stock-id 12)))

(defun make-menu-entry (stock-id)
  (make-instance 'gtk-image-menu-item
                 :label stock-id
                 :use-underline t
                 :use-stock     t))

(defun update-current-color (color-button document)
  (setf (palette-button-color color-button)
        (current-color document)))

(sera:-> settings-dialog
         (gtk-window unsigned-byte unsigned-byte)
         (values unsigned-byte unsigned-byte boolean &optional))
(defun settings-dialog (parent width height)
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

(sera:-> exec-settings-and-update (gtk-window document &rest scheme-area)
         (values &optional))
(defun exec-settings-and-update (parent document &rest areas)
  (multiple-value-bind (width height changedp)
      (settings-dialog
       parent
       (document-width  document)
       (document-height document))
    (when changedp
      (update-scheme document width height)
      (mapc #'gtk-widget-queue-draw areas)))
  (values))

(defun run ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title          "cl-beads"
                                  :default-width  600
                                  :default-height 800))


           (menu-bar      (make-instance 'gtk-menu-bar))
           (workspace-box (make-instance 'gtk-hbox))
           (toolbar-box   (make-instance 'gtk-hbox))
           (main-box      (make-instance 'gtk-vbox))

           (document (make-instance 'document))
           (draft-area      (make-instance 'scheme-area
                                           :width-request 160
                                           :model (make-instance 'draft-model :document document)))
           (corrected-area  (make-instance 'scheme-area
                                           :width-request 160
                                           :model (make-instance 'corrected-model :document document)))
           (simulation-area (make-instance 'scheme-area
                                           :width-request 160
                                           :model (make-instance 'dummy-model :document document)))
           (all-areas (list draft-area corrected-area simulation-area)))

      (dolist (area all-areas)
        (g-signal-connect
         area "my-bead-clicked"
         (lambda (widget bead-idx)
           (declare (ignore widget))
           (when (< bead-idx (array-total-size (document-scheme document)))
             ;; How can it be otherwise?
             (let ((current-color (document-palette-idx document)))
               (symbol-macrolet ((bead (row-major-aref (document-scheme document) bead-idx)))
                 (setf bead (if (= bead current-color) 0 current-color))))
             ;; TODO: Redraw only a small area
             (mapc #'gtk-widget-queue-draw all-areas)))))

      (g-signal-connect
       window "destroy"
       (lambda (widget)
         (declare (ignore widget))
         (leave-gtk-main)))

      (let ((frame-box     (make-instance 'gtk-vbox))
            (frame         (make-instance 'gtk-frame :width-request 600)))
        (let ((drawing-box (make-instance 'gtk-hbox)))
          (gtk-box-pack-start drawing-box draft-area      :padding 20)
          (gtk-box-pack-start drawing-box corrected-area  :padding 20)
          (gtk-box-pack-start drawing-box simulation-area :padding 20)
          (gtk-box-pack-start frame-box drawing-box))
        (let ((label-box (make-instance 'gtk-hbox)))
          (gtk-box-pack-start
           label-box (make-instance 'gtk-label :label "Draft")      :padding 20)
          (gtk-box-pack-start
           label-box (make-instance 'gtk-label :label "Corrected")  :padding 20)
          (gtk-box-pack-start
           label-box (make-instance 'gtk-label :label "Simulation") :padding 20)
          (gtk-box-pack-end frame-box label-box :expand nil))
        (gtk-container-add frame frame-box)

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
             (dolist (area all-areas)
               (setf (scheme-area-position area)
                     (- 1 (gtk-adjustment-value adjustment))))))

          (gtk-box-pack-start
           workspace-box
           (make-instance 'gtk-scrollbar
                          :orientation :vertical
                          :adjustment  adjustment)
           :expand nil))
        (gtk-box-pack-start workspace-box frame    :expand nil))

      (let ((document-box (make-instance 'gtk-hbox))
            (new-button  (make-stock-button "document-new"))
            (open-button (make-stock-button "document-open"))
            (save-button (make-stock-button "document-save")))
        (gtk-box-pack-start document-box new-button  :expand nil)
        (gtk-box-pack-start document-box open-button :expand nil)
        (gtk-box-pack-start document-box save-button :expand nil)
        (gtk-box-pack-start toolbar-box document-box :expand nil :padding 5))

      (let ((edit-box (make-instance 'gtk-hbox))
            (undo-button (make-stock-button "edit-undo"))
            (redo-button (make-stock-button "edit-redo")))
        (gtk-box-pack-start edit-box undo-button :expand nil)
        (gtk-box-pack-start edit-box redo-button :expand nil)
        (gtk-box-pack-start toolbar-box edit-box :expand nil :padding 5))

      (let ((settings-box (make-instance 'gtk-hbox))
            (pref-button (make-stock-button "gtk-preferences")))

        (g-signal-connect
         pref-button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (exec-settings-and-update
            window document
            draft-area corrected-area simulation-area)))

        (gtk-box-pack-start settings-box pref-button :expand nil)
        (gtk-box-pack-start toolbar-box settings-box :expand nil :padding 5))

      (let ((frame         (make-instance 'gtk-frame))
            (current-color (make-instance 'palette-button :sensitive nil))
            (grid          (make-instance 'gtk-grid
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
                (setf (document-palette-idx document) n)
                (update-current-color current-color document)))

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
                (mapc #'gtk-widget-queue-draw all-areas)))

             (g-signal-connect
              color-button "my-color-set"
              (lambda (widget)
                (declare (ignore widget))
                (setf (palette-color document n)
                      (palette-button-color color-button))
                (when (= (document-palette-idx document) n)
                  (update-current-color current-color document))
                ;; Redraw areas
                (mapc #'gtk-widget-queue-draw all-areas)))
             background-button))
         nil (si:range 0 (palette-length document)))

        (gtk-box-pack-end toolbar-box current-color :expand nil)
        (gtk-container-add frame grid)
        (gtk-box-pack-end workspace-box frame))
      
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
         item-file-quit "activate"
         (lambda (widget)
           (declare (ignore widget))
           (gtk-window-close window)))
          
        (setf (gtk-menu-item-submenu item-file) submenu)
        (gtk-menu-shell-append menu-bar item-file))

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

      (let ((item-settings (make-instance 'gtk-menu-item
                                          :label "_Settings"
                                          :use-underline t))
            (submenu (make-instance 'gtk-menu))
            (item-settings-pref (make-menu-entry "gtk-preferences")))

        (g-signal-connect
         item-settings-pref "activate"
         (lambda (widget)
           (declare (ignore widget))
           (exec-settings-and-update
            window document
            draft-area corrected-area simulation-area)))

        (gtk-menu-shell-append submenu item-settings-pref)
        (setf (gtk-menu-item-submenu item-settings) submenu)
        (gtk-menu-shell-append menu-bar item-settings))

      (gtk-box-pack-start main-box menu-bar :expand nil)
      (gtk-box-pack-start main-box toolbar-box :expand nil)
      (gtk-box-pack-end   main-box workspace-box :padding 10)

      (gtk-container-add window main-box)
      (gtk-widget-show-all window))))
