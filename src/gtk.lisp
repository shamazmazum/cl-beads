;; Add missing functionality

(in-package :gtk)

(defcfun ("gtk_css_provider_load_from_data" %gtk-css-provider-load-from-data)
    :boolean
  (css-provider (g-object gtk-css-provider))
  (data         :string)
  (length       :int)
  (error        :pointer))

(defun gtk-css-provider-load-from-data (css-provider data)
  (with-g-error (err)
    (%gtk-css-provider-load-from-data
     css-provider data (length data) err)))
(export 'gtk-css-provider-load-from-data)

(defcfun ("gtk_render_background" %gtk-render-background) :void
  (context (g-object gtk-style-context))
  (cr      :pointer)
  (x       :double)
  (y       :double)
  (width   :double)
  (height  :double))

(defun gtk-render-background (context cr x y width height)
  (%gtk-render-background
   context cr
   (float x 0d0)
   (float y 0d0)
   (float width 0d0)
   (float height 0d0)))
(export 'gtk-render-background)
