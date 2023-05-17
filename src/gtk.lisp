;; Add missing functionality

;; GDK
(in-package :gdk-pixbuf)

(defun initialize-pixbuf-space (array)
  (declare (type (simple-array (unsigned-byte 8)) array))
  (foreign-alloc :uint8
                 :count (array-total-size array)
                 :initial-contents (coerce (aops:flatten array) 'list)))

(defcallback free-pixbuf-space :void ((ptr (:pointer :uint8)) (data :pointer))
  (declare (ignore data))
  (foreign-free ptr))

(defcfun ("gdk_pixbuf_new_from_data" %gdk-pixbuf-new-from-data)
    (g-object gdk-pixbuf :already-referenced)
  (data            (:pointer :uint8))
  (colorspace      gdk-colorspace)
  (has-alpha       :boolean)
  (bits-per-sample :int)
  (width           :int)
  (height          :int)
  (rowstride       :int)
  (destroy-fn      :pointer)
  (destroy-fn-data :pointer))

(defun gdk-pixbuf-new-from-data (data)
  (%gdk-pixbuf-new-from-data
   (initialize-pixbuf-space data) :rgb
   (= (array-dimension data 2) 4) 8
   (array-dimension data 1)
   (array-dimension data 0)
   (* (array-dimension data 1)
      (array-dimension data 2))
   (callback free-pixbuf-space)
   (null-pointer)))
(export 'gdk-pixbuf-new-from-data)

;; GTK
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
  (cr      (:pointer (:struct cairo:cairo-t)))
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

(defcfun ("gtk_file_filter_set_name" gtk-file-filter-set-name) :void
  (filter  (g-object gtk-file-filter))
  (pattern :string))
(export 'gtk-file-filter-set-name)

;; Cairo
(in-package :cairo)

(defcfun ("cairo_identity_matrix" cairo-identity-matrix) :void
  (cr (:pointer (:struct cairo-t))))
(export 'cairo-identity-matrix)

(defun fill-foreign-matrix (matrix ptr)
  ;; More elegant way to do this?!
  (setf (foreign-slot-value ptr '(:struct cairo-matrix-t) 'xx)
        (getf matrix 'xx)
        (foreign-slot-value ptr '(:struct cairo-matrix-t) 'yx)
        (getf matrix 'yx)
        (foreign-slot-value ptr '(:struct cairo-matrix-t) 'xy)
        (getf matrix 'xy)
        (foreign-slot-value ptr '(:struct cairo-matrix-t) 'yy)
        (getf matrix 'yy)
        (foreign-slot-value ptr '(:struct cairo-matrix-t) 'x0)
        (getf matrix 'x0)
        (foreign-slot-value ptr '(:struct cairo-matrix-t) 'y0)
        (getf matrix 'y0)))

(defcfun ("cairo_get_matrix" %cairo-get-matrix) :void
  (cr (:pointer (:struct cairo-t)))
  (matrix (:pointer (:struct cairo-matrix-t))))

(defun cairo-get-matrix (cr)
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (%cairo-get-matrix cr matrix)
    (convert-from-foreign matrix '(:struct cairo-matrix-t))))
(export 'cairo-get-matrix)

(defcfun ("cairo_matrix_init_identity" %cairo-matrix-init-identity) :void
  (matrix (:pointer (:struct cairo-matrix-t))))

(defun cairo-matrix-init-identity ()
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (%cairo-matrix-init-identity matrix)
    (convert-from-foreign matrix '(:struct cairo-matrix-t))))
(export 'cairo-matrix-init-identity)

(defcfun ("cairo_matrix_transform_point" %cairo-matrix-transform-point) :void
  (matrix (:pointer (:struct cairo-matrix-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun cairo-matrix-transform-point (matrix x y)
  (with-foreign-objects ((x-ptr :double)
                         (y-ptr :double)
                         (m-ptr '(:struct cairo-matrix-t)))
    (setf (mem-ref x-ptr :double)
          (float x 0d0)
          (mem-ref y-ptr :double)
          (float y 0d0))
    (fill-foreign-matrix matrix m-ptr)
    (%cairo-matrix-transform-point m-ptr x-ptr y-ptr)
    (values
     (mem-ref x-ptr :double)
     (mem-ref y-ptr :double))))
(export 'cairo-matrix-transform-point)

(defcfun ("cairo_matrix_transform_distance" %cairo-matrix-transform-distance) :void
  (matrix (:pointer (:struct cairo-matrix-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun cairo-matrix-transform-distance (matrix x y)
  (with-foreign-objects ((x-ptr :double)
                         (y-ptr :double)
                         (m-ptr '(:struct cairo-matrix-t)))
    (setf (mem-ref x-ptr :double)
          (float x 0d0)
          (mem-ref y-ptr :double)
          (float y 0d0))
    (fill-foreign-matrix matrix m-ptr)
    (%cairo-matrix-transform-distance m-ptr x-ptr y-ptr)
    (values
     (mem-ref x-ptr :double)
     (mem-ref y-ptr :double))))
(export 'cairo-matrix-transform-distance)

(defcfun ("cairo_matrix_invert" %cairo-matrix-invert) cairo-status-t
  (matrix (:pointer (:struct cairo-matrix-t))))

(defun cairo-matrix-invert (matrix)
  (with-foreign-object (m-ptr '(:struct cairo-matrix-t))
    (fill-foreign-matrix matrix m-ptr)
    (let ((status (%cairo-matrix-invert m-ptr)))
      (values
       (convert-from-foreign m-ptr '(:struct cairo-matrix-t))
       status))))
(export 'cairo-matrix-invert)

(defpackage gtk-fixed
  (:use #:cl)
  (:local-nicknames (:sera :serapeum))
  (:export #:gtk-render-focus))
(in-package :gtk-fixed)

(defun gtk-render-focus (context cr x y width height)
  (gtk:gtk-render-focus
   context cr
   (float x 0d0)
   (float y 0d0)
   (float width 0d0)
   (float height 0d0)))
