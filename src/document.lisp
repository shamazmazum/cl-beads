(in-package :cl-beads)

(sera:defconstructor color
  (r double-float)
  (g double-float)
  (b double-float))

(sera:-> color->gdk-rgba (color)
         (values gdk-rgba &optional))
(defun color->gdk-rgba (color)
  "Convert a color to an instance of GDK RGBA."
  (make-gdk-rgba
   :red   (color-r color)
   :green (color-g color)
   :blue  (color-b color)
   :alpha 1d0))

(sera:-> gdk-rgba->color (gdk-rgba)
         (values color &optional))
(defun gdk-rgba->color (gdk-rgba)
"Convert an instance of GDK RGBA to color."
  (color (gdk-rgba-red   gdk-rgba)
         (gdk-rgba-green gdk-rgba)
         (gdk-rgba-blue  gdk-rgba)))

(deftype palette () '(simple-array color (*)))
(deftype scheme () '(simple-array unsigned-byte (* *)))

;; Taken from JBead
(defparameter *initial-palette*
  (map
   '(vector color) #'identity 
   (list
    (COLOR 1.0d0 1.0d0 1.0d0)
    (COLOR 0.5019607843137255d0 0.0d0 0.0d0)
    (COLOR 0.996078431372549d0 0.058823529411764705d0 0.058823529411764705d0)
    (COLOR 0.9647058823529412d0 0.1568627450980392d0 0.011764705882352941d0)
    (COLOR 0.996078431372549d0 0.2980392156862745d0 0.14901960784313725d0)
    (COLOR 0.984313725490196d0 0.5450980392156862d0 0.043137254901960784d0)
    (COLOR 1.0d0 0.9058823529411765d0 0.08627450980392157d0)
    (COLOR 0.9607843137254902d0 0.9764705882352941d0 0.023529411764705882d0)
    (COLOR 0.996078431372549d0 0.996078431372549d0 0.4235294117647059d0)
    (COLOR 0.5294117647058824d0 0.043137254901960784d0 0.09411764705882353d0)
    (COLOR 0.7019607843137254d0 0.3686274509803922d0 0.011764705882352941d0)
    (COLOR 0.16470588235294117d0 0.07058823529411765d0 0.611764705882353d0)
    (COLOR 0.35294117647058826d0 0.16470588235294117d0 0.9882352941176471d0)
    (COLOR 0.25098039215686274d0 0.6039215686274509d0 0.9019607843137255d0)
    (COLOR 0.40784313725490196d0 0.7372549019607844d0 0.984313725490196d0)
    (COLOR 0.4117647058823529d0 0.7764705882352941d0 0.6941176470588235d0)
    (COLOR 0.25098039215686274d0 0.6745098039215687d0 0.7254901960784313d0)
    (COLOR 0.6d0 0.807843137254902d0 0.6901960784313725d0)
    (COLOR 0.2980392156862745d0 0.5450980392156862d0 0.33725490196078434d0)
    (COLOR 0.0d0 0.6901960784313725d0 0.3607843137254902d0)
    (COLOR 0.24705882352941178d0 0.8745098039215686d0 0.11372549019607843d0)
    (COLOR 0.5568627450980392d0 0.8941176470588236d0 0.4666666666666667d0)
    (COLOR 0.8745098039215686d0 0.3411764705882353d0 0.7333333333333333d0)
    (COLOR 1.0d0 0.3764705882352941d0 0.8313725490196079d0)
    (COLOR 0.7843137254901961d0 0.7098039215686275d0 1.0d0)
    (COLOR 0.6901960784313725d0 0.5333333333333333d0 0.6274509803921569d0)
    (COLOR 0.8862745098039215d0 0.9294117647058824d0 0.9372549019607843d0)
    (COLOR 0.8588235294117647d0 0.8627450980392157d0 0.8705882352941177d0)
    (COLOR 0.5607843137254902d0 0.5764705882352941d0 0.6235294117647059d0)
    (COLOR 0.22745098039215686d0 0.27450980392156865d0 0.33725490196078434d0)
    (COLOR 0.14901960784313725d0 0.20392156862745098d0 0.21568627450980393d0)
    (COLOR 0.0d0 0.0d0 0.0d0)))
  "Initial palette for a newly created document.")

(defparameter *default-outline-color* (color 0d0 0d0 0d0)
  "Default color for bead outline.")

(defclass document ()
  ((palette       :initform (copy-seq *initial-palette*)
                  :initarg  :palette
                  :type     palette
                  :reader   document-palette)
   (palette-idx   :initform 0
                  :initarg  :palette-idx
                  :type     unsigned-byte
                  :accessor document-palette-idx)
   (scheme        :initarg  :scheme
                  :type     scheme
                  :accessor document-scheme)
   (outline-color :initform *default-outline-color*
                  :initarg  :outline-color
                  :type     color
                  :accessor document-outline-color)
   (author        :initform ""
                  :initarg  :author
                  :type     string
                  :accessor document-author)
   (organization  :initform ""
                  :initarg  :organization
                  :type     string
                  :accessor document-organization)
   (notes         :initform ""
                  :initarg  :organization
                  :type     string
                  :accessor document-notes))
  (:documentation "A generic class for a document"))

(defgeneric palette-color (document index)
  (:documentation "Accessor for a particular color from the document's palette."))

(defmethod palette-color ((document document) index)
  (aref (document-palette document) index))

(defmethod (setf palette-color) (value (document document) index)
  (setf (aref (document-palette document) index) value))

(sera:-> palette-length (document)
         (values unsigned-byte &optional))
(defun palette-length (document)
  (length (document-palette document)))

(sera:-> current-color (document)
         (values color &optional))
(defun current-color (document)
  (palette-color document (document-palette-idx document)))

(sera:-> update-scheme (document unsigned-byte unsigned-byte)
         (values &optional))
(defun update-scheme (document width height)
  (let* ((scheme (document-scheme document))
         (selection-height (min height (array-dimension scheme 0)))
         (selection-width  (min width  (array-dimension scheme 1)))
         (new-scheme (make-array (list height width)
                                 :element-type 'unsigned-byte
                                 :initial-element 0)))
    (setf (document-scheme document) new-scheme
          (select:select new-scheme
            (select:range 0 selection-height)
            (select:range 0 selection-width))
          (select:select scheme
            (select:range 0 selection-height)
            (select:range 0 selection-width))))
  (values))

(sera:-> document-height (document)
         (values unsigned-byte &optional))
(defun document-height (document)
  (array-dimension (document-scheme document) 0))

(sera:-> document-width (document)
         (values unsigned-byte &optional))
(defun document-width (document)
  (array-dimension (document-scheme document) 1))

(sera:-> clone-rows-up (document unsigned-byte unsigned-byte)
         (values document &optional))
(defun clone-rows-up (document from to)
  "Replicate rows from FROM to TO periodically from TO+1 to the end of
the document."
  (assert (< from to))
  (let ((scheme (document-scheme document))
        (width  (document-width  document))
        (height (document-height document))
        (length (- to from)))
    (loop with selection = (select:select scheme
                             (select:range from to)
                             (select:range 0 width))
          for start from to by length
          while (< (+ start length -1) height) do
          (setf (select:select scheme
                  (select:range start (+ start length))
                  (select:range 0 width))
                selection)))
  document)

;; Document types

(defun make-scheme (height width)
  (make-array (list height width)
              :element-type 'unsigned-byte
              ;; Initialize with background color
              :initial-element 0))

(defparameter *default-rope-width* 15
  "Initial width of a scheme (beaded rope).")

(defparameter *default-rope-height* 200
  "Initial height of a scheme (beaded rope).")

(defclass document-rope (document)
  ()
  (:default-initargs
   :scheme (make-scheme *default-rope-height*
                        *default-rope-width*))
  (:documentation "A document for a beaded crochet rope"))

(defparameter *default-ring-width* 30
  "Initial width of a scheme (ring).")

(defparameter *default-ring-height* 6
  "Initial height of a scheme (ring).")

(defclass document-ring (document)
  ()
  (:default-initargs
   :scheme (make-scheme *default-ring-height*
                        *default-ring-width*))
  (:documentation "A document for a ring in mosaic technique"))
