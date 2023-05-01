(defsystem :cl-beads
  :name :cl-beads
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A GTK-based rope crochet design tool similar to JBead"
  :licence "2-clause BSD"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "document")
               (:file "jbb-format")
               (:file "model")
               (:file "gtk")
               (:file "signals")
               (:file "palette-button")
               (:file "scheme-area")
               (:file "gui"))
  :depends-on (:serapeum
               :alexandria
               :stateless-iterators
               :cl-cffi-gtk
               :cl-css
               :select
               :closer-mop))

(defsystem :cl-beads/application
  :name :cl-beads/application
  :depends-on (:cl-beads)
  :build-operation program-op
  :build-pathname "cl-beads"
  :entry-point "cl-beads:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
