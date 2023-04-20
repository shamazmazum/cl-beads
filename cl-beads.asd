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
               (:file "palette-button")
               (:file "scheme-area")
               (:file "gui"))
  :depends-on (:serapeum
               :alexandria
               :stateless-iterators
               :cl-cffi-gtk
               :cl-css))
