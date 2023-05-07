(defpackage cl-beads
  (:use #:cl #:gtk #:gdk #:gdk-pixbuf #:cairo #:gobject)
  (:local-nicknames (#:si    #:stateless-iterators)
                    (#:alex  #:alexandria)
                    (#:flexi #:flexi-streams)
                    (#:sera  #:serapeum))
  (:export #:run
           #:main))
