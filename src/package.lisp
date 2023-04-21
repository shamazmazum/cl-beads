(defpackage cl-beads
  (:use #:cl #:gtk #:gdk #:cairo #:gobject)
  (:local-nicknames (#:si   #:stateless-iterators)
                    (#:alex #:alexandria)
                    (#:sera #:serapeum))
  (:export #:run
           #:main))
