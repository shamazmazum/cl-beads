(defpackage cl-beads
  (:use #:cl #:gtk #:gdk #:cairo #:gobject)
  (:local-nicknames (#:si   #:stateless-iterators)
                    (#:sera #:serapeum))
  (:export #:run))
