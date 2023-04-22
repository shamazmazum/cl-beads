(ql:quickload :cl-beads)
(handler-bind
    ((error (lambda (c)
              (princ c)
              (uiop:quit 1))))
  (asdf:load-system :cl-beads :force t)
  (uiop:quit 0))
