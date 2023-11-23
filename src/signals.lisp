(in-package :cl-beads)

;; All custom GTK signals are created here
(defun add-new-signals ()
  (cffi:with-foreign-object (param 'g-type)
    (setf (cffi:mem-ref param 'g-type)
          +g-type-uint+)
    (g-signal-newv
     "my-bead-clicked"
     +g-type-object+
     '(:run-last)
     (cffi:null-pointer)
     (cffi:null-pointer)
     (cffi:null-pointer)
     (cffi:null-pointer)
     +g-type-none+ 1
     param))
  (flet ((trivial-new-signal (name)
           (g-signal-newv
            name
            +g-type-object+
            '(:run-last)
            (cffi:null-pointer)
            (cffi:null-pointer)
            (cffi:null-pointer)
            (cffi:null-pointer)
            +g-type-none+ 0
            (cffi:null-pointer))))
    (mapc #'trivial-new-signal
          '("my-color-set"
            "my-background-change-request"
            "my-reading-line-position-changed"))))

(sera:defalias add-new-signals-once (sera:once #'add-new-signals))
