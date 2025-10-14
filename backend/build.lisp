;;;; build.lisp - Build standalone executable

;; Load the system with all dependencies (Quicklisp already loaded by ros)
(format t "~%Loading CollabCanvas with dependencies...~%")
(ql:quickload :collabcanvas :silent t)

;; Build standalone executable
(format t "Building standalone executable...~%")
#+sbcl
(progn
  (sb-ext:save-lisp-and-die
   "collabcanvas-server"
   :toplevel #'collabcanvas:main
   :executable t
   :compression t
   :save-runtime-options t)
  (format t "Build complete!~%"))

#+ccl
(progn
  (ccl:save-application
   "collabcanvas-server"
   :toplevel-function #'collabcanvas:main
   :prepend-kernel t)
  (format t "Build complete!~%"))

#-(or sbcl ccl)
(error "Unsupported Lisp implementation")
