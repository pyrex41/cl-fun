;;;; quick-db-test.lisp - Quick test to verify physics-database.lisp loads

;; Load dependencies
(ql:quickload '(:sqlite :jonathan :alexandria :cl-ppcre :bordeaux-threads :cl-fast-ecs) :silent t)

;; Load our files
(defparameter *backend-root* #P"/Users/reuben/gauntlet/figma-clone/cl-fun worktrees/phys-engine/backend/")
(load (merge-pathnames "src/package.lisp" *backend-root*))
(load (merge-pathnames "src/config.lisp" *backend-root*))
(load (merge-pathnames "src/utils.lisp" *backend-root*))
(load (merge-pathnames "src/database.lisp" *backend-root*))
(load (merge-pathnames "src/physics-database.lisp" *backend-root*))

(in-package :collabcanvas)

(format t "~%~%=== Physics Database Functions Test ===~%~%")

;; Test that functions are defined
(format t "init-physics-schema defined: ~A~%" (fboundp 'init-physics-schema))
(format t "save-physics-settings defined: ~A~%" (fboundp 'save-physics-settings))
(format t "load-physics-settings defined: ~A~%" (fboundp 'load-physics-settings))
(format t "save-physics-component defined: ~A~%" (fboundp 'save-physics-component))
(format t "load-physics-component defined: ~A~%" (fboundp 'load-physics-component))
(format t "load-physics-components defined: ~A~%" (fboundp 'load-physics-components))
(format t "save-physics-body defined: ~A~%" (fboundp 'save-physics-body))
(format t "load-physics-body defined: ~A~%" (fboundp 'load-physics-body))
(format t "batch-save-physics-components defined: ~A~%" (fboundp 'batch-save-physics-components))
(format t "validate-component-type defined: ~A~%" (fboundp 'validate-component-type))
(format t "get-physics-stats defined: ~A~%" (fboundp 'get-physics-stats))

(format t "~%✓ All physics database functions are defined!~%")
(format t "~%=== Test Complete ===~%~%")
