;;;; collabcanvas.asd - ASDF system definition for CollabCanvas (Woo Edition)

(defsystem "collabcanvas"
  :description "Real-time collaborative design tool with WebSocket synchronization (Woo/Clack backend)"
  :author "CollabCanvas Team"
  :license "MIT"
  :version "0.2.0"
  :serial t
  :depends-on (;; Web framework and server
               :clack
               :woo
               :lack
               :websocket-driver
               ;; Utilities and libraries (kept from original)
               :jonathan
               :ironclad
               :bordeaux-threads
               :sqlite
               :alexandria
               :cl-ppcre
               :local-time)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "utils" :depends-on ("package" "config"))
                 (:file "database" :depends-on ("package" "config" "utils"))
                 (:file "auth" :depends-on ("package" "database" "utils"))
                 (:file "canvas-state" :depends-on ("package" "database"))
                 (:file "websocket-adapter" :depends-on ("package" "auth" "canvas-state"))
                 (:file "app" :depends-on ("package" "websocket-adapter" "auth" "canvas-state"))
                 (:file "server" :depends-on ("package" "app" "database"))
                 (:file "main" :depends-on ("package" "server" "auth")))))
  :in-order-to ((test-op (test-op "collabcanvas/tests"))))

(defsystem "collabcanvas/tests"
  :description "Test suite for CollabCanvas"
  :author "CollabCanvas Team"
  :license "MIT"
  :depends-on (:collabcanvas
               :rove)
  :components ((:module "tests"
                :components
                ((:file "test-auth")
                 (:file "test-database")
                 (:file "test-websocket"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))