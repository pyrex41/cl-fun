;;;; package.lisp - Package definition for CollabCanvas (Woo/Clack Edition)

(defpackage #:collabcanvas
  (:use #:cl)
  (:import-from #:clack
                #:clackup
                #:stop)
  (:import-from #:websocket-driver
                #:make-server
                #:send
                #:on
                #:start-connection)
  (:import-from #:jonathan
                #:parse
                #:to-json)
  (:import-from #:ironclad
                #:digest-sequence
                #:byte-array-to-hex-string
                #:ascii-string-to-byte-array)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-thread
                #:destroy-thread
                #:all-threads)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values
                #:when-let
                #:if-let)
  (:import-from #:cl-ppcre
                #:scan
                #:split)
  (:export
   ;; Server management
   #:start-server
   #:stop-server
   #:restart-server
   #:main
   #:*server*

   ;; Configuration
   #:*port*
   #:*host*
   #:*database-path*
   #:*session-timeout*

   ;; Database
   #:init-database
   #:with-database
   #:execute-query
   #:execute-non-query

   ;; Authentication
   #:register-user
   #:login-user
   #:logout-user
   #:validate-session
   #:hash-password

   ;; Canvas operations
   #:save-canvas-state
   #:load-canvas-state
   #:get-canvas-history

   ;; WebSocket
   #:broadcast-to-room
   #:handle-canvas-message
   #:get-room-users))