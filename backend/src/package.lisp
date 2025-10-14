;;;; package.lisp - Package definition for CollabCanvas

(defpackage #:collabcanvas
  (:use #:cl)
  (:import-from #:hunchentoot
                #:*acceptor*
                #:*request*
                #:start
                #:stop)
  (:import-from #:hunchensocket
                #:websocket-acceptor
                #:websocket-resource
                #:websocket-client
                #:text-message-received
                #:client-connected
                #:client-disconnected
                #:send-text-message)
  (:import-from #:jonathan
                #:parse
                #:to-json)
  (:import-from #:ironclad
                #:digest-sequence
                #:byte-array-to-hex-string
                #:ascii-string-to-byte-array)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values
                #:when-let
                #:if-let)
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