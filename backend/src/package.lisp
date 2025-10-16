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
  (:import-from #:dexador)
  (:import-from #:babel)
  (:import-from #:cl-base64)
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

   ;; Auth0 config & OAuth
   #:ensure-auth0-config!
   #:auth0-authorize-url
   #:auth0-token-url
   #:auth0-userinfo-url
   #:auth0-logout-url
   #:handle-auth0-login
   #:handle-auth0-callback

   ;; Database
   #:init-database
   #:with-database
   #:execute-query
   #:execute-non-query
   #:ensure-auth0-user-columns
   #:get-user-by-auth0-sub
   #:find-or-create-user-from-oauth

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