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
  (:import-from #:dexador)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:babel)
  (:import-from #:cl-base64)
  (:import-from #:flexi-streams
                #:octets-to-string)
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
  (:import-from #:cl-fast-ecs
                #:make-storage
                #:make-entity
                #:entity
                #:entity-valid-p
                #:define-entity
                #:define-component
                #:make-component
                #:delete-component
                #:reset-component
                #:replace-component
                #:define-system
                #:defsystem
                #:delete-system
                #:run-systems
                #:make-object
                #:with-storage
                #:current-entity
                #:print-entity
                #:print-entities/picture
                #:*storage*
                #:storage)
  (:import-from #:quadtree
                #:make
                #:clear
                #:insert
                #:query
                #:intersect-p)
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
   #:handle-auth0-link

   ;; Database
   #:init-database
   #:with-database
   #:execute-query
   #:execute-non-query
   #:ensure-auth0-user-columns
   #:get-user-by-auth0-sub
   #:find-or-create-user-from-oauth
   #:link-auth0-to-existing-user

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
   #:get-room-users

   ;; Auth Metrics
   #:get-auth-metrics
   #:get-auth-migration-stats
   #:handle-auth-metrics
   #:log-oauth-error
   #:log-failed-login

   ;; Physics ECS Storage Management
   #:*physics-canvases*
   #:*physics-canvases-lock*
   #:physics-canvas-config
   #:make-physics-canvas-config
   #:physics-canvas-config-storage
   #:physics-canvas-config-gravity-x
   #:physics-canvas-config-gravity-y
   #:physics-canvas-config-simulation-rate
   #:physics-canvas-config-created-at
   #:init-canvas-physics
   #:get-canvas-ecs-storage
   #:get-canvas-physics-config
   #:destroy-canvas-physics
   #:list-active-physics-canvases
   #:canvas-physics-active-p
   #:update-canvas-physics-settings
   #:cleanup-all-physics-canvases

   ;; Legacy (deprecated)
   #:*physics-world*
   #:init-physics-world

   ;; Physics Entity Spawning
   #:spawn-physics-ball
   #:spawn-physics-block

   ;; Physics Loop (to be implemented)
   #:start-physics-loop
   #:stop-physics-loop
   #:add-physics-object
   #:update-physics-object
   #:remove-physics-object
   #:get-physics-state
   #:apply-force
   #:set-velocity

   ;; Physics Database
   #:init-physics-schema
   #:init-physics-db
   #:save-physics-settings
   #:load-physics-settings
   #:delete-physics-settings
   #:save-physics-component
   #:load-physics-component
   #:load-physics-components
   #:delete-physics-component
   #:delete-canvas-physics-components
   #:count-physics-components
   #:save-physics-body
   #:load-physics-body
   #:load-physics-bodies-by-component
   #:delete-physics-body
   #:delete-component-physics-bodies
   #:cleanup-stale-physics-bodies
   #:batch-save-physics-components
   #:batch-save-physics-bodies
   #:validate-component-type
   #:get-physics-stats

   ;; Physics Quadtree (Spatial Partitioning)
   #:entity-bounds
   #:make-entity-bounds
   #:entity-bounds-entity-id
   #:entity-bounds-x
   #:entity-bounds-y
   #:entity-bounds-radius
   #:entity-bounds-type
   #:make-physics-quadtree
   #:clear-physics-quadtree
   #:insert-entity-bounds
   #:query-nearby-entities
   #:entities-potentially-collide-p
   #:*quadtree-stats*
   #:reset-quadtree-stats
   #:get-quadtree-stats))