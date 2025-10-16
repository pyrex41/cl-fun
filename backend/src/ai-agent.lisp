;;;; ai-agent.lisp - AI Agent for Component Generation (Multi-Provider)

(in-package #:collabcanvas)

;;; LangChain-style Configuration

(defparameter *ai-provider* (or (uiop:getenv "AI_PROVIDER") "openai")
  "AI provider: openai, groq, or anthropic")

(defparameter *ai-model*
  (or (uiop:getenv "AI_MODEL")
      (cond
        ((string= *ai-provider* "openai") "gpt-4o-mini")
        ((string= *ai-provider* "groq") "llama-3.3-70b-versatile")
        ((string= *ai-provider* "anthropic") "claude-3-5-sonnet-20241022")
        (t "gpt-4o-mini")))
  "AI model to use for component generation")

(defparameter *ai-api-key*
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "GROQ_API_KEY")
      (uiop:getenv "ANTHROPIC_API_KEY"))
  "API key from environment variable")

(defparameter *ai-max-tokens* 4096
  "Maximum tokens for AI response")

(defparameter *ai-timeout* 30
  "Timeout for AI API calls in seconds")

(defparameter *max-component-count* 50
  "Maximum objects AI can create in single command")

;;; Provider-specific configuration

(defparameter *provider-config*
  '((:openai
     (:base-url "https://api.openai.com/v1/chat/completions")
     (:auth-header "Authorization")
     (:auth-format "Bearer ~A")
     (:models ("gpt-4o" "gpt-4o-mini" "gpt-4-turbo")))
    (:groq
     (:base-url "https://api.groq.com/openai/v1/chat/completions")
     (:auth-header "Authorization")
     (:auth-format "Bearer ~A")
     (:models ("llama-3.3-70b-versatile" "llama-3.1-70b-versatile" "mixtral-8x7b-32768")))
    (:anthropic
     (:base-url "https://api.anthropic.com/v1/messages")
     (:auth-header "x-api-key")
     (:auth-format "~A")
     (:models ("claude-3-5-sonnet-20241022" "claude-3-opus-20240229"))))
  "Provider-specific API configuration")

;;; API Client

(defun get-provider-config (provider-key)
  "Get configuration for a specific provider"
  (let ((config (cdr (assoc provider-key *provider-config*))))
    (unless config
      (error "Unknown provider: ~A" provider-key))
    config))

(defun validate-api-key ()
  "Ensure API key is set before making requests"
  (unless (and *ai-api-key*
               (> (length *ai-api-key*) 0))
    (error "No API key found. Set OPENAI_API_KEY, GROQ_API_KEY, or ANTHROPIC_API_KEY"))

  (log-info "Using AI provider: ~A, model: ~A" *ai-provider* *ai-model*))

(defun call-llm-api (messages tools)
  "Call LLM API with provider-agnostic interface"
  (validate-api-key)

  (let* ((provider (intern (string-upcase *ai-provider*) :keyword))
         (config (get-provider-config provider)))

    (cond
      ;; OpenAI and Groq use OpenAI-compatible format
      ((or (eq provider :openai) (eq provider :groq))
       (call-openai-compatible-api messages tools config))

      ;; Anthropic uses different format
      ((eq provider :anthropic)
       (call-anthropic-api messages tools config))

      (t
       (error "Unsupported provider: ~A" provider)))))

(defun call-openai-compatible-api (messages tools config)
  "Call OpenAI-compatible API (OpenAI, Groq, etc.)"
  (let* ((url (getf config :base-url))
         (auth-header (getf config :auth-header))
         (auth-format (getf config :auth-format))
         (auth-value (format nil auth-format *ai-api-key*))
         (headers `((,auth-header . ,auth-value)
                   ("content-type" . "application/json")))
         (body-hash (make-hash-table :test 'equal)))

    ;; Build OpenAI format request
    (setf (gethash "model" body-hash) *ai-model*)
    (setf (gethash "max_tokens" body-hash) *ai-max-tokens*)
    (setf (gethash "messages" body-hash) (convert-messages-openai messages))
    (setf (gethash "tools" body-hash) (convert-tools-openai tools))

    (log-info "Calling ~A API: ~A" *ai-provider* url)

    (handler-case
        (multiple-value-bind (response status)
            (dex:post url
                      :headers headers
                      :content (jonathan:to-json body-hash)
                      :read-timeout *ai-timeout*)

          (unless (= status 200)
            (error "~A API error (status ~A): ~A" *ai-provider* status response))

          (log-info "~A API response received (status ~A)" *ai-provider* status)
          (parse-openai-response (parse-json response)))

      (error (e)
        (error "~A API error: ~A" *ai-provider* e))))

(defun call-anthropic-api (messages tools config)
  "Call Anthropic Claude API"
  (let* ((url (getf config :base-url))
         (auth-header (getf config :auth-header))
         (headers `((,auth-header . ,*ai-api-key*)
                   ("anthropic-version" . "2023-06-01")
                   ("content-type" . "application/json")))
         (body-hash (make-hash-table :test 'equal)))

    ;; Build Anthropic format request
    (setf (gethash "model" body-hash) *ai-model*)
    (setf (gethash "max_tokens" body-hash) *ai-max-tokens*)
    (setf (gethash "messages" body-hash) (convert-messages messages))
    (setf (gethash "tools" body-hash) (convert-tools tools))

    (log-info "Calling Anthropic API: ~A" url)

    (handler-case
        (multiple-value-bind (response status)
            (dex:post url
                      :headers headers
                      :content (jonathan:to-json body-hash)
                      :read-timeout *ai-timeout*)

          (unless (= status 200)
            (error "Anthropic API error (status ~A): ~A" status response))

          (log-info "Anthropic API response received (status ~A)" status)
          (parse-json response))

      (error (e)
        (error "Anthropic API error: ~A" e)))))

(defun parse-openai-response (response)
  "Parse OpenAI API response and normalize to standard format"
  (let* ((choices (cdr (assoc :choices response)))
         (message (when choices (cdr (assoc :message (first choices)))))
         (tool-calls (when message (cdr (assoc :tool-calls message)))))

    (if tool-calls
        ;; Convert OpenAI tool_calls to standard format
        `((:content . ,(mapcar (lambda (tc)
                                (let* ((func (cdr (assoc :function tc)))
                                       (name (cdr (assoc :name func)))
                                       (args-str (cdr (assoc :arguments func)))
                                       (args (parse-json args-str)))
                                  `((:type . "tool_use")
                                    (:name . ,name)
                                    (:input . ,args))))
                              tool-calls)))
        ;; No tool calls - return text content
        `((:content . ,(cdr (assoc :content message))))))))

(defun convert-messages-openai (messages)
  "Convert messages to OpenAI format (includes system message in messages array)"
  (mapcar (lambda (msg)
            (let ((hash (make-hash-table :test 'equal)))
              (setf (gethash "role" hash) (cdr (assoc :role msg)))
              (setf (gethash "content" hash) (cdr (assoc :content msg)))
              hash))
          messages))

(defun convert-tools-openai (tools)
  "Convert tool definitions to OpenAI function calling format"
  (mapcar (lambda (tool)
            (let ((tool-hash (make-hash-table :test 'equal)))
              (setf (gethash "type" tool-hash) "function")
              (let ((func-hash (make-hash-table :test 'equal)))
                (setf (gethash "name" func-hash) (getf tool :name))
                (setf (gethash "description" func-hash) (getf tool :description))
                (setf (gethash "parameters" func-hash)
                      (plist-to-hash-recursive (getf tool :input_schema)))
                (setf (gethash "function" tool-hash) func-hash))
              tool-hash))
          tools))

(defun convert-messages (messages)
  "Convert Lisp messages to Anthropic format"
  (mapcar (lambda (msg)
            (let ((hash (make-hash-table :test 'equal)))
              (setf (gethash "role" hash) (cdr (assoc :role msg)))
              (setf (gethash "content" hash) (cdr (assoc :content msg)))
              hash))
          messages))

(defun convert-tools (tools)
  "Convert tool definitions to Anthropic format"
  (mapcar #'convert-tool tools))

(defun convert-tool (tool)
  "Convert single tool definition to Anthropic format"
  (let ((tool-hash (make-hash-table :test 'equal)))
    (setf (gethash "name" tool-hash) (getf tool :name))
    (setf (gethash "description" tool-hash) (getf tool :description))
    (setf (gethash "input_schema" tool-hash)
          (plist-to-hash-recursive (getf tool :input_schema)))
    tool-hash))

(defun plist-to-hash-recursive (plist)
  "Recursively convert plist to hash table for JSON encoding"
  (cond
    ((null plist) nil)
    ((and (listp plist) (keywordp (first plist)))
     ;; It's a plist
     (let ((hash (make-hash-table :test 'equal)))
       (loop for (key value) on plist by #'cddr
             do (setf (gethash (string-downcase (symbol-name key)) hash)
                     (plist-to-hash-recursive value)))
       hash))
    ((listp plist)
     ;; It's a list of items
     (mapcar #'plist-to-hash-recursive plist))
    (t plist)))

;;; Tool Definitions

(defparameter *component-tools*
  '((:name "create_login_form"
     :description "Create a login form with email/username and password fields"
     :input_schema (:type "object"
                    :properties (:x (:type "number"
                                    :description "X position for top-left corner")
                                :y (:type "number"
                                    :description "Y position for top-left corner")
                                :theme (:type "string"
                                       :description "Color theme: light, dark, blue, etc."
                                       :default "light"))
                    :required (:x :y)))

    (:name "create_button"
     :description "Create a button with text"
     :input_schema (:type "object"
                    :properties (:x (:type "number")
                                :y (:type "number")
                                :text (:type "string"
                                      :description "Button label")
                                :width (:type "number"
                                       :description "Button width"
                                       :default 120)
                                :style (:type "string"
                                       :description "primary, secondary, danger"
                                       :default "primary"))
                    :required (:x :y :text)))

    (:name "create_input_field"
     :description "Create a text input field with optional label"
     :input_schema (:type "object"
                    :properties (:x (:type "number")
                                :y (:type "number")
                                :label (:type "string"
                                       :description "Label text above input")
                                :placeholder (:type "string"
                                             :description "Placeholder text")
                                :width (:type "number"
                                       :default 200))
                    :required (:x :y)))

    (:name "create_card"
     :description "Create a card container (rounded rectangle with shadow)"
     :input_schema (:type "object"
                    :properties (:x (:type "number")
                                :y (:type "number")
                                :width (:type "number"
                                       :default 300)
                                :height (:type "number"
                                        :default 200)
                                :title (:type "string"
                                       :description "Card title"))
                    :required (:x :y)))))

;;; Command Processing

(defun validate-ai-command (command)
  "Validate user command before sending to AI"
  ;; Max length
  (when (> (length command) 500)
    (error "Command too long (max 500 characters)"))

  ;; No empty commands
  (when (string= (string-trim " " command) "")
    (error "Command cannot be empty"))

  ;; Basic sanitization
  (sanitize-string command))

(defun execute-ai-command (command canvas-id canvas-state user-id)
  "Execute natural language command and return list of objects to create"
  (log-info "AI command: ~A (canvas: ~A, user: ~A)" command canvas-id user-id)

  ;; Validate command
  (let ((clean-command (validate-ai-command command)))

    ;; Build context from canvas state
    (let* ((context (build-canvas-context canvas-state))
           (context-summary (format nil "~A existing objects"
                                   (or (cdr (assoc :object-count context)) 0)))
           (system-message
            (format nil "You are an AI assistant for a collaborative design tool. ~
                        The user wants to create UI components on a canvas. ~
                        Current canvas state: ~A. ~
                        Use the provided tools to create components."
                    context-summary))
           (user-message clean-command))

      ;; Call LLM API with tools
      (let* ((messages `(((:role . "user")
                          (:content . ,user-message))))
             (response (call-llm-api messages *component-tools*)))

        ;; Parse tool calls from response
        (let ((tool-calls (extract-tool-calls response)))
          (if tool-calls
              ;; Execute each tool call
              (let ((objects nil))
                (dolist (call tool-calls)
                  (let ((tool-name (cdr (assoc :name call)))
                        (tool-input (cdr (assoc :input call))))
                    (log-info "AI tool call: ~A with ~A" tool-name tool-input)

                    ;; Execute component builder function
                    (let ((result (execute-component-builder tool-name tool-input)))
                      (setf objects (append objects result)))))

                ;; Validate component count
                (when (> (length objects) *max-component-count*)
                  (error "Too many objects generated: ~A (max: ~A)"
                         (length objects) *max-component-count*))

                ;; Return generated objects
                (log-info "AI generated ~A objects" (length objects))
                objects)

              ;; No tool calls - error
              (error "AI did not generate any components")))))))

(defun extract-tool-calls (response)
  "Extract tool_use blocks from normalized LLM response"
  (let ((content (cdr (assoc :content response))))
    (when (listp content)
      (loop for block in content
            when (string= (cdr (assoc :type block)) "tool_use")
            collect `((:name . ,(cdr (assoc :name block)))
                      (:input . ,(cdr (assoc :input block))))))))

;;; Rate Limiting (placeholder for future implementation)

(defparameter *ai-command-rate-limit* 10
  "Max AI commands per user per hour")

(defparameter *ai-rate-limiters* (make-hash-table :test 'equal)
  "Hash table of user-id -> rate-limiter")

(defstruct ai-rate-limiter
  (commands-this-hour 0)
  (hour-start (get-universal-time)))

(defun check-ai-rate-limit (user-id)
  "Check if user has exceeded AI command rate limit"
  (let ((limiter (gethash user-id *ai-rate-limiters*)))
    (unless limiter
      ;; Create new limiter for user
      (setf limiter (make-ai-rate-limiter))
      (setf (gethash user-id *ai-rate-limiters*) limiter))

    ;; Reset counter if hour has passed
    (when (> (- (get-universal-time) (ai-rate-limiter-hour-start limiter)) 3600)
      (setf (ai-rate-limiter-commands-this-hour limiter) 0)
      (setf (ai-rate-limiter-hour-start limiter) (get-universal-time)))

    ;; Check limit
    (when (>= (ai-rate-limiter-commands-this-hour limiter) *ai-command-rate-limit*)
      (error "Rate limit exceeded. Try again in ~A minutes."
             (ceiling (/ (- 3600 (- (get-universal-time)
                                   (ai-rate-limiter-hour-start limiter)))
                        60))))

    ;; Increment counter
    (incf (ai-rate-limiter-commands-this-hour limiter))))
