;;;; components.lisp - UI Component Builders for AI Agent

(in-package #:collabcanvas)

;;; Theme System

(defun get-theme-colors (theme-name)
  "Get color palette for theme"
  (case (intern (string-upcase theme-name) :keyword)
    (:light
     '(:background "#FFFFFF"
       :text-primary "#1A1A1A"
       :text-secondary "#666666"
       :text-placeholder "#999999"
       :text-on-primary "#FFFFFF"
       :primary "#3B82F6"
       :border "#E5E5E5"
       :input-background "#F9FAFB"))

    (:dark
     '(:background "#1E1E1E"
       :text-primary "#FFFFFF"
       :text-secondary "#AAAAAA"
       :text-placeholder "#666666"
       :text-on-primary "#FFFFFF"
       :primary "#3B82F6"
       :border "#333333"
       :input-background "#2A2A2A"))

    (:blue
     '(:background "#EFF6FF"
       :text-primary "#1E3A8A"
       :text-secondary "#3B82F6"
       :text-placeholder "#93C5FD"
       :text-on-primary "#FFFFFF"
       :primary "#2563EB"
       :border "#BFDBFE"
       :input-background "#FFFFFF"))

    (t
     (log-info "Unknown theme ~A, using light" theme-name)
     (get-theme-colors :light)))) ; Default to light

;;; Component Builders

(defun create-button (x y text &key (width 120) (style "primary") (theme "light"))
  "Create a button with text"
  ;; Validate inputs
  (unless (and (numberp x) (numberp y))
    (error "X and Y must be numbers"))
  (unless (stringp text)
    (error "Text must be a string"))

  (let* ((colors (get-theme-colors theme))
         (button-height 44)
         (style-color (case (intern (string-upcase style) :keyword)
                       (:primary (getf colors :primary))
                       (:secondary (getf colors :border))
                       (:danger "#EF4444")
                       (t (getf colors :primary)))))

    (list
     ;; Button background
     `((:id . ,(generate-object-id))
       (:type . "rectangle")
       (:x . ,x)
       (:y . ,y)
       (:width . ,width)
       (:height . ,button-height)
       (:color . ,style-color)
       (:rounded . 6)
       (:z-index . 1))

     ;; Button text
     `((:id . ,(generate-object-id))
       (:type . "text")
       (:x . ,(+ x (/ width 2)))
       (:y . ,(+ y (/ button-height 2)))
       (:text . ,text)
       (:font-size . 16)
       (:font-weight . "medium")
       (:color . ,(getf colors :text-on-primary))
       (:text-align . "center")
       (:z-index . 2)))))

(defun create-input-field (x y &key label placeholder (width 200) (type "text") (theme "light"))
  "Create a labeled input field"
  ;; Validate inputs
  (unless (and (numberp x) (numberp y))
    (error "X and Y must be numbers"))

  (let* ((colors (get-theme-colors theme))
         (label-height 20)
         (input-height 40)
         (objects nil))

    ;; Label (if provided)
    (when label
      (push `((:id . ,(generate-object-id))
              (:type . "text")
              (:x . ,x)
              (:y . ,y)
              (:text . ,label)
              (:font-size . 14)
              (:color . ,(getf colors :text-secondary))
              (:z-index . 1))
            objects))

    ;; Input box
    (push `((:id . ,(generate-object-id))
            (:type . "rectangle")
            (:x . ,x)
            (:y . ,(if label (+ y label-height 4) y))
            (:width . ,width)
            (:height . ,input-height)
            (:color . ,(getf colors :input-background))
            (:border . ,(format nil "1px solid ~A" (getf colors :border)))
            (:rounded . 4)
            (:z-index . 1))
          objects)

    ;; Placeholder text (if provided)
    (when placeholder
      (push `((:id . ,(generate-object-id))
              (:type . "text")
              (:x . ,(+ x 12))
              (:y . ,(+ (if label (+ y label-height 4) y) 12))
              (:text . ,placeholder)
              (:font-size . 14)
              (:color . ,(getf colors :text-placeholder))
              (:z-index . 2))
            objects))

    (nreverse objects)))

(defun create-card (x y &key (width 300) (height 200) title (theme "light"))
  "Create a card container (rounded rectangle with shadow)"
  ;; Validate inputs
  (unless (and (numberp x) (numberp y))
    (error "X and Y must be numbers"))

  (let* ((colors (get-theme-colors theme))
         (objects nil))

    ;; Card background
    (push `((:id . ,(generate-object-id))
            (:type . "rectangle")
            (:x . ,x)
            (:y . ,y)
            (:width . ,width)
            (:height . ,height)
            (:color . ,(getf colors :background))
            (:border . ,(format nil "1px solid ~A" (getf colors :border)))
            (:rounded . 8)
            (:shadow . "0 2px 8px rgba(0,0,0,0.1)")
            (:z-index . 1))
          objects)

    ;; Card title (if provided)
    (when title
      (push `((:id . ,(generate-object-id))
              (:type . "text")
              (:x . ,(+ x 16))
              (:y . ,(+ y 16))
              (:text . ,title)
              (:font-size . 18)
              (:font-weight . "bold")
              (:color . ,(getf colors :text-primary))
              (:z-index . 2))
            objects))

    (nreverse objects)))

(defun create-login-form (x y &key (theme "light"))
  "Create a complete login form with email, password, button"
  ;; Validate inputs
  (unless (and (numberp x) (numberp y))
    (error "X and Y must be numbers"))
  (unless (member theme '("light" "dark" "blue") :test #'string=)
    (log-info "Unknown theme ~A, defaulting to light" theme)
    (setf theme "light"))

  (let* ((colors (get-theme-colors theme))
         (width 320)
         (label-height 20)
         (input-height 40)
         (button-height 44)
         (spacing 12)
         (current-y y)
         (objects nil))

    ;; Background card
    (push `((:id . ,(generate-object-id))
            (:type . "rectangle")
            (:x . ,x)
            (:y . ,y)
            (:width . ,width)
            (:height . ,(+ 240 (* spacing 4)))
            (:color . ,(getf colors :background))
            (:border . ,(format nil "1px solid ~A" (getf colors :border)))
            (:rounded . 8)
            (:shadow . "0 2px 8px rgba(0,0,0,0.1)")
            (:z-index . 0))
          objects)

    ;; Title
    (incf current-y 24)
    (push `((:id . ,(generate-object-id))
            (:type . "text")
            (:x . ,(+ x 24))
            (:y . ,current-y)
            (:text . "Sign In")
            (:font-size . 24)
            (:font-weight . "bold")
            (:color . ,(getf colors :text-primary))
            (:z-index . 1))
          objects)

    ;; Email field
    (incf current-y (+ 24 spacing))
    (setf objects (append objects
                          (create-input-field (+ x 24) current-y
                                             :label "Email"
                                             :placeholder "you@example.com"
                                             :width (- width 48)
                                             :theme theme)))
    (incf current-y (+ label-height input-height spacing))

    ;; Password field
    (incf current-y spacing)
    (setf objects (append objects
                          (create-input-field (+ x 24) current-y
                                             :label "Password"
                                             :placeholder "••••••••"
                                             :width (- width 48)
                                             :type "password"
                                             :theme theme)))
    (incf current-y (+ label-height input-height spacing))

    ;; Submit button
    (incf current-y spacing)
    (push `((:id . ,(generate-object-id))
            (:type . "rectangle")
            (:x . ,(+ x 24))
            (:y . ,current-y)
            (:width . ,(- width 48))
            (:height . ,button-height)
            (:color . ,(getf colors :primary))
            (:rounded . 6)
            (:z-index . 1))
          objects)

    ;; Button text
    (push `((:id . ,(generate-object-id))
            (:type . "text")
            (:x . ,(+ x (/ width 2)))
            (:y . ,(+ current-y (/ button-height 2)))
            (:text . "Sign In")
            (:font-size . 16)
            (:font-weight . "medium")
            (:color . ,(getf colors :text-on-primary))
            (:text-align . "center")
            (:z-index . 2))
          objects)

    ;; Return all objects
    (nreverse objects)))

;;; Component Builder Registry

(defparameter *component-builders*
  '(("create_button" . create-button)
    ("create_input_field" . create-input-field)
    ("create_card" . create-card)
    ("create_login_form" . create-login-form))
  "Registry of component builder functions")

(defun execute-component-builder (tool-name tool-input)
  "Execute a component builder function by name"
  (let ((builder-fn (cdr (assoc tool-name *component-builders* :test #'string=))))
    (unless builder-fn
      (error "Unknown component builder: ~A" tool-name))

    (log-info "Executing component builder: ~A" tool-name)

    ;; Call the builder function with the tool input as keyword arguments
    (apply builder-fn
           (loop for (key . value) in tool-input
                 append (list (intern (string-upcase (symbol-name key)) :keyword)
                             value)))))
