;;;; utils.lisp - Utility functions for CollabCanvas

(in-package #:collabcanvas)

;;; String utilities
(defun generate-uuid ()
  "Generate a UUID v4 string"
  (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (random (expt 2 32))
          (random (expt 2 16))
          (logior #x4000 (random (expt 2 16)))
          (logior #x8000 (random (expt 2 16)))
          (random (expt 2 48))))

(defun generate-session-id ()
  "Generate a secure session ID"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array
     (format nil "~A~A~A"
             (get-universal-time)
             (random (expt 2 64))
             (generate-uuid))))))

;;; Time utilities
(defun current-timestamp ()
  "Get current timestamp in ISO 8601 format"
  (local-time:format-timestring nil (local-time:now)))

(defun timestamp-to-unix (timestamp)
  "Convert timestamp string to Unix epoch"
  (local-time:timestamp-to-unix
   (local-time:parse-timestring timestamp)))

(defun unix-to-timestamp (unix)
  "Convert Unix epoch to timestamp string"
  (local-time:format-timestring nil
   (local-time:unix-to-timestamp unix)))

(defun expired-p (expires-at)
  "Check if a timestamp has expired"
  (local-time:timestamp<
   (local-time:parse-timestring expires-at)
   (local-time:now)))

;;; JSON utilities
(defun plist-to-alist (plist)
  "Convert property list to association list"
  (loop for (key value) on plist by #'cddr
        collect (cons key value)))

(defun normalize-json-keys (object)
  "Convert Jonathan's pipe-escaped keywords to regular keywords and plist to alist"
  (cond
    ((null object) nil)
    ;; Handle plists (convert to alist)
    ((and (consp object) (keywordp (car object)))
     (plist-to-alist
      (loop for (key value) on object by #'cddr
            collect (intern (string-upcase (symbol-name key)) :keyword)
            collect (normalize-json-keys value))))
    ;; Handle cons cells (alists)
    ((consp object)
     (cons (normalize-json-keys (car object))
           (normalize-json-keys (cdr object))))
    ;; Pass through everything else
    (t object)))

(defun parse-json (string)
  "Parse JSON string, returning nil on error"
  (handler-case
      (normalize-json-keys (jonathan:parse string))
    (error (e)
      (when *debug-mode*
        (format t "JSON parse error: ~A~%" e))
      nil)))

(defun alist-to-hash (alist)
  "Convert association list to hash table for Jonathan"
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (string-downcase (symbol-name (car pair))) hash)
            (cdr pair)))
    hash))

(defun to-json-string (object)
  "Convert object to JSON string"
  (cond
    ;; If it's an alist (list of cons cells), convert to hash table
    ((and (listp object)
          (every #'consp object))
     (jonathan:to-json (alist-to-hash object)))
    ;; Otherwise pass through
    (t (jonathan:to-json object))))

;;; HTTP utilities
(defun get-json-body ()
  "Get and parse JSON body from current request"
  (let ((raw-body (hunchentoot:raw-post-data :force-text t)))
    (when raw-body
      (parse-json raw-body))))

(defun json-response (data &key (status 200))
  "Send JSON response with appropriate headers"
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (to-json-string data))

(defun error-response (message &key (status 400))
  "Send error response as JSON"
  (json-response `((:error . ,message)) :status status))

(defun success-response (data)
  "Send success response as JSON"
  (json-response `((:success . t) (:data . ,data))))

;;; CORS utilities
(defun set-cors-headers ()
  "Set CORS headers for development"
  (when *cors-enabled*
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
    (setf (hunchentoot:header-out "Access-Control-Allow-Methods")
          "GET, POST, PUT, DELETE, OPTIONS")
    (setf (hunchentoot:header-out "Access-Control-Allow-Headers")
          "Content-Type, Authorization")
    (setf (hunchentoot:header-out "Access-Control-Max-Age") "3600")))

;;; Validation utilities
(defun valid-email-p (email)
  "Check if email is valid"
  (and (stringp email)
       (cl-ppcre:scan "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$" email)))

(defun valid-username-p (username)
  "Check if username is valid"
  (and (stringp username)
       (>= (length username) 3)
       (<= (length username) 30)
       (cl-ppcre:scan "^[a-zA-Z0-9_-]+$" username)))

(defun valid-password-p (password)
  "Check if password meets requirements"
  (and (stringp password)
       (>= (length password) 8)))

;;; Canvas utilities
(defun valid-canvas-id-p (canvas-id)
  "Check if canvas ID is valid"
  (and (stringp canvas-id)
       (cl-ppcre:scan "^[a-zA-Z0-9-]+$" canvas-id)))

(defun generate-canvas-id ()
  "Generate a unique canvas ID"
  (format nil "canvas-~A" (subseq (generate-uuid) 0 8)))