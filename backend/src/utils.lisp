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

(defun camel-case-to-kebab-case (string)
  "Convert camelCase to kebab-case: sessionId -> session-id"
  (let ((result '())
        (prev-lower nil))
    (loop for char across string
          do (cond
               ;; If uppercase and previous was lowercase, add hyphen before
               ((and (upper-case-p char) prev-lower)
                (push #\- result)
                (push (char-downcase char) result)
                (setf prev-lower nil))
               ;; If uppercase, just downcase
               ((upper-case-p char)
                (push (char-downcase char) result)
                (setf prev-lower nil))
               ;; If lowercase, keep as is
               (t
                (push char result)
                (setf prev-lower t))))
    (coerce (nreverse result) 'string)))

(defun normalize-json-keys (object)
  "Convert Jonathan's pipe-escaped keywords to regular keywords and plist to alist.
   Also converts camelCase to kebab-case for Common Lisp conventions."
  (cond
    ((null object) nil)
    ;; Handle plists (convert to alist)
    ((and (consp object) (keywordp (car object)))
     (plist-to-alist
      (loop for (key value) on object by #'cddr
            for key-str = (symbol-name key)
            for kebab-str = (camel-case-to-kebab-case key-str)
            collect (intern (string-upcase kebab-str) :keyword)
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

(defun alist-p (obj)
  "Check if object is an association list"
  (and (listp obj)
       (not (null obj))
       (every #'consp obj)
       ;; Make sure it's not a nested alist value
       (every (lambda (pair) (keywordp (car pair))) obj)))

(defun hash-table-to-alist (hash)
  "Convert hash table to association list"
  (let ((result '()))
    (maphash (lambda (key value)
               (push (cons (intern (string-upcase key) :keyword) value) result))
             hash)
    (nreverse result)))

(defun convert-to-hash (obj)
  "Recursively convert alists to hash tables for Jonathan"
  (cond
    ;; If it's a hash table, convert to alist first then to hash
    ((hash-table-p obj)
     (convert-to-hash (hash-table-to-alist obj)))
    ;; If it's an alist, convert to hash table
    ((and (listp obj)
          (not (null obj))
          (every #'consp obj)
          (every (lambda (pair) (keywordp (car pair))) obj))
     (let ((hash (make-hash-table :test 'equal)))
       (dolist (pair obj)
         (setf (gethash (string-downcase (symbol-name (car pair))) hash)
               (convert-to-hash (cdr pair))))
       hash))
    ;; If it's a list of non-alists, convert each element
    ((listp obj)
     (mapcar #'convert-to-hash obj))
    ;; Otherwise return as-is (atomic values)
    (t obj)))

(defun to-json-string (object)
  "Convert object to JSON string"
  (jonathan:to-json (convert-to-hash object)))

;;; HTTP utilities - Removed old Hunchentoot functions
;;; See app.lisp for Clack equivalents:
;;;  - parse-env-body (was get-json-body)
;;;  - clack-json-response (was json-response)
;;;  - clack-error-response (was error-response)
;;;  - clack-success-response (was success-response)
;;; CORS is now handled by Lack middleware in make-app

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