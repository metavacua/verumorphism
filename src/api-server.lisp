;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unified API Server (Common LISP with Hunchentoot)
;;;
;;; Serves a static frontend and provides API endpoints for the
;;; Prover and Refuter applications.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:api-server
  (:use #:cl #:hunchentoot #:jonathan #:prover #:refuter)
  (:import-from #:sb-posix #:chdir #:getcwd #:sigint)
  (:import-from #:sb-sys #:enable-interrupt)
  (:import-from #:sb-ext #:*load-truename*)
  (:export #:start-api-server #:stop-api-server #:restart-api-server))

(in-package #:api-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *frontend-directory* nil "The absolute path to the frontend files.")
(defvar *api-server* nil "Holds the current Hunchentoot acceptor instance.")
(defvar *api-server-file-path* nil "Stores the absolute path of this file.")
(defvar *last-started-port* 8080 "Stores the last used port.")
(defvar *last-started-frontend-dir* nil "Stores the last used frontend directory.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automation: Set script path on load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *load-truename*
  (setf *api-server-file-path* (truename *load-truename*))
  (format t "~&[INFO] API Server script path set to: ~S~%" *api-server-file-path*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server Control Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stop-api-server ()
  "Stops the running API server."
  (when *api-server*
    (format t "~&[INFO] Stopping API server...~%")
    (stop *api-server*)
    (setf *api-server* nil)
    (format t "~&[INFO] API server stopped.~%")))

(defun handle-termination-signal (signal &rest args)
  "Handles termination signals like SIGINT."
  (declare (ignore args))
  (format t "~&[INFO] Received signal ~A. Stopping server...~%" signal)
  (stop-api-server)
  (sb-ext:exit :code 0))

(defun start-api-server (&key (port 8080) (frontend-dir nil frontend-dir-provided-p))
  "Starts the Hunchentoot web server."
  (format t "~&[INFO] Starting API server...~%")

  (let ((effective-frontend-dir
          (if frontend-dir-provided-p
              frontend-dir
              (if *api-server-file-path*
                  (make-pathname :directory (pathname-directory *api-server-file-path*))
                  (getcwd)))))
    (let ((dir-string (namestring (truename effective-frontend-dir))))
      (unless (char= (aref dir-string (1- (length dir-string))) #\/)
        (setf dir-string (concatenate 'string dir-string "/")))
      (setf *frontend-directory* (pathname dir-string))
      (format t "~&[INFO] Frontend directory set to: ~S~%" *frontend-directory*)))

  (when *frontend-directory*
    (chdir (namestring *frontend-directory*)))

  (format t "[INFO] Attempting to start server on port ~A...~%" port)

  (handler-case
      (progn
        (setf *api-server* (start (make-instance 'easy-acceptor
                                                 :port port
                                                 :document-root *frontend-directory*)))
        (setf *last-started-port* port)
        (setf *last-started-frontend-dir* *frontend-directory*)
        (format t "~&[INFO] Server started on port ~A. Press Ctrl+C to stop.~%" port)
        (sb-sys:enable-interrupt sb-unix:sigint #'handle-termination-signal)
        (sb-sys:enable-interrupt sb-unix:sigterm #'handle-termination-signal)
        (loop (sleep 1)))
    (usocket:address-in-use-error ()
      (format *error-output* "~&[ERROR] Port ~A is already in use.~%" port)
      (setf *api-server* nil))
    (error (e)
      (format *error-output* "~&[ERROR] An unexpected error occurred: ~A~%" e)
      (setf *api-server* nil))))

(defun restart-api-server (&key (port nil port-provided-p) (frontend-dir nil frontend-dir-provided-p))
  "Restarts the API server."
  (format t "~&[INFO] Restarting API server...~%")
  (when *api-server*
    (stop-api-server))
  (let ((restart-port (if port-provided-p port *last-started-port*))
        (restart-frontend-dir (if frontend-dir-provided-p frontend-dir *last-started-frontend-dir*)))
    (when *api-server-file-path*
      (format t "~&[INFO] Reloading source file: ~S~%" *api-server-file-path*)
      (load *api-server-file-path*))
    (start-api-server :port restart-port :frontend-dir restart-frontend-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Endpoint Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-logic-request (logic-function)
  "Helper function to handle requests for the prover and refuter."
  (unless (eq (hunchentoot:request-method*) :post)
    (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
    (setf (hunchentoot:header-out "Allow") "POST")
    (return-from handle-logic-request "Method Not Allowed. Only POST requests are accepted."))

  (setf (content-type*) "application/json")
  (let ((request-body (raw-post-data :force-text t)))
    (handler-case
        (let* ((json-data (jojo:parse request-body))
               (formula-str (gethash "formula" json-data)))
          (if formula-str
              (let ((formula (read-from-string formula-str)))
                (let ((result (funcall logic-function formula)))
                  (jojo:to-json `(:obj ("status" . "success") ("result" . ,result)))))
              (progn
                (setf (return-code*) hunchentoot:+http-bad-request+)
                (jojo:to-json '(:obj ("status" . "error") ("message" . "Missing 'formula' key in request."))))))
      (error (e)
        (setf (return-code*) hunchentoot:+http-internal-server-error+)
        (jojo:to-json `(:obj ("status" . "error") ("message" . ,(format nil "Error processing request: ~A" e))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Endpoints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-easy-handler (prove-endpoint :uri "/prove") ()
  "API endpoint to prove a formula."
  (handle-logic-request #'prover:run-prover))

(define-easy-handler (refute-endpoint :uri "/refute") ()
  "API endpoint to refute a formula."
  (handle-logic-request #'refuter:run-refuter))

(define-easy-handler (stop-server-endpoint :uri "/stop-server") ()
  "Web endpoint to stop the server gracefully."
  (stop-api-server)
  (jojo:to-json '(:obj ("status" . "success") ("message" . "Server stop requested."))))

(define-easy-handler (restart-server-endpoint :uri "/restart-server") ()
  "Web endpoint to restart the server."
  (restart-api-server)
  (jojo:to-json '(:obj ("status" . "success") ("message" . "Server restart requested."))))