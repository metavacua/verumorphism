;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hunchentoot Static File Serving Test Script (Exported start-test-server)
;;;
;;; This script sets up a minimal Hunchentoot server to test
;;; different methods of serving static files, including
;;; absolute and relative path handling.
;;;
;;; Designed for focused experimentation on directory modeling issues.
;;; FIXED: Resolved NAME-CONFLICT by using IMPORT-FROM for SB-POSIX symbols.
;;; FIXED: Exported START-TEST-SERVER to make it accessible from other packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:static-file-tests
  (:use #:cl #:hunchentoot)
  (:import-from #:sb-posix #:chdir #:getcwd)
  (:documentation "This package provides a test environment for experimenting with
Hunchentoot's static file serving capabilities. It is designed to help diagnose
and understand how Hunchentoot handles absolute and relative file paths,
particularly in relation to the Lisp process's current working directory (CWD).")
  (:export #:start-test-server #:stop-test-server #:*static-base-directory*))

(in-package #:static-file-tests)

;; Global variable to hold the server instance
(defvar *test-server* nil
  "Holds the active Hunchentoot acceptor instance for the test server.
This variable is set by `start-test-server` and used by `stop-test-server`.")

;; Global variable to hold the base directory for static files
(defvar *static-base-directory* nil
  "Specifies the absolute path to the directory containing the static test
files (e.g., 'index.html', 'test-absolute.html'). This path is set by
`start-test-server`.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handler to serve a file using an ABSOLUTE path constructed from the base directory
(define-easy-handler (serve-absolute :uri "/test-absolute") ()
  "A test handler that attempts to serve the 'test-absolute.html' file
using a fully specified, absolute pathname. This is used to verify the most
reliable method of file serving."
  (setf (content-type*) "text/html") ;; Assuming HTML test files

  (if *static-base-directory*
      (let* ((file-name "test-absolute.html")
             ;; Construct the absolute path by merging the file name with the base directory
             (absolute-path (merge-pathnames file-name *static-base-directory*)))

        (format t "~&Handler /test-absolute: Attempting to serve absolute path ~S~%" absolute-path)

        ;; Attempt to serve the file using the absolute path
        (handler-case
            (handle-static-file absolute-path)
          (error (e)
            (setf (return-code*) +http-not-found+) ;; Return 404 on error
            (format nil "Error serving file ~S: ~A" absolute-path e))))

      ;; Return error if base directory is not set
      (progn
        (setf (return-code*) +http-internal-server-error+)
        "Static base directory (*static-base-directory*) is not set.")))

;; Handler to serve a file using a RELATIVE path, relying on CWD
;; NOTE: This tests the hypothesis that handle-static-file might resolve
;; relative paths against the process's CWD.
(define-easy-handler (serve-relative :uri "/test-relative") ()
  "A test handler that attempts to serve the 'test-relative.html' file
using a relative pathname. This test is designed to see if Hunchentoot
resolves the path against the process's current working directory (CWD),
which is explicitly set by `start-test-server`."
  (setf (content-type*) "text/html") ;; Assuming HTML test files

  (if *static-base-directory*
      (let* ((file-name "test-relative.html")
             ;; Use a relative pathname directly
             (relative-path (pathname file-name)))

        (format t "~&Handler /test-relative: Attempting to serve relative path ~S from CWD (~A)~%"
                relative-path (getcwd)) ;; Log CWD for context

        ;; Attempt to serve the file using the relative path
        (handler-case
            (handle-static-file relative-path)
          (error (e)
            (setf (return-code*) +http-not-found+) ;; Return 404 on error
            (format nil "Error serving file ~S: ~A" relative-path e))))

      ;; Return error if base directory is not set
      (progn
        (setf (return-code*) +http-internal-server-error+)
        "Static base directory (*static-base-directory*) is not set.")))

;; Handler to serve the root path, defaulting to index.html using absolute path
(define-easy-handler (serve-root :uri "/") ()
  "A test handler for the root URI ('/'). It attempts to serve 'index.html'
using a fully specified, absolute pathname."
  (setf (content-type*) "text/html") ;; Assuming HTML test files

  (if *static-base-directory*
      (let* ((file-name "index.html")
             ;; Construct the absolute path for index.html
             (absolute-path (merge-pathnames file-name *static-base-directory*)))

        (format t "~&Handler /: Attempting to serve absolute path ~S~%" absolute-path)

        ;; Attempt to serve index.html using the absolute path
        (handler-case
            (handle-static-file absolute-path)
          (error (e)
            (setf (return-code*) +http-not-found+) ;; Return 404 on error
            (format nil "Error serving file ~S: ~A" absolute-path e))))

      ;; Return error if base directory is not set
      (progn
        (setf (return-code*) +http-internal-server-error+)
        "Static base directory (*static-base-directory*) is not set.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server Control Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-server (&key (port 8080) (static-dir nil))
  "Starts the Hunchentoot test server for static file experiments.

This function sets the base directory for static files, changes the Lisp
process's current working directory to that directory, and starts a Hunchentoot
server on the specified port.

Parameters:
  - PORT (Keyword, Optional): The port number for the server. Defaults to 8080.
  - STATIC-DIR (Keyword, Optional): The absolute path to the directory containing
    the static test files. This is a required parameter for the server to function correctly.

Returns:
  - The Hunchentoot acceptor instance on success, NIL on failure.

Side Effects:
  - Sets `*static-base-directory*`.
  - Changes the current working directory.
  - Starts the Hunchentoot server in a new thread and sets `*test-server*`."
  (format t "~&Starting static file test server...~%")

  ;; Set the static base directory and ensure it's an absolute directory pathname
  (if static-dir
      (let ((dir-string (namestring (truename static-dir)))) ;; Use truename to resolve symlinks etc.
        (unless (char= (aref dir-string (1- (length dir-string))) #\/)
          (setf dir-string (concatenate 'string dir-string "/")))
        (setf *static-base-directory* (pathname dir-string))
        (format t "~&Static base directory set to: ~S~%" *static-base-directory*))
      (progn
        (setf *static-base-directory* nil)
        (format t "~&WARNING: Static base directory not provided. Static file serving will fail.~%")))


  ;; Change the process's current working directory to the static base directory.
  ;; This is necessary for testing relative path handling.
  (if *static-base-directory*
      (handler-case
          (chdir (namestring *static-base-directory*))
        (error (e)
          (format *error-output* "~&Error changing current directory to ~S: ~A~%"
                  (namestring *static-base-directory*) e)
          (format *error-output* "~&Relative path serving tests may fail.~%")))
      (format t "~&Skipping CWD change as static base directory is not set.~%"))


  (format t "Attempting to start server on port ~A, CWD is ~A...~%"
          port (getcwd))

  (handler-case
      (setf *test-server* (start (make-instance 'acceptor :port port)))
    (usocket:address-in-use-error (e)
      (format *error-output* "~&Error: Port ~A is already in use.~%" port)
      (format *error-output* "~&Original error: ~A~%" e)
      (setf *test-server* nil)
      nil)
    (error (e)
      (format *error-output* "~&An unexpected error occurred during server startup: ~A~%" e)
      (setf *test-server* nil)
      nil)))

(defun stop-test-server ()
  "Stops the running static file test server.
It checks if `*test-server*` is running and stops it if it is.

Side Effects:
  - Stops the Hunchentoot server.
  - Sets `*test-server*` to NIL."
  (when *test-server*
    (format t "~&Stopping test server...~%")
    (stop *test-server*)
    (setf *test-server* nil)
    (format t "~&Test server stopped.~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use this test script:
;;
;; 1. Save this code as a .lisp file (e.g., static-test-server.lisp).
;;
;; 2. Create a directory to hold your test HTML files (e.g., 'static-files').
;;    Inside this directory, create three simple HTML files:
;;    - index.html (e.g., <h1>Root Test</h1>)
;;    - test-absolute.html (e.g., <h1>Absolute Path Test</h1>)
;;    - test-relative.html (e.g., <h1>Relative Path Test</h1>)
;;
;; 3. Start your SBCL REPL.
;;
;; 4. Load the dependencies using the minimal setup script:
;;    (load "path/to/your/setup-deps.lisp")
;;    (setup-dependencies)
;;
;; 5. Load this test script:
;;    (load "path/to/static-test-server.lisp")
;;
;; 6. Start the server, providing the ABSOLUTE path to your 'static-files' directory:
;;    (static-file-tests:start-test-server :static-dir #P"/absolute/path/to/your/static-files/")
;;    Replace "/absolute/path/to/your/static-files/" with the actual path.
;;    Make sure the path is correct and refers to the directory containing the HTML files.
;;
;; 7. Open your web browser and visit the following URLs:
;;    - http://localhost:8080/        (Tests serving index.html using absolute path)
;;    - http://localhost:8080/test-absolute (Tests serving test-absolute.html using absolute path)
;;    - http://localhost:8080/test-relative (Tests serving test-relative.html using relative path)
;;
;; 8. Observe the output in your SBCL REPL and the browser.
;;    - The REPL will show which path the server is attempting to serve for each request.
;;    - The browser will show the content of the HTML file if successful (200 OK)
;;      or a "Not Found" message (404) if it fails.
;;
;; This experiment will provide clear evidence of how Hunchentoot handles
;; absolute and relative paths for static file serving in your specific environment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

