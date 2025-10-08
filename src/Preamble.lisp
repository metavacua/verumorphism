;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project Preamble
;;;
;;; This script loads the Weaver system using ASDF and Quicklisp,
;;; then starts the API server.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure Quicklisp is loaded (if not already in the image)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

;; Add the project root directory to ASDF's central registry to help it find the .asd file.
;; *default-pathname-defaults* is the directory where the Lisp process was started.
(push *default-pathname-defaults* asdf:*central-registry*)

;; Load the entire Weaver system via Quicklisp
(ql:quickload :weaver)

;; Start the server, setting the frontend directory to the project root.
(api-server:start-api-server :port 8080 :frontend-dir *default-pathname-defaults*)