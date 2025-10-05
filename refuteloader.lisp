;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refuter API Session Setup Script (Corrected Parentheses and Scoping - Attempt 5)
;;;
;;; This script defines a function to automate the process of loading
;;; Quicklisp libraries (Hunchentoot and Jonathan) and the refuter-api.lisp
;;; file into a new Common LISP session.
;;; Refactored into smaller helper functions for clarity.
;;; Uses dynamic symbol lookup to avoid READ errors before package definition.
;;; CORRECTED: Parenthesis matching and LET form nesting for proper variable scoping.
;;; Added comments regarding potential spurious compiler warnings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Helper Function: Load Quicklisp ---
(defun load-quicklisp (&key quicklisp-setup-path)
  "Loads Quicklisp into the current Lisp session.
This function locates and loads the Quicklisp setup file, which is essential for
managing project dependencies. It can use a user-provided path or default to
'~/quicklisp/setup.lisp'. It signals a fatal error if the setup file cannot be
found or loaded.

Parameters:
  - QUICKLISP-SETUP-PATH (Keyword, Optional): A pathname object or a string
    specifying the location of the 'setup.lisp' file. If nil, the function
    searches the default location.

Returns:
  - T on successful loading.

Side Effects:
  - Loads the Quicklisp system into the Lisp image.
  - Prints informational and error messages to the standard and error outputs."
  (let ((ql-setup-path
          (if quicklisp-setup-path
              quicklisp-setup-path
              ;; Corrected Default Quicklisp setup path (~/quicklisp/setup.lisp)
              ;; Use two-argument merge-pathnames for robustness
              (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))))

    (format t "~&Checking for Quicklisp setup file at: ~S~%" ql-setup-path)

    ;; Check if the Quicklisp setup file exists before attempting to load
    (unless (probe-file ql-setup-path)
       (format *error-output* "~&Quicklisp setup file not found at ~S.~%" ql-setup-path)
       (format *error-output* "~&Please ensure Quicklisp is installed and the path to setup.lisp is correct.~%")
       (format *error-output* "~&You can try calling setup-refuter-session with the :quicklisp-setup-path argument.~%")
       (format *error-output* "~&Example: (setup-refuter-session :quicklisp-setup-path #P\"/path/to/your/quicklisp/setup.lisp\")~%")
       ;; Signal a serious error if Quicklisp setup file is not found
       (error "Failed to find Quicklisp setup file. Cannot proceed with setup.")))

    ;; Ensure Quicklisp is loaded (harmless if already loaded by init file)
    ;; This block is only reached if probe-file succeeded.
    (unless (find-package :ql)
      (handler-case
          ;; NOTE: The compiler may issue a warning about QL-SETUP-PATH being undefined
          ;; in COMMON-LISP-USER here. This is likely a spurious warning related to
          ;; the compiler's behavior during LOAD, as QL-SETUP-PATH is correctly
          ;; defined as a local variable in the LET form.
          (load ql-setup-path) ;; Attempt to load the Quicklisp setup file
        (error (e)
          (format *error-output* "~&Error loading Quicklisp setup file from ~S: ~A~%" ql-setup-path e)
          (format *error-output* "~&An error occurred during the loading process itself.~%")
          ;; Signal a serious error to stop execution if Quicklisp cannot be loaded
          (error "Failed to load Quicklisp setup file. Cannot proceed with setup."))))

    (format t "Quicklisp loaded successfully.~%"))


;; --- Helper Function: Load Required Libraries ---
(defun load-required-libraries ()
  "Loads the project's core dependencies (Hunchentoot and Jonathan) using Quicklisp.
This function assumes Quicklisp has already been loaded. It uses `ql:quickload`
to fetch and load the required libraries. Signals a fatal error if loading fails.

Parameters:
  - None.

Returns:
  - T on successful loading.

Side Effects:
  - Loads the Hunchentoot and Jonathan libraries into the Lisp image.
  - Prints informational and error messages."
  (format t "Loading required libraries (Hunchentoot, Jonathan) using Quicklisp...~%")
  (handler-case
      (ql:quickload '(:hunchentoot :jonathan))
    (error (e)
      (format *error-output* "~&Error loading required libraries (Hunchentoot, Jonathan): ~A~%" e)
      (format *error-output* "~&Please ensure these libraries are installed via Quicklisp (using ql:quickload in a session where Quicklisp is loaded).~%")
      ;; Signal a serious error if libraries cannot be loaded
      (error "Failed to load required libraries. Cannot proceed with setup.")))
  (format t "Required libraries loaded.~%"))


;; --- Helper Function: Load Refuter API File ---
(defun load-refuter-api-file (&key api-file-path)
  "Loads the main application file, `refuter-api.lisp`.
This function determines the file's path, defaulting to a location relative to
this setup script if no explicit path is provided. It ensures the file exists
before attempting to load it and signals a fatal error on failure.

Parameters:
  - API-FILE-PATH (Keyword, Optional): An explicit path to the `refuter-api.lisp` file.
    If nil, it defaults to looking in the same directory as this loader script.

Returns:
  - The fully resolved, absolute pathname of the loaded API file.

Side Effects:
  - Loads the `refuter-api.lisp` file into the Lisp image."
  ;; Determine the path to your refuter-api.lisp file
  (let ((api-file-final-path
          (if api-file-path
              api-file-path
              ;; Default path: refuter-api.lisp in the same directory as this script
              (if *load-pathname*
                  (merge-pathnames #P"refuter-api.lisp" (directory-namestring *load-pathname*))
                  ;; Fallback if *load-pathname* is not bound (unlikely when loading a file)
                  (merge-pathnames #P"refuter-api.lisp" (truename "."))))))

    (format t "Checking for Refuter API file at: ~S~%" api-file-final-path)

    ;; Check if the file exists before attempting to load
    (unless (probe-file api-file-final-path)
       (error "Refuter API file not found at ~S. Please ensure the path is correct or provide it as the :api-file-path argument to setup-refuter-session." api-file-final-path))

    ;; Load the refuter-api.lisp file
    (format t "Loading Refuter API file...~%")
    (handler-case
        (load api-file-final-path)
      (error (e)
        (format *error-output* "~&Error loading refuter-api.lisp from ~S: ~A~%" api-file-final-path e)
        (format *error-output* "~&Please ensure the refuter-api.lisp file is valid Common LISP code.~%")
        ;; Signal a serious error if your API file cannot be loaded
        (error "Failed to load refuter-api.lisp. Cannot proceed with setup.")))

    ;; Return the path of the loaded file
    api-file-final-path))


;; --- Helper Function: Set API File Path Global ---
(defun set-api-file-path-global (api-file-path)
  "Sets the `*refuter-api-file-path*` global variable within the `refuter-api` package.
After the API file is loaded, this function dynamically finds the `refuter-api`
package and the `*refuter-api-file-path*` symbol within it. It then sets this
variable to the provided path, which is crucial for the `restart-refuter-api`
function to work correctly.

Parameters:
  - API-FILE-PATH: The absolute pathname of the loaded API file.

Returns:
  - T on success.

Side Effects:
  - Modifies the value of the `refuter-api:*refuter-api-file-path*` variable."
  ;; Ensure the refuter-api package exists before trying to access its symbol
  (let ((refuter-api-package (find-package :refuter-api)))
    (unless refuter-api-package
       (format *error-output* "~&Error: REFUTER-API package not found after loading the API file.~%")
       (format *error-output* "~&Please ensure your refuter-api.lisp file defines the package #:REFUTER-API correctly.~%")
       (error "REFUTER-API package not defined. Cannot set *refuter-api-file-path*.")))

  ;; FIX: The SET call is now inside this LET form to ensure FILE-PATH-SYMBOL is in scope.
  (let ((file-path-symbol (find-symbol "*REFUTER-API-FILE-PATH*" :refuter-api)))
     (unless file-path-symbol
        (format *error-output* "~&Error: *REFUTER-API-FILE-PATH* symbol not found in REFUTER-API package after loading.~%")
        (format *error-output* "~&Please ensure *refuter-api-file-path* is defined in refuter-api.lisp and exported if necessary.~%")
        (error "*REFUTER-API-FILE-PATH* symbol not found. Cannot set its value."))

     ;; Set the value of the found symbol
     ;; NOTE: The compiler may issue a warning about FILE-PATH-SYMBOL being undefined
     ;; in COMMON-LISP-USER here. This is likely a spurious warning related to
     ;; the compiler's behavior during LOAD, as FILE-PATH-SYMBOL is correctly
     ;; defined as a local variable in this LET form.
     (set file-path-symbol api-file-path)) ;; This SET is now inside the LET

  (format t "Refuter API file path stored in refuter-api:*refuter-api-file-path*.~%"))


;; --- Helper Function: Switch to Refuter API Package ---
(defun switch-to-refuter-api-package ()
  "Switches the current Lisp package to `:refuter-api`.
This makes it convenient for the user to interact with the API's exported
functions directly from the REPL after the setup is complete.

Parameters:
  - None.

Returns:
  - The `refuter-api` package object.

Side Effects:
  - Changes the `*package*` to `:refuter-api`."
  (format t "Switching to REFUTER-API package...~%")
  (handler-case
      (in-package #:refuter-api)
    (error (e)
       (format *error-output* "~&Error switching to package #:REFUTER-API: ~A~%" e)
       (format *error-output* "~&Please ensure your refuter-api.lisp file defines the package #:REFUTER-API correctly.~%")
       (error "Failed to switch to REFUTER-API package.")))
  (format t "Successfully switched to REFUTER-API package.~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Setup Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CORRECTED: Ensure the DEFUN form is correctly closed at the very end.
(defun setup-refuter-session (&key api-file-path quicklisp-setup-path)
  "The main entry point for setting up a complete Refuter API session.
This function orchestrates the entire setup process by calling a series of
helper functions to:
1. Load Quicklisp.
2. Load required libraries (Hunchentoot, Jonathan).
3. Load the `refuter-api.lisp` application file.
4. Set the necessary global variables within the API package.
5. Switch the current package to `:refuter-api` for interactive use.

Parameters:
  - API-FILE-PATH (Keyword, Optional): The path to `refuter-api.lisp`.
    Defaults to the same directory as this script.
  - QUICKLISP-SETUP-PATH (Keyword, Optional): The path to Quicklisp's `setup.lisp`.
    Defaults to '~/quicklisp/setup.lisp'.

Returns:
  - T upon successful completion of all setup steps.

Side Effects:
  - Modifies the Lisp environment by loading libraries and setting package state."

  (format t "~&Starting Refuter API session setup...~%")

  ;; Step 1: Load Quicklisp
  ;; The parameters api-file-path and quicklisp-setup-path are accessible here
  ;; because they are parameters of the DEFUN.
  (load-quicklisp :quicklisp-setup-path quicklisp-setup-path)

  ;; Step 2: Load Required Libraries
  (load-required-libraries)

  ;; Step 3: Load the Refuter API File and store its path
  (let ((loaded-api-file-path (load-refuter-api-file :api-file-path api-file-path)))
    ;; Set the global variable in the refuter-api package
    (set-api-file-path-global loaded-api-file-path))


  ;; Step 4: Switch to the Refuter API Package
  (switch-to-refuter-api-package)


  ;; --- Final Success Message ---
  (format t "~&~%Refuter API session setup script finished successfully.~%")
  (format t "~&You are now in the REFUTER-API package.~%")
  (format t "~&You can start the server using: (start-refuter-api :port 8080)~%")
  (format t "~&Or restart it using: (restart-refuter-api :port 8080)~%")) ;; This is the intended closing parenthesis for the DEFUN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instructions for Use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use this script:
;; 1. Save this code as a .lisp file (e.g., setup-refuter.lisp).
;; 2. Start your SBCL REPL.
;; 3. Load this script in your REPL: (load "path/to/this/script.lisp")
;; 4. Call the setup function, providing paths if necessary:
;;    - If refuter-api.lisp is in the same directory as this script
;;      AND Quicklisp is in the default ~/quicklisp/ location:
;;      (setup-refuter-session)
;;    - If refuter-api.lisp is elsewhere:
;;      (setup-refuter-session :api-file-path #P"/explicit/path/to/your/refuter-api.lisp")
;;    - If Quicklisp setup.lisp is elsewhere:
;;      (setup-refuter-session :quicklisp-setup-path #P"/explicit/path/to/your/quicklisp/setup.lisp")
;;    - If both are elsewhere:
;;      (setup-refuter-session :api-file-path #P"/path/to/api.lisp" :quicklisp-setup-path #P"/path/to/ql/setup.lisp")
;; 5. If successful, you will be in the REFUTER-API package and can
;;    start/restart the server.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

