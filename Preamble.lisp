;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Preamble - Main Entry Point
;;;
;;; This file serves as the main entry point for the Weaver system. It is
;;; responsible for loading all necessary Lisp files in the correct order,
;;; setting up dependencies, and starting the required services, including
;;; the Refuter API and test servers.
;;;
;;; To run the entire system, this file should be loaded into a Common Lisp
;;; environment.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "QHJ.lisp")
(setup-dependencies)
(load "STT.lisp")

(in-package #:refuter-api)
(setf *frontend-directory* #P"/home/madsci/Documents/Weaver/")
(start-refuter-api :port 8080)

(strace-test:restart-strace-test-server)


(load "SFT.lisp")
(static-file-tests:start-test-server :static-dir #P"/home/madsci/Documents/Weaver/")