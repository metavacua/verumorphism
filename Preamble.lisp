(load "QHJ.lisp")
(setup-dependencies)
(load "STT.lisp")

(in-package #:refuter-api)
(setf *frontend-directory* *default-pathname-defaults*)
(start-refuter-api :port 8080)

(strace-test:restart-strace-test-server)


(load "SFT.lisp")
(static-file-tests:start-test-server :static-dir *default-pathname-defaults*)
