(asdf:defsystem #:weaver
  :description "A system for the Weaver project, including a prover, refuter, and API server."
  :author "Jules"
  :license "Proprietary"
  :version "1.0.0"
  :serial t
  :depends-on (#:hunchentoot #:jonathan)
  :components ((:module "src"
                :components
                ((:file "prover")
                 (:file "refuter")
                 (:file "api-server")))))