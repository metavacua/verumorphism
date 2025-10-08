;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RelNet Abstraction of RelNet Weaver Self-Testing System (LISP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *relnet-self-testing-abstraction*
  '(
    ;;; Node Types ;;;
    (:node-types (function data-structure metric axiom rule thread orchestration test-function test-runner test-summary test-category-list assertion-function))

    ;;; Edge Types ;;;
    (:edge-types (uses-data calls implements-rule manages-threads updates-metric defines-axiom defines-rule orchestrates tests asserts summarizes includes-test updates-summary formats-output))

    ;;; Nodes ;;;
    (:nodes
     (
      ;;; Data Structures ;;;
      (node :name knowledge-base :type data-structure :description "Global Knowledge Base (*knowledge-base*)")
      (node :name relnet-node-class :type data-structure :description "Data structure class for RelNet nodes (relnet-node)")
      (node :name test-suite-summary-data :type data-structure :description "Data structure for test suite summary (*test-suite-summary*)")

      ;;; Complexity Metrics ;;;
      (node :name axiom-applications-count :type metric :description "Counter for axiom applications (*axiom-applications-count*)")
      (node :name rule-applications-count :type metric :description "Counter for rule applications (*rule-applications-count*)")

      ;;; Axioms ;;;
      (node :name axiom-con-r :type axiom :description "Proof Axiom Function (axiom-con-r)")
      (node :name axiom-incon-l :type axiom :description "Refutation Axiom Function (axiom-incon-l)")

      ;;; Rules - Dependence ;;;
      (node :name rule-dependence-r :type rule :description "Dependence Right Rule Function (rule-dependence-r)")
      (node :name rule-dependence-l :type rule :description "Dependence Left Rule Function (rule-dependence-l)")

      ;;; Rules - Independence ;;;
      (node :name rule-independence-r :type rule :description "Independence Right Rule Function (rule-independence-r) - THREADED")
      (node :name rule-independence-l :type rule :description "Independence Left Rule Function (rule-independence-l) - THREADED")

      ;;; Thread Functions ;;;
      (node :name proof-thread-function :type thread :description "Proof Thread Function (proof-thread-function)")
      (node :name refutation-thread-function :type thread :description "Refutation Thread Function (refutation-thread-function)")

      ;;; Orchestration Functions ;;;
      (node :name run-prover :type orchestration :description "Main Prover Run Function (run-prover)")
      (node :name initialize-knowledge-base :type orchestration :description "Knowledge Base Initialization Function (initialize-knowledge-base)")
      (node :name reset-complexity-counters :type orchestration :description "Complexity Counters Reset Function (reset-complexity-counters)")
      (node :name main-entry-point :type orchestration :description "Main Entry Point Function (main)")

      ;;; Test Functions - Axioms ;;;
      (node :name test-axiom-con-r-func :type test-function :description "Test function for axiom-con-r (test-axiom-con-r)")
      (node :name test-axiom-incon-l-func :type test-function :description "Test function for axiom-incon-l (test-axiom-incon-l)")

      ;;; Test Functions - Dependence Rules ;;;
      (node :name test-rule-dependence-r-positive-func :type test-function :description "Positive test function for rule-dependence-r (test-rule-dependence-r-positive)")
      (node :name test-rule-dependence-r-negative-func :type test-function :description "Negative test function for rule-dependence-r (test-rule-dependence-r-negative)")
      (node :name test-rule-dependence-l-positive-func :type test-function :description "Positive test function for rule-dependence-l (test-rule-dependence-l-positive)")
      (node :name test-rule-dependence-l-negative-func :type test-function :description "Negative test function for rule-dependence-l (test-rule-dependence-l-negative)")

      ;;; Test Functions - Independence Rules ;;;
      (node :name test-rule-independence-r-premise1-con-func :type test-function :description "Test for rule-independence-r premise1 con (test-rule-independence-r-premise1-con)")
      (node :name test-rule-independence-r-premise2-con-func :type test-function :description "Test for rule-independence-r premise2 con (test-rule-independence-r-premise2-con)")
      (node :name test-rule-independence-r-negative-func :type test-function :description "Negative test for rule-independence-r (test-rule-independence-r-negative)")
      (node :name test-rule-independence-r-early-exit-premise1-func :type test-function :description "Early exit test rule-independence-r premise1 (test-rule-independence-r-early-exit-premise1)")
      (node :name test-rule-independence-l-premise1-incon-func :type test-function :description "Test for rule-independence-l premise1 incon (test-rule-independence-l-premise1-incon)")
      (node :name test-rule-independence-l-premise2-incon-func :type test-function :description "Test for rule-independence-l premise2 incon (test-rule-independence-l-premise2-incon)")
      (node :name test-rule-independence-l-negative-func :type test-function :description "Negative test for rule-independence-l (test-rule-independence-l-negative)")
      (node :name test-rule-independence-l-early-exit-premise1-func :type test-function :description "Early exit test rule-independence-l premise1 (test-rule-independence-l-early-exit-premise1)")


      ;;; Test Runner Functions ;;;
      (node :name run-tests-func :type test-runner :description "Generic test runner function (run-tests)")
      (node :name run-all-tests-func :type test-runner :description "Runs all test categories (run-all-tests)")

      ;;; Test Summary Functions ;;;
      (node :name reset-test-summary-func :type test-summary :description "Resets test summary (reset-test-summary)")
      (node :name update-test-summary-func :type test-summary :description "Updates test summary (update-test-summary)")
      (node :name iterate-test-summary-func :type test-summary :description "Iterates and prints test summary (iterate-test-summary)")
      (node :name format-test-result-func :type test-summary :description "Formats test result output (format-test-result)")

      ;;; Assertion Function ;;;
      (node :name assert-result-func :type assertion-function :description "Generic assertion function (assert-result)")


      ;;; Test Category Lists ;;;
      (node :name axiom-tests-list :type test-category-list :description "List of axiom test functions (*axiom-tests*)")
      (node :name dependence-rule-tests-list :type test-category-list :description "List of dependence rule test functions (*dependence-rule-tests*)")
      (node :name independence-rule-tests-list :type test-category-list :description "List of independence rule test functions (*independence-rule-tests*)")


     )
    )

    ;;; Edges (Relationships) ;;;
    (:edges
     (
      ;;; Axioms update metrics (same as before) ;;;
      (edge :from axiom-con-r :to axiom-applications-count :type updates-metric :description "con_R increments axiom counter")
      (edge :from axiom-incon-l :to axiom-applications-count :type updates-metric :description "incon_L increments axiom counter")

      ;;; Rules update metrics (same as before) ;;;
      (edge :from rule-dependence-r :to rule-applications-count :type updates-metric :description "dependenceR increments rule counter")
      (edge :from rule-dependence-l :to rule-applications-count :type updates-metric :description "dependenceL increments rule counter")
      (edge :from rule-independence-r :to rule-applications-count :type updates-metric :description "independenceR increments rule counter")
      (edge :from rule-independence-l :to rule-applications-count :type updates-metric :description "independenceL increments rule counter")


      ;;; Rules call axioms (in base case for prototype) (same as before) ;;;
      (edge :from rule-dependence-r :to axiom-con-r :type calls :description "dependenceR calls axiom-con-r (for premises - prototype base case)")
      (edge :from rule-dependence-l :to axiom-incon-l :type calls :description "dependenceL calls axiom-incon-l (for premises - prototype base case)")
      (edge :from rule-independence-r :to axiom-con-r :type calls :description "independenceR calls axiom-con-r (for premises - prototype base case)")
      (edge :from rule-independence-l :to axiom-incon-l :type calls :description "independenceL calls axiom-incon-l (for premises - prototype base case)")


      ;;; Rules manage threads (Independence Rules) (same as before) ;;;
      (edge :from rule-independence-r :to proof-thread-function :type manages-threads :description "independenceR manages proof threads")
      (edge :from rule-independence-r :to refutation-thread-function :type manages-threads :description "independenceR manages refutation threads") ; Technically proof threads, but for conceptual simplicity...
      (edge :from rule-independence-l :to proof-thread-function :type manages-threads :description "independenceL manages proof threads") ; Technically refutation threads, but for conceptual simplicity...
      (edge :from rule-independence-l :to refutation-thread-function :type manages-threads :description "independenceL manages refutation threads")


      ;;; Thread functions call axioms and rules (sequential logic within threads) (same as before) ;;;
      (edge :from proof-thread-function :to axiom-con-r :type calls :description "proof-thread-function calls axiom-con-r")
      (edge :from proof-thread-function :to rule-dependence-r :type calls :description "proof-thread-function calls rule-dependence-r")
      (edge :from proof-thread-function :to rule-independence-r :type calls :description "proof-thread-function calls rule-independence-r")

      (edge :from refutation-thread-function :to axiom-incon-l :type calls :description "refutation-thread-function calls axiom-incon-l")
      (edge :from refutation-thread-function :to rule-dependence-l :type calls :description "refutation-thread-function calls rule-dependence-l")
      (edge :from refutation-thread-function :to rule-independence-l :type calls :description "refutation-thread-function calls rule-independence-l")


      ;;; Orchestration functions (same as before) ;;;
      (edge :from run-prover :to initialize-knowledge-base :type orchestrates :description "run-prover orchestrates knowledge base initialization")
      (edge :from run-prover :to reset-complexity-counters :type orchestrates :description "run-prover orchestrates complexity counter reset")
      (edge :from run-prover :to proof-thread-function :type orchestrates :description "run-prover orchestrates proof thread execution")
      (edge :from run-prover :to refutation-thread-function :type orchestrates :description "run-prover orchestrates refutation thread execution")
      (edge :from main-entry-point :to run-prover :type calls :description "main entry point calls run-prover")
      (edge :from initialize-knowledge-base :to reset-complexity-counters :type calls :description "initialize-knowledge-base calls reset-complexity-counters")


      ;;; Functions use data structures (same as before) ;;;
      (edge :from axiom-con-r :to knowledge-base :type uses-data :description "axiom-con-r uses knowledge-base (though currently ignores it)")
      (edge :from axiom-incon-l :to knowledge-base :type uses-data :description "axiom-incon-l uses knowledge-base (though currently ignores it)")
      (edge :from rule-dependence-r :to knowledge-base :type uses-data :description "rule-dependence-r uses knowledge-base")
      (edge :from rule-dependence-l :to knowledge-base :type uses-data :description "rule-dependence-l uses knowledge-base")
      (edge :from rule-independence-r :to knowledge-base :type uses-data :description "rule-independence-r uses knowledge-base")
      (edge :from rule-independence-l :to knowledge-base :type uses-data :description "rule-independence-l uses knowledge-base")
      (edge :from proof-thread-function :to knowledge-base :type uses-data :description "proof-thread-function uses knowledge-base")
      (edge :from refutation-thread-function :to knowledge-base :type uses-data :description "refutation-thread-function uses knowledge-base")
      (edge :from relnet-node-class :to data-structure :type uses-data :description "relnet-node-class uses data-structure (conceptually)")


      ;;; Axioms are types of rules (conceptual - could be refined) (same as before) ;;;
      (edge :from axiom-con-r :to rule :type implements-rule :description "axiom-con-r implements a rule (axiom)")
      (edge :from axiom-incon-l :to rule :type implements-rule :description "axiom-incon-l implements a rule (axiom)")
      (edge :from rule-dependence-r :to rule :type implements-rule :description "rule-dependence-r implements a rule")
      (edge :from rule-dependence-l :to rule :type implements-rule :description "rule-dependence-l implements a rule")
      (edge :from rule-independence-r :to rule :type implements-rule :description "rule-independence-r implements a rule")
      (edge :from rule-independence-l :to rule :type implements-rule :description "rule-independence-l implements a rule")


      ;;; Metrics are related to functions (functions update metrics) (same as before) ;;;
      (edge :from axiom-applications-count :to metric :type defines-axiom :description "axiom-applications-count is a metric related to axioms")
      (edge :from rule-applications-count :to metric :type defines-rule :description "rule-applications-count is a metric related to rules")


      ;;; Test Runner Functions ;;;
      (edge :from run-tests-func :to test-runner :type tests :description "run-tests-func is a test runner")
      (edge :from run-all-tests-func :to test-runner :type tests :description "run-all-tests-func is a test runner")
      (edge :from run-all-tests-func :to run-tests-func :type calls :description "run-all-tests-func calls run-tests-func")
      (edge :from run-tests-func :to assert-result-func :type calls :description "run-tests-func calls assert-result-func")
      (edge :from run-all-tests-func :to iterate-test-summary-func :type calls :description "run-all-tests-func calls iterate-test-summary-func")
      (edge :from run-all-tests-func :to reset-test-summary-func :type calls :description "run-all-tests-func calls reset-test-summary-func")

      ;;; Test Summary Functions ;;;
      (edge :from reset-test-summary-func :to test-summary-data :type uses-data :description "reset-test-summary-func uses test-suite-summary-data")
      (edge :from update-test-summary-func :to test-summary-data :type updates-summary :description "update-test-summary-func updates test-suite-summary-data")
      (edge :from iterate-test-summary-func :to test-suite-summary-data :type uses-data :description "iterate-test-summary-func uses test-suite-summary-data")
      (edge :from format-test-result-func :to test-summary :type formats-output :description "format-test-result-func formats output for test summary")
      (edge :from update-test-summary-func :to test-summary :type updates-summary :description "update-test-summary-func updates test summary")


      ;;; Assertion Function ;;;
      (edge :from assert-result-func :to assertion-function :type asserts :description "assert-result-func is an assertion function")

      ;;; Test Category Lists include Test Functions ;;;
      (edge :from axiom-tests-list :to test-axiom-con-r-func :type includes-test :description "*axiom-tests* includes test-axiom-con-r-func")
      (edge :from axiom-tests-list :to test-axiom-incon-l-func :type includes-test :description "*axiom-tests* includes test-axiom-incon-l-func")

      (edge :from dependence-rule-tests-list :to test-rule-dependence-r-positive-func :type includes-test :description "*dependence-rule-tests* includes test-rule-dependence-r-positive-func")
      (edge :from dependence-rule-tests-list :to test-rule-dependence-r-negative-func :type includes-test :description "*dependence-rule-tests* includes test-rule-dependence-r-negative-func")
      (edge :from dependence-rule-tests-list :to test-rule-dependence-l-positive-func :type includes-test :description "*dependence-rule-tests* includes test-rule-dependence-l-positive-func")
      (edge :from dependence-rule-tests-list :to test-rule-dependence-l-negative-func :type includes-test :description "*dependence-rule-tests* includes test-rule-dependence-l-negative-func")

      (edge :from independence-rule-tests-list :to test-rule-independence-r-premise1-con-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-r-premise1-con-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-r-premise2-con-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-r-premise2-con-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-r-negative-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-r-negative-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-r-early-exit-premise1-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-r-early-exit-premise1-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-l-premise1-incon-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-l-premise1-incon-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-l-premise2-incon-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-l-premise2-incon-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-l-negative-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-l-negative-func")
      (edge :from independence-rule-tests-list :to test-rule-independence-l-early-exit-premise1-func :type includes-test :description "*independence-rule-tests* includes test-rule-independence-l-early-exit-premise1-func")


      ;;; Main Entry Point orchestrates tests and prover ;;;
      (edge :from main-entry-point :to run-all-tests-func :type orchestrates :description "main entry point orchestrates test execution")
      (edge :from main-entry-point :to run-prover :type orchestrates :description "main entry point orchestrates prover execution")


     )
    )
  )
 "RelNet Abstraction of RelNet Weaver Self-Testing System Prototype."
 )

*relnet-self-testing-abstraction*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes on the RelNet Abstraction - Self-Testing System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Node Types (Extended):
;;; - test-function:    Represents individual test functions (e.g., test-axiom-con-r).
;;; - test-runner:      Functions that run tests (e.g., run-tests, run-all-tests).
;;; - test-summary:     Functions related to test result summarization (e.g., iterate-test-summary).
;;; - test-category-list: Data structures holding lists of test functions (*axiom-tests*, etc.).
;;; - assertion-function: Generic assertion function (assert-result).
;;; - (All previous node types from the prover abstraction are reused)

;;; Edge Types (Extended):
;;; - tests:            Function is a test runner.
;;; - asserts:          Function is an assertion function.
;;; - summarizes:       Function is related to test summarization.
;;; - includes-test:    Test category list includes a specific test function.
;;; - updates-summary:  Function updates the test summary data structure.
;;; - formats-output:   Function formats output (specifically for test results).
;;; - (All previous edge types from the prover abstraction are reused)


;;; Key Extensions for Self-Testing:
;;; - Test Component Nodes and Edges:  New node and edge types are introduced to specifically represent the testing infrastructure (test functions, runners, summaries, assertions).
;;; - Test Orchestration:  The 'main-entry-point' node now has 'orchestrates' edges to both 'run-all-tests-func' and 'run-prover', reflecting its role in running both tests and the prover itself.
;;; - Test Data Flow:  Edges like 'updates-summary' and 'uses-data' link test functions and runners to the test summary data structure.
;;; - Clear Separation: The RelNet clearly separates the theorem prover logic from the testing logic, while also showing how they are orchestrated together in the 'main' function.


;;; Potential Refinements (Self-Testing):
;;; - Test Case Details:  For more complex tests, you could add nodes to represent individual test cases and link test functions to their test cases.
;;; - Test Coverage Metrics: If test coverage analysis is added, new metrics and nodes to represent coverage data could be included.
;;; - Deeper Test Structure:  For larger test suites, a more hierarchical structure for test categories and sub-categories could be reflected in the RelNet.