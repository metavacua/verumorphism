;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RelNet Abstraction of RelNet Weaver Theorem Prover (LISP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *relnet-weaver-abstraction*
  '(
    ;;; Node Types ;;;
    (:node-types (function data-structure metric axiom rule thread orchestration))

    ;;; Edge Types ;;;
    (:edge-types (uses-data calls implements-rule manages-threads updates-metric defines-axiom defines-rule orchestrates))

    ;;; Nodes ;;;
    (:nodes
     (
      ;; Data Structures
      (node :name knowledge-base :type data-structure :description "Global Knowledge Base")
      (node :name relnet-node :type data-structure :description "Data structure for RelNet nodes")

      ;; Complexity Metrics
      (node :name axiom-applications-count :type metric :description "Counter for axiom applications")
      (node :name rule-applications-count :type metric :description "Counter for rule applications")

      ;; Axioms
      (node :name axiom-con-r :type axiom :description "Proof Axiom (con_R)")
      (node :name axiom-incon-l :type axiom :description "Refutation Axiom (incon_L)")

      ;; Rules - Dependence
      (node :name rule-dependence-r :type rule :description "Dependence Right Rule (dependenceR)")
      (node :name rule-dependence-l :type rule :description "Dependence Left Rule (dependenceL)")

      ;; Rules - Independence
      (node :name rule-independence-r :type rule :description "Independence Right Rule (independenceR) - THREADED")
      (node :name rule-independence-l :type rule :description "Independence Left Rule (independenceL) - THREADED")

      ;; Thread Functions
      (node :name proof-thread-function :type thread :description "Proof Thread Function")
      (node :name refutation-thread-function :type thread :description "Refutation Thread Function")

      ;; Orchestration Functions
      (node :name run-prover :type orchestration :description "Main function to run the prover")
      (node :name initialize-knowledge-base :type orchestration :description "Initializes knowledge base and resets metrics")
      (node :name reset-complexity-counters :type orchestration :description "Resets complexity counters")
      (node :name main-entry-point :type orchestration :description "Main entry point to start the system (main function)")
     )
    )

    ;;; Edges (Relationships) ;;;
    (:edges
     (
      ;; Axioms update metrics
      (edge :from axiom-con-r :to axiom-applications-count :type updates-metric :description "con_R increments axiom counter")
      (edge :from axiom-incon-l :to axiom-applications-count :type updates-metric :description "incon_L increments axiom counter")

      ;; Rules update metrics
      (edge :from rule-dependence-r :to rule-applications-count :type updates-metric :description "dependenceR increments rule counter")
      (edge :from rule-dependence-l :to rule-applications-count :type updates-metric :description "dependenceL increments rule counter")
      (edge :from rule-independence-r :to rule-applications-count :type updates-metric :description "independenceR increments rule counter")
      (edge :from rule-independence-l :to rule-applications-count :type updates-metric :description "independenceL increments rule counter")


      ;; Rules call axioms (in base case for prototype)
      (edge :from rule-dependence-r :to axiom-con-r :type calls :description "dependenceR calls axiom-con-r (for premises - prototype base case)")
      (edge :from rule-dependence-l :to axiom-incon-l :type calls :description "dependenceL calls axiom-incon-l (for premises - prototype base case)")
      (edge :from rule-independence-r :to axiom-con-r :type calls :description "independenceR calls axiom-con-r (for premises - prototype base case)")
      (edge :from rule-independence-l :to axiom-incon-l :type calls :description "independenceL calls axiom-incon-l (for premises - prototype base case)")


      ;; Rules manage threads (Independence Rules)
      (edge :from rule-independence-r :to proof-thread-function :type manages-threads :description "independenceR manages proof threads")
      (edge :from rule-independence-r :to refutation-thread-function :type manages-threads :description "independenceR manages refutation threads") ; Technically proof threads, but for conceptual simplicity...
      (edge :from rule-independence-l :to proof-thread-function :type manages-threads :description "independenceL manages proof threads") ; Technically refutation threads, but for conceptual simplicity...
      (edge :from rule-independence-l :to refutation-thread-function :type manages-threads :description "independenceL manages refutation threads")


      ;; Thread functions call axioms and rules (sequential logic within threads)
      (edge :from proof-thread-function :to axiom-con-r :type calls :description "proof-thread-function calls axiom-con-r")
      (edge :from proof-thread-function :to rule-dependence-r :type calls :description "proof-thread-function calls rule-dependence-r")
      (edge :from proof-thread-function :to rule-independence-r :type calls :description "proof-thread-function calls rule-independence-r")

      (edge :from refutation-thread-function :to axiom-incon-l :type calls :description "refutation-thread-function calls axiom-incon-l")
      (edge :from refutation-thread-function :to rule-dependence-l :type calls :description "refutation-thread-function calls rule-dependence-l")
      (edge :from refutation-thread-function :to rule-independence-l :type calls :description "refutation-thread-function calls rule-independence-l")


      ;; Orchestration functions
      (edge :from run-prover :to initialize-knowledge-base :type orchestrates :description "run-prover orchestrates knowledge base initialization")
      (edge :from run-prover :to reset-complexity-counters :type orchestrates :description "run-prover orchestrates complexity counter reset")
      (edge :from run-prover :to proof-thread-function :type orchestrates :description "run-prover orchestrates proof thread execution")
      (edge :from run-prover :to refutation-thread-function :type orchestrates :description "run-prover orchestrates refutation thread execution")
      (edge :from main-entry-point :to run-prover :type calls :description "main entry point calls run-prover")
      (edge :from initialize-knowledge-base :to reset-complexity-counters :type calls :description "initialize-knowledge-base calls reset-complexity-counters")


      ;; Functions use data structures
      (edge :from axiom-con-r :to knowledge-base :type uses-data :description "axiom-con-r uses knowledge-base (though currently ignores it)")
      (edge :from axiom-incon-l :to knowledge-base :type uses-data :description "axiom-incon-l uses knowledge-base (though currently ignores it)")
      (edge :from rule-dependence-r :to knowledge-base :type uses-data :description "rule-dependence-r uses knowledge-base")
      (edge :from rule-dependence-l :to knowledge-base :type uses-data :description "rule-dependence-l uses knowledge-base")
      (edge :from rule-independence-r :to knowledge-base :type uses-data :description "rule-independence-r uses knowledge-base")
      (edge :from rule-independence-l :to knowledge-base :type uses-data :description "rule-independence-l uses knowledge-base")
      (edge :from proof-thread-function :to knowledge-base :type uses-data :description "proof-thread-function uses knowledge-base")
      (edge :from refutation-thread-function :to knowledge-base :type uses-data :description "refutation-thread-function uses knowledge-base")


      ;; Axioms are types of rules (conceptual - could be refined)
      (edge :from axiom-con-r :to rule :type implements-rule :description "axiom-con-r implements a rule (axiom)")
      (edge :from axiom-incon-l :to rule :type implements-rule :description "axiom-incon-l implements a rule (axiom)")
      (edge :from rule-dependence-r :to rule :type implements-rule :description "rule-dependence-r implements a rule")
      (edge :from rule-dependence-l :to rule :type implements-rule :description "rule-dependence-l implements a rule")
      (edge :from rule-independence-r :to rule :type implements-rule :description "rule-independence-r implements a rule")
      (edge :from rule-independence-l :to rule :type implements-rule :description "rule-independence-l implements a rule")


      ;; Metrics are related to functions (functions update metrics)
      (edge :from axiom-applications-count :to metric :type defines-axiom :description "axiom-applications-count is a metric related to axioms")
      (edge :from rule-applications-count :to metric :type defines-rule :description "rule-applications-count is a metric related to rules")


     )
    )
  )
 "A data structure representing a Relational Network (RelNet) abstraction of the RelNet Weaver theorem prover.

This variable holds a structured representation of the prover's architecture, defining its core components as nodes and their interactions as edges.

- **Node Types:** Include `function`, `data-structure`, `metric`, `axiom`, `rule`, `thread`, and `orchestration`.
- **Edge Types:** Represent relationships like `uses-data`, `calls`, `implements-rule`, `manages-threads`, and `updates-metric`.
- **Nodes:** Define specific components of the prover, such as `knowledge-base`, `axiom-con-r`, `proof-thread-function`, etc.
- **Edges:** Describe the relationships between these nodes, for example, how a rule calls an axiom or updates a complexity metric.

This abstraction is intended for analysis, visualization, and meta-level reasoning about the theorem prover's structure and logic."
 )

*relnet-weaver-abstraction*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes on the RelNet Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Node Types:
;;; - function:  Represents LISP functions (axioms, rules, threads, orchestration).
;;; - data-structure: Represents data structures (knowledge base, node class).
;;; - metric:      Represents complexity metrics (counters).
;;; - axiom:       Specialized type for axiom functions.
;;; - rule:        General type for rule functions (including axioms conceptually).
;;; - thread:      Type for thread functions.
;;; - orchestration: Type for functions that orchestrate the prover process.

;;; Edge Types:
;;; - uses-data:     Function uses a specific data structure.
;;; - calls:         Function calls another function.
;;; - implements-rule: Function implements a logical rule or axiom (conceptual).
;;; - manages-threads: Function is responsible for creating and managing threads (specifically for independence rules).
;;; - updates-metric: Function increments or updates a complexity metric counter.
;;; - defines-axiom: Metric is related to axioms.
;;; - defines-rule:  Metric is related to rules.
;;; - orchestrates:  Orchestration function manages or calls other functions to control the prover flow.


;;; Key Abstractions Made:
;;; - Focus on Functionality:  Abstraction focuses on the functional components (functions, data) and their relationships, rather than detailed code implementation.
;;; - Simplified Rule Representation:  Rules and axioms are both represented as 'rule' type nodes with 'implements-rule' edges, simplifying the type hierarchy.  More specific types (axiom, dependence rule, independence rule) could be added if needed for finer-grained analysis.
;;; - Thread Management:  'manages-threads' edges highlight the thread-related logic of independence rules, a key complexity aspect.
;;; - Complexity Metric Links: 'updates-metric' and 'defines-metric' edges explicitly link functions to the complexity metrics they affect, making the complexity analysis aspect more visible in the RelNet.
;;; - Omission of Prototyping Details:  Print statements, unit tests, and test summaries are intentionally excluded from this abstraction as they are not core to the theorem prover's logic itself.


;;; Potential Refinements:
;;; - More Granular Rule Types:  Instead of just 'rule', have 'dependence-rule', 'independence-rule', 'axiom' node types for more precise classification.
;;; - Data Flow Edges:  Add edges to represent data flow more explicitly (e.g., function 'returns-data' of type X, function 'takes-data' of type X).
;;; - Deeper Abstraction of Thread Logic:  For more complex threading patterns, the 'manages-threads' edge might need to be more detailed.
;;; - Integration with Complexity Classification:  The RelNet could be extended to *include* complexity class information directly as node properties or relationships, once the classification system is implemented.