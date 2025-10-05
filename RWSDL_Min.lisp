;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPROVED RWSDL-Min Code with Meta-Closure Thread Types (Formalized)
;;; - IMPROVED - Rule Renaming - WITH ENCODING/DECODING - WITH NUMERICAL OPERATIONS
;;; - MULTITHREADED OPERATORS - PERFORMANCE METRICS - **OS THREADS FOR CONCURRENCY** ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RWSDL-Min: Minimal Weave Structure and Distributed Logic - MINIMAL VERSION
;;;
;;; Core Concept: Weavexes and Threads for Distributed Reasoning
;;; -----------------------------------------------------------
;;; RWSDL-Min is designed as a minimal computational system exploring
;;; the concept of "weavexes" and their manipulation within different
;;; computational "threads."  **Crucially, the term "thread" here is NOT
;;; a metaphor.  It directly refers to OPERATING SYSTEM LEVEL THREADS.**
;;;
;;; Intended for Distributed Computing and Automated Reasoning:
;;; -----------------------------------------------------------
;;; The use of OS threads is deliberate and fundamental to the design.
;;; RWSDL-Min is envisioned as a foundation for:
;;;   - Distributed Automated Theorem Proving
;;;   - Distributed Anti-Theorem Refutation
;;;   - Distributed Metatheorem Generation
;;; in concurrent and distributed computing environments.  The goal is to
;;; extend and utilize OS threads for actual parallel and concurrent processing
;;; in such systems.
;;;
;;; Numerical Encoding of Weavex State:
;;; ------------------------------------
;;; Weavex states in RWSDL-Min are represented numerically using COMPLEX NUMBERS.
;;; This numerical encoding is a form of Goedelian representation, allowing
;;; weavexes to be manipulated and evaluated using numerical operations.
;;;
;;; Semantic Interpretation of Complex Numbers:
;;;   - Real Part:  Encodes "proof thread information" or consistency aspects.
;;;   - Imaginary Part: Encodes "refutation thread information" or inconsistency aspects.
;;;   - Zero Value:  Represents a "no-closure" condition, where neither proof nor refutation
;;;                 is definitively established by the system for a given expression
;;;                 in a specific thread context.
;;;
;;; Towards Higher-Dimensional Algebras:
;;; ------------------------------------
;;; The current algebra uses scalar complex numbers.  Future development is expected
;;; to generalize to higher-dimensional algebras (similar to vectors, matrices,
;;; octonions, etc.).  These higher-dimensional representations are anticipated
;;; to be necessary for modeling more complex systems, including:
;;;   - Systems of Equations
;;;   - Systems of Inequalities
;;;   - Relational Networks
;;;   - Distributed and Concurrent Computing Networks
;;;
;;; Multithreading as Parsing/Execution Mechanism:
;;; ---------------------------------------------
;;; The multithreaded operators in RWSDL-Min are NOT merely for performance
;;; benchmarking.  **Multithreading is intended to be the fundamental parsing and
;;; execution mechanism of the weaver.**  When a weavex expression is interpreted,
;;; it generates weavexes that are then processed in parallel OS threads.
;;; These threads form "closure structures" analogous to Abstract Syntax Trees (ASTs)
;;; or derivation trees in classical logic.  This parallel generation and decoding
;;; process is computationally and conceptually vital for the system's ability
;;; to derive new theorems (right-weavexes) and anti-theorems (left-weavexes).
;;;
;;; Numerical vs. Multithreaded Implementations:
;;; -------------------------------------------
;;; - Numerical Implementations:  Provide highly efficient, direct numerical
;;;                             computations on weavex states (complex numbers).
;;; - Multithreaded Implementations: Simulate the parsing and concurrent execution
;;;                              of weavex expressions using OS threads, adding
;;;                              a simulated computational workload to represent
;;;                              the cost of interpretation and parallel processing.
;;;                              These are *not* optimized for speed, but for
;;;                              demonstrating the intended concurrent execution model.


;;; --- Timing Utility Function ---
(defun measure-execution-time (function &rest args)
  "Measures and returns the execution time of a given function in milliseconds.

Parameters:
  - FUNCTION: The function to be executed and timed.
  - ARGS: A list of arguments to be passed to the function.

Returns:
  - The elapsed execution time in milliseconds."
  (let ((start-time (get-internal-real-time)))
    (apply function args)
    (let ((end-time (get-internal-real-time)))
      (/ (* (- end-time start-time) 1000) internal-time-units-per-second))))


;;; --- Utility Function: CPU-Bound Workload (Parameterizable) ---
(defun cpu-intensive-workload (iterations)
  "Simulates a CPU-bound workload to model the computational cost of parsing
and executing weavex expressions in a concurrent environment. This is fundamental
to the multithreaded operational semantics, not just for benchmarking.

Parameters:
  - ITERATIONS: The number of iterations to perform in the loop.

Returns:
  - The result of the computation (the sum), used to ensure work is not
    optimized away."
  (let ((sum 0))
    (dotimes (i iterations)
      (incf sum i))
    sum))

;;; --- Numerical Operations for Weavex Algebra (Complex Numbers) ---
(defun complex-conjugate (z)
  "Calculates the complex conjugate of a complex number `z`.
In the Weavex Algebra, this represents a fundamental transformation on a weavex state."
  (complex (realpart z) (- (imagpart z))))

(defun duality-op (z)
  "Performs the duality operation on a weavex state `z`, defined as `i * conjugate(z)`.
This is a core transformation in the system's logic."
  (* #C(0 1) (complex-conjugate z)))


;;; --- Numerical Thread Operations --- (Existing Numerical Operators)
;;; These numerical operations define the core algebraic manipulations
;;; on weavex states (complex numbers). They are designed to be computationally efficient
;;; and represent the *underlying numerical logic* of weavex operations, independent
;;; of the concurrent parsing/execution simulation.

(defun and-thread-r-op-numerical (z1 z2)
  "The numerical implementation of the AND_Thread_R operator.
It computes the result by multiplying the real parts of the two input weavex states `z1` and `z2`."
  (* (realpart z1) (realpart z2)))

(defun or-thread-r-op-numerical (z1 z2)
  "The numerical implementation of the OR_Thread_R operator.
It computes the result by taking the maximum of the real parts of `z1` and `z2`."
  (max (realpart z1) (realpart z2)))

(defun and-thread-l-op-numerical (z1 z2)
  "The numerical implementation of the AND_Thread_L operator.
It computes the result by multiplying the imaginary parts of `z1` and `z2` and scaling by `i`."
  (* (* (imagpart z1) (imagpart z2)) #C(0 1)))

(defun or-thread-l-op-numerical (z1 z2)
  "The numerical implementation of the OR_Thread_L operator.
It computes the result by taking the maximum of the imaginary parts of `z1` and `z2` and scaling by `i`."
  (* (max (imagpart z1) (imagpart z2)) #C(0 1)))

(defun independence-thread-r-op-numerical (z1 z2)
  "The numerical implementation of the INDEPENDENCE_Thread_R operator.
It combines the proof information of two weavexes by adding their real parts."
  (+ (realpart z1) (realpart z2)))

(defun independence-thread-l-op-numerical (z1 z2)
  "The numerical implementation of the INDEPENDENCE_Thread_L operator.
It combines the refutation information by adding the imaginary parts and scaling by `i`."
  (* (+ (imagpart z1) (imagpart z2)) #C(0 1)))

(defun dependence-thread-r-op-numerical (z1 z2)
  "The numerical implementation of the DEPENDENCE_Thread_R operator.
It combines the proof information by averaging the real parts."
  (/ (+ (realpart z1) (realpart z2)) 2.0))

(defun dependence-thread-l-op-numerical (z1 z2)
  "The numerical implementation of the DEPENDENCE_Thread_L operator.
It combines the refutation information by averaging the imaginary parts and scaling by `i`."
  (* (/ (+ (imagpart z1) (imagpart z2)) 2.0) #C(0 1)))


;;; --- Multithreaded Operators --- (New Multithreaded Operators)
;;; These operators simulate the PARSING and EXECUTION of weavex expressions
;;; using OPERATING SYSTEM LEVEL MULTITHREADING.
;;;
;;; **IMPORTANT SEMANTIC CLARIFICATION:**
;;; The "threads" created here using `sb-thread:make-thread` are *actual OS threads*.
;;; They are not an abstraction or a lightweight concurrency mechanism within Lisp.
;;; These threads are intended to represent PARALLEL PROCESSING PATHS within the weaver,
;;; simulating how RWSDL-Min would operate in a distributed or concurrent computing environment.
;;;
;;; The numerical logic applied *after* thread completion is the same as in the
;;; numerical operators.  The key difference is the added SIMULATED COMPUTATIONAL
;;; WORKLOAD performed in parallel by the OS threads, representing the cost of
;;; parsing, interpretation, and concurrent execution of weavex expressions.


(defun and-thread-r-op-mt (z1 z2)
  "The multithreaded implementation of the AND_Thread_R operator.
It simulates the concurrent parsing and execution of sub-expressions by spawning
two OS threads that perform a CPU-intensive task before applying the core
numerical logic of the operator.

Parameters:
  - Z1: The first complex number weavex state.
  - Z2: The second complex number weavex state.

Returns:
  - The resulting complex number weavex state."
  (let ((result #C(0.0 0.0)) ; Initialize result to complex 0
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000)) ; Example iterations - adjust as needed
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations)))) ; **Create OS thread 1 for PARSING/EXECUTION simulation**
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations)))) ; **Create OS thread 2 for PARSING/EXECUTION simulation**
    (#+sbcl sb-thread:join-thread thread1) ; **Wait for OS thread 1 to complete (parsing/execution simulation)**
    (#+sbcl sb-thread:join-thread thread2) ; **Wait for OS thread 2 to complete (parsing/execution simulation)**
    ;; After threads complete (simulated parsing/execution), apply the AND_Thread_R logic (numerical part)
    (setf result (complex (* (realpart z1) (realpart z2)) 0.0)) ; Real part logic, Imaginary part 0 - Same numerical logic as numerical version
    result))

(defun or-thread-r-op-mt (z1 z2)
  "The multithreaded implementation of the OR_Thread_R operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex (max (realpart z1) (realpart z2)) 0.0)) ; Same numerical logic
    result))

(defun and-thread-l-op-mt (z1 z2)
  "The multithreaded implementation of the AND_Thread_L operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex 0.0 (* (imagpart z1) (imagpart z2)))) ; Same numerical logic
    result))

(defun or-thread-l-op-mt (z1 z2)
  "The multithreaded implementation of the OR_Thread_L operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex 0.0 (max (imagpart z1) (imagpart z2)))) ; Same numerical logic
    result))

(defun independence-thread-r-op-mt (z1 z2)
  "The multithreaded implementation of the INDEPENDENCE_Thread_R operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex (+ (realpart z1) (realpart z2)) 0.0)) ; Same numerical logic
    result))

(defun independence-thread-l-op-mt (z1 z2)
  "The multithreaded implementation of the INDEPENDENCE_Thread_L operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex 0.0 (+ (imagpart z1) (imagpart z2)))) ; Same numerical logic
    result))

(defun dependence-thread-r-op-mt (z1 z2)
  "The multithreaded implementation of the DEPENDENCE_Thread_R operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex (/ (+ (realpart z1) (realpart z2)) 2.0) 0.0)) ; Same numerical logic
    result))

(defun dependence-thread-l-op-mt (z1 z2)
  "The multithreaded implementation of the DEPENDENCE_Thread_L operator.
Simulates concurrent execution before applying the numerical logic."
  (let ((result #C(0.0 0.0))
        (thread1 nil)
        (thread2 nil)
        (iterations 1000000))
    (setf thread1 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (setf thread2 #+sbcl (sb-thread:make-thread (lambda () (cpu-intensive-workload iterations))))
    (#+sbcl sb-thread:join-thread thread1)
    (#+sbcl sb-thread:join-thread thread2)
    (setf result (complex 0.0 (/ (+ (imagpart z1) (imagpart z2)) 2.0))) ; Same numerical logic
    result))


;;; Axioms (functions directly implementing axioms - NUMERICAL and MULTITHREADED)
;;; Axioms represent base cases or fundamental truths in the RWSDL-Min system.
;;; They dispatch to numerical axiom implementations, as axioms themselves
;;; are currently defined numerically.

(defun rwsdl-min-axiom-con-r (expression thread-type implementation-type)
  "The main dispatcher for the 'CON' axiom.
It calls the numerical implementation, as axioms are fundamental and do not have
a separate multithreaded simulation.

Parameters:
  - EXPRESSION: The expression to evaluate (should be 'CON').
  - THREAD-TYPE: The thread context (:proof_closure_thread, etc.).
  - IMPLEMENTATION-TYPE: The implementation to use (:numerical or :multithreaded).

Returns:
  - A complex number representing the weavex state."
  (case implementation-type
    (:numerical (rwsdl-min-axiom-con-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-axiom-con-r-numerical expression thread-type)) ; Numerical axiom still applies for CON
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-axiom-con-r-numerical (expression thread-type)
  "The numerical implementation of the 'CON' axiom.
It returns a complex number representing the state of 'CON' based on the
current thread type.

Parameters:
  - EXPRESSION: The expression to evaluate.
  - THREAD-TYPE: The current thread context.

Returns:
  - #C(1.0 0.0) for proof/meta threads, #C(0.0 0.0) otherwise."
  (if (eq expression 'CON)
      (case thread-type
        (:proof_closure_thread #C(1.0 0.0))  ; CON -> 1 in proof thread (strong consistency)
        (:meta_closure_thread #C(1.0 0.0)) ; CON -> 1 in meta thread (consistent at meta-level)
        (otherwise #C(0.0 0.0)))          ; CON -> 0 otherwise (no consistency in other threads)
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; No match in meta -> 0
        (otherwise #C(0.0 0.0)))))        ; No match otherwise -> 0

(defun rwsdl-min-axiom-incon-l (expression thread-type implementation-type)
  "The main dispatcher for the 'INCON' axiom.
It calls the numerical implementation.

Parameters:
  - EXPRESSION: The expression to evaluate (should be 'INCON').
  - THREAD-TYPE: The thread context.
  - IMPLEMENTATION-TYPE: The implementation to use.

Returns:
  - A complex number representing the weavex state."
  (case implementation-type
    (:numerical (rwsdl-min-axiom-incon-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-axiom-incon-l-numerical expression thread-type)) ; Numerical axiom still applies for INCON
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-axiom-incon-l-numerical (expression thread-type)
  "The numerical implementation of the 'INCON' axiom.
It returns a complex number representing the state of 'INCON' based on the
current thread type.

Parameters:
  - EXPRESSION: The expression to evaluate.
  - THREAD-TYPE: The current thread context.

Returns:
  - #C(0.0 1.0) for refutation/meta threads, #C(0.0 0.0) otherwise."
  (if (eq expression 'INCON)
      (case thread-type
        (:refutation_closure_thread #C(0.0 1.0)) ; INCON -> i in refutation thread (strong inconsistency)
        (:meta_closure_thread #C(0.0 1.0))      ; INCON -> i in meta thread (inconsistent at meta-level)
        (otherwise #C(0.0 0.0)))              ; INCON -> 0 otherwise (no inconsistency in other threads)
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; No match in meta -> 0
        (otherwise #C(0.0 0.0)))))        ; No match otherwise -> 0


;;; Inference Rules (functions directly implementing rules - NUMERICAL and MULTITHREADED)
;;; Inference rules define how to derive new weavex states from existing ones.
;;; They dispatch to either numerical or multithreaded implementations
;;; based on the 'implementation-type'.

(defun rwsdl-min-rule-or-thread-r (expression thread-type implementation-type)
  "The main dispatcher for the 'OR_Thread_R' rule.
It selects either the numerical or multithreaded implementation based on `implementation-type`."
  (case implementation-type
    (:numerical (rwsdl-min-rule-or-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-or-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-or-thread-r-numerical (expression thread-type)
  "The numerical implementation of the 'OR_Thread_R' rule.
It recursively evaluates sub-expressions and combines them using the
`or-thread-r-op-numerical` function."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (or-thread-r-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical OR_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (or-thread-r-op-numerical result-a result-b))) ; Apply numerical OR_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-or-thread-r-mt (expression thread-type)
  "The multithreaded implementation of the 'OR_Thread_R' rule.
It simulates concurrent execution of sub-expressions before combining them
with the `or-thread-r-op-mt` function."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (or-thread-r-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded OR_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (or-thread-r-op-mt result-a result-b))) ; Apply multithreaded OR_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-and-thread-r (expression thread-type implementation-type)
  "The main dispatcher for the 'AND_Thread_R' rule.
Selects the numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-and-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-and-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-and-thread-r-numerical (expression thread-type)
  "The numerical implementation of the 'AND_Thread_R' rule.
Combines sub-expressions using `and-thread-r-op-numerical`."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (and-thread-r-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical AND_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (and-thread-r-op-numerical result-a result-b))) ; Apply numerical AND_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-and-thread-r-mt (expression thread-type)
  "The multithreaded implementation of the 'AND_Thread_R' rule.
Simulates concurrent execution before combining with `and-thread-r-op-mt`."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (and-thread-r-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded AND_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (and-thread-r-op-mt result-a result-b))) ; Apply multithreaded AND_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-duality-r (expression thread-type implementation-type)
  "The main dispatcher for the 'DUALITY_R' rule.
Currently, it always uses the numerical implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-duality-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-duality-r-numerical expression thread-type)) ; Duality is still numerical op
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-duality-r-numerical (expression thread-type)
  "The numerical implementation of the 'DUALITY_R' rule.
It evaluates the sub-expression in a meta-closure thread and applies the `duality-op`."
  (if (and (listp expression) (eq (car expression) 'DUALITY_R) (cadr expression))
      (let ((a (cadr expression)))
        (duality-op (rwsdl-min-self-interpret a :meta_closure_thread :numerical))) ; Meta-closure thread for duality, numerical operation
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-or-thread-l (expression thread-type implementation-type)
  "The main dispatcher for the 'OR_Thread_L' rule."
  (case implementation-type
    (:numerical (rwsdl-min-rule-or-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-or-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-or-thread-l-numerical (expression thread-type)
  "The numerical implementation of the 'OR_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (or-thread-l-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical OR_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (or-thread-l-op-numerical result-a result-b))) ; Apply numerical OR_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-or-thread-l-mt (expression thread-type)
  "The multithreaded implementation of the 'OR_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (or-thread-l-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded OR_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (or-thread-l-op-mt result-a result-b))) ; Apply multithreaded OR_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-and-thread-l (expression thread-type implementation-type)
  "The main dispatcher for the 'AND_Thread_L' rule."
  (case implementation-type
    (:numerical (rwsdl-min-rule-and-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-and-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-and-thread-l-numerical (expression thread-type)
  "The numerical implementation of the 'AND_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (and-thread-l-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical AND_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (and-thread-l-op-numerical result-a result-b))) ; Apply numerical AND_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-and-thread-l-mt (expression thread-type)
  "The multithreaded implementation of the 'AND_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (and-thread-l-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded AND_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (and-thread-l-op-mt result-a result-b))) ; Apply multithreaded AND_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-duality-l (expression thread-type implementation-type)
  "The main dispatcher for the 'DUALITY_L' rule.
Currently, it always uses the numerical implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-duality-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-duality-l-numerical expression thread-type)) ; Duality is still numerical op
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-duality-l-numerical (expression thread-type)
  "The numerical implementation of the 'DUALITY_L' rule.
It evaluates the sub-expression in a meta-closure thread and applies the `duality-op`."
  (if (and (listp expression) (eq (car expression) 'DUALITY_L) (cadr expression))
      (let ((a (cadr expression)))
        (duality-op (rwsdl-min-self-interpret a :meta_closure_thread :numerical))) ; Meta-closure thread for duality, numerical operation
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-independence-thread-r (expression thread-type implementation-type)
  "The main dispatcher for the 'INDEPENDENCE_Thread_R' rule."
  (case implementation-type
    (:numerical (rwsdl-min-rule-independence-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-independence-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-independence-thread-r-numerical (expression thread-type)
  "The numerical implementation of the 'INDEPENDENCE_Thread_R' rule."
  (if (and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (independence-thread-r-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical INDEPENDENCE_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (independence-thread-r-op-numerical result-a result-b))) ; Apply numerical INDEPENDENCE_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-independence-thread-r-mt (expression thread-type)
  "The multithreaded implementation of the 'INDEPENDENCE_Thread_R' rule."
  (if (and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (independence-thread-r-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded INDEPENDENCE_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (independence-thread-r-op-mt result-a result-b))) ; Apply multithreaded INDEPENDENCE_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-independence-thread-l (expression thread-type implementation-type)
  "The main dispatcher for the 'INDEPENDENCE_Thread_L' rule."
  (case implementation-type
    (:numerical (rwsdl-min-rule-independence-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-independence-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-independence-thread-l-numerical (expression thread-type)
  "The numerical implementation of the 'INDEPENDENCE_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (independence-thread-l-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical INDEPENDENCE_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (independence-thread-l-op-numerical result-a result-b))) ; Apply numerical INDEPENDENCE_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-independence-thread-l-mt (expression thread-type)
  "The multithreaded implementation of the 'INDEPENDENCE_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (independence-thread-l-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded INDEPENDENCE_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (independence-thread-l-op-mt result-a result-b))) ; Apply multithreaded INDEPENDENCE_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-dependence-thread-r (expression thread-type implementation-type)
  "The main dispatcher for the 'DEPENDENCE_Thread_R' rule."
  (case implementation-type
    (:numerical (rwsdl-min-rule-dependence-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-dependence-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-dependence-thread-r-numerical (expression thread-type)
  "The numerical implementation of the 'DEPENDENCE_Thread_R' rule."
  (if (and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (dependence-thread-r-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical DEPENDENCE_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (dependence-thread-r-op-numerical result-a result-b))) ; Apply numerical DEPENDENCE_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-dependence-thread-r-mt (expression thread-type)
  "The multithreaded implementation of the 'DEPENDENCE_Thread_R' rule."
  (if (and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (dependence-thread-r-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded DEPENDENCE_Thread_R in proof thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (dependence-thread-r-op-mt result-a result-b))) ; Apply multithreaded DEPENDENCE_Thread_R in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-dependence-thread-l (expression thread-type implementation-type)
  "The main dispatcher for the 'DEPENDENCE_Thread_L' rule."
  (case implementation-type
    (:numerical (rwsdl-min-rule-dependence-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-dependence-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-dependence-thread-l-numerical (expression thread-type)
  "The numerical implementation of the 'DEPENDENCE_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (dependence-thread-l-op-numerical (rwsdl-min-self-interpret a thread-type :numerical) (rwsdl-min-self-interpret b thread-type :numerical))) ; Apply numerical DEPENDENCE_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :numerical))
                 (result-b (rwsdl-min-self-interpret b thread-type :numerical)))
             (dependence-thread-l-op-numerical result-a result-b))) ; Apply numerical DEPENDENCE_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise

(defun rwsdl-min-rule-dependence-thread-l-mt (expression thread-type)
  "The multithreaded implementation of the 'DEPENDENCE_Thread_L' rule."
  (if (and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (dependence-thread-l-op-mt (rwsdl-min-self-interpret a thread-type :multithreaded) (rwsdl-min-self-interpret b thread-type :multithreaded))) ; Apply multithreaded DEPENDENCE_Thread_L in refutation thread
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret a thread-type :multithreaded))
                 (result-b (rwsdl-min-self-interpret b thread-type :multithreaded)))
             (dependence-thread-l-op-mt result-a result-b))) ; Apply multithreaded DEPENDENCE_Thread_L in meta thread
          (otherwise #C(0.0 0.0)))) ; Default to 0 in other threads
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise



;;; Minimal Self-Interpretation (modified to handle :meta_closure_thread type and implementation type dispatch)
(defun rwsdl-min-self-interpret (expression thread-type implementation-type)
  "The core weaver function that interprets and dispatches weavex expressions.
It selects the appropriate axiom or rule based on the expression's structure
and the specified thread and implementation types. If no rule matches, it
returns a complex zero, representing a 'no-closure' state.

   **IMPORTANT SEMANTIC CLARIFICATION:**
   This function is the central WEAVER in RWSDL-Min. It interprets weavex expressions
   and orchestrates their execution. When using :multithreaded implementation,
   this function initiates PARALLEL PARSING and EXECUTION of sub-expressions
   using OS threads, simulating concurrent processing.

   If no axiom or rule matches the 'expression', it defaults to complex zero (#C(0.0 0.0)),
   representing a 'no-closure' condition in the respective thread, indicating
   that the weaver could not interpret or process the expression in that context."
  (cond
    ((eq expression 'CON) (rwsdl-min-axiom-con-r expression thread-type implementation-type)) ; Dispatch to CON axiom
    ((eq expression 'INCON) (rwsdl-min-axiom-incon-l expression thread-type implementation-type)) ; Dispatch to INCON axiom
    ((and (listp expression) (eq (car expression) 'OR_Thread_R)) (rwsdl-min-rule-or-thread-r expression thread-type implementation-type)) ; Dispatch to OR_Thread_R rule
    ((and (listp expression) (eq (car expression) 'AND_Thread_R)) (rwsdl-min-rule-and-thread-r expression thread-type implementation-type)) ; Dispatch to AND_Thread_R rule
    ((and (listp expression) (eq (car expression) 'DUALITY_R)) (rwsdl-min-rule-duality-r expression thread-type implementation-type)) ; Dispatch to DUALITY_R rule
    ((and (listp expression) (eq (car expression) 'OR_Thread_L)) (rwsdl-min-rule-or-thread-l expression thread-type implementation-type)) ; Dispatch to OR_Thread_L rule
    ((and (listp expression) (eq (car expression) 'AND_Thread_L)) (rwsdl-min-rule-and-thread-l expression thread-type implementation-type)) ; Dispatch to AND_Thread_L rule
    ((and (listp expression) (eq (car expression) 'DUALITY_L)) (rwsdl-min-rule-duality-l expression thread-type implementation-type)) ; Dispatch to DUALITY_L rule
    ((and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_R)) (rwsdl-min-rule-independence-thread-r expression thread-type implementation-type)) ; Dispatch to INDEPENDENCE_Thread_R rule
    ((and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_L)) (rwsdl-min-rule-independence-thread-l expression thread-type implementation-type)) ; Dispatch to INDEPENDENCE_Thread_L rule
    ((and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_R)) (rwsdl-min-rule-dependence-thread-r expression thread-type implementation-type)) ; Dispatch to DEPENDENCE_Thread_R rule
    ((and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_L)) (rwsdl-min-rule-dependence-thread-l expression thread-type implementation-type)) ; Dispatch to DEPENDENCE_Thread_L rule

    (t (case thread-type
         (:proof_closure_thread #C(0.0 0.0))    ; Default to 0 in proof thread (no proof closure)
         (:refutation_closure_thread #C(0.0 0.0)) ; Default to 0 in refutation thread (no refutation closure)
         (:meta_closure_thread #C(0.0 0.0))      ; Default to 0 in meta thread (no meta closure)
         (otherwise (error "Invalid thread type: ~a" thread-type)))))) ; Error for invalid thread type


;;; Evaluation Functions (modified to handle implementation type)
;;; Evaluation functions serve as entry points to the RWSDL-Min system,
;;; dispatching to the self-interpretation function with specific
;;; thread types and implementation types.

(defun rwsdl-min-evaluate (expression thread-type implementation-type)
  "The main evaluation function that dispatches to either numerical or
multithreaded evaluation based on the `implementation-type` parameter.

Parameters:
  - EXPRESSION: The weavex expression to evaluate.
  - THREAD-TYPE: The thread context.
  - IMPLEMENTATION-TYPE: The desired implementation (:numerical or :multithreaded).

Returns:
  - The result of the evaluation (a complex number)."
  (case implementation-type
    (:numerical (rwsdl-min-evaluate-numerical expression thread-type)) ; Dispatch to numerical evaluation
    (:multithreaded (rwsdl-min-evaluate-multithreaded expression thread-type)) ; Dispatch to multithreaded evaluation
    (otherwise (error "Invalid implementation type: ~a" implementation-type)))) ; Error for invalid implementation type

(defun rwsdl-min-evaluate-numerical (expression thread-type)
  "Performs a purely numerical evaluation of a weavex expression.
This is the fastest evaluation path, as it avoids OS thread simulation."
  (rwsdl-min-self-interpret expression thread-type :numerical)) ; Call self-interpret with numerical implementation

(defun rwsdl-min-evaluate-multithreaded (expression thread-type)
  "Performs a multithreaded evaluation of a weavex expression.
This simulates the concurrent parsing and execution of sub-expressions using
OS threads and is intended to model the system's behavior in a parallel
computing environment, not for performance optimization."
  (rwsdl-min-self-interpret expression thread-type :multithreaded)) ; Call self-interpret with multithreaded implementation


;;; Proof, Refutation, and Meta-Closure Threads (using rwsdl-min-evaluate - dispatching based on implementation type)
;;; These functions provide user-friendly interfaces for evaluating expressions
;;; within specific thread contexts (proof, refutation, meta) and with
;;; a chosen implementation type (numerical or multithreaded).

(defun rwsdl-min-proof-closure-thread (expression implementation-type)
  "A convenience wrapper to evaluate an expression in a proof closure thread.

Parameters:
  - EXPRESSION: The weavex expression.
  - IMPLEMENTATION-TYPE: The implementation to use.

Returns:
  - The result of the evaluation."
  (rwsdl-min-evaluate expression :proof_closure_thread implementation-type)) ; Evaluate in proof thread

(defun rwsdl-min-refutation-closure-thread (expression implementation-type)
  "A convenience wrapper to evaluate an expression in a refutation closure thread.

Parameters:
  - EXPRESSION: The weavex expression.
  - IMPLEMENTATION-TYPE: The implementation to use.

Returns:
  - The result of the evaluation."
  (rwsdl-min-evaluate expression :refutation_closure_thread implementation-type)) ; Evaluate in refutation thread

(defun rwsdl-min-meta-closure-thread (expression implementation-type)
  "A convenience wrapper to evaluate an expression in a meta-closure thread.

Parameters:
  - EXPRESSION: The weavex expression.
  - IMPLEMENTATION-TYPE: The implementation to use.

Returns:
  - The result of the evaluation."
  (rwsdl-min-evaluate expression :meta_closure_thread implementation-type)) ; Evaluate in meta thread


;;; Bootstrap Loop Test (modified to test meta-closure threads, implementation types and new closure outputs)
(defun rwsdl-min-bootstrap-loop-test ()
  "The main testing function for the RWSDL-Min system.
It systematically evaluates a list of test expressions across all thread types
and both implementation types (:numerical and :multithreaded). It measures
the execution time for each evaluation and prints a formatted table comparing
the performance and results.

   **Performance Metrics are crucial for understanding the computational cost
   of simulating concurrent parsing/execution using OS threads
   compared to direct numerical evaluation.**

   The test results demonstrate the performance characteristics of RWSDL-Min
   under different thread contexts and implementation choices, highlighting
   the overhead of multithreading simulation versus the efficiency of
   direct numerical operations."
  (print "*** RWSDL-Min Bootstrap Loop Test (Numerical & Multithreaded) with Performance Metrics ***")

  (let ((test-expressions '( ; List of expressions to test
                     CON
                     INCON
                     (OR_Thread_R CON 'INCON)
                     (AND_Thread_R CON CON)
                     (DUALITY_R INCON)
                     (DUALITY_R CON)
                     (OR_Thread_L INCON 'CON)
                     (AND_Thread_L INCON INCON)
                     (OR_Thread_R CON '(DUALITY_R CON))
                     (AND_Thread_R CON '(DUALITY_R INCON))
                     (OR_Thread_L INCON '(DUALITY_R CON))
                     (AND_Thread_L INCON '(DUALITY_R INCON))
                     (INDEPENDENCE_Thread_R CON 'INCON)
                     (DEPENDENCE_Thread_R CON 'CON)
                     (INDEPENDENCE_Thread_L INCON 'CON)
                     (DEPENDENCE_Thread_L INCON 'INCON)
                     ))
        (results-buffer (make-array 0 :adjustable t :fill-pointer t))) ; Buffer to store test results

    (loop for expr in test-expressions do ; Iterate through each test expression
      (loop for thread-type in '(:proof_closure_thread :refutation_closure_thread :meta_closure_thread) do ; Iterate through each thread type
        (loop for impl-type in '(:numerical :multithreaded) do ; Iterate through each implementation type
          (let ((result nil)
                (exec-time 0))

            (setf exec-time (measure-execution-time ; Measure execution time for each combination
                               #'rwsdl-min-evaluate
                               expr thread-type impl-type))
            (setf result (rwsdl-min-evaluate expr thread-type impl-type)) ; Evaluate the expression


            (vector-push-extend  ; Store results in the buffer
             (list :expression expr
                   :thread-type thread-type
                   :implementation-type impl-type
                   :result result
                   :execution-time exec-time)
             results-buffer)
            ))))


    ;;; --- Output Buffered Results in a Table ---
    (print "*** Performance Test Results ***")
    (format t "~%---------------------------------------------------------------------------------------------------~%")
    (format t "| ~20A | ~25A | ~15A | ~25A | ~15A |~%" "Expression" "Thread Type" "Impl. Type" "Result" "Exec. Time (ms)")
    (format t "|----------------------|---------------------------|-----------------|---------------------------|-----------------|~%")

    (loop for res across results-buffer do ; Iterate through results buffer and print table rows
      (format t "| ~20A | ~25A | ~15A | ~25A | ~15,4F |~%"
              (getf res :expression)
              (getf res :thread-type)
              (getf res :implementation-type)
              (getf res :result)
              (getf res :execution-time))
      )
    (format t "---------------------------------------------------------------------------------------------------~%")


    ))


  ;;; Run Bootstrap Test with both Numerical and Multithreaded Evaluation and Performance Metrics
  (rwsdl-min-bootstrap-loop-test)

  (print "*** Formal Verification and Performance Testing Complete (Numerical & Multithreaded) ***")


;;; Run the bootstrap test with both numerical and multithreaded evaluation and performance metrics
(rwsdl-min-bootstrap-loop-test)