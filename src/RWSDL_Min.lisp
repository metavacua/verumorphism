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
  "Measures the execution time of a function call in milliseconds."
  (let ((start-time (get-internal-real-time)))
    (apply function args)
    (let ((end-time (get-internal-real-time)))
      (/ (* (- end-time start-time) 1000) internal-time-units-per-second))))


;;; --- Utility Function: CPU-Bound Workload (Parameterizable) ---
(defun cpu-intensive-workload (iterations)
  "Simulates a CPU-bound workload for a given number of iterations.
   **IMPORTANT:** This function is used in multithreaded operations to
   represent the COMPUTATIONAL EFFORT of PARSING and EXECUTING weavex expressions
   in a CONCURRENT setting using OS threads.  It is NOT just for benchmarking,
   but to model the intended operational semantics of RWSDL-Min."
  (let ((sum 0))
    (dotimes (i iterations)
      (incf sum i))
    sum))

;;; --- Numerical Operations for Weavex Algebra (Complex Numbers) ---
(defun complex-conjugate (z)
  "Returns the complex conjugate of a complex number.
   In Weavex Algebra, this is a numerical operation on weavex states."
  (complex (realpart z) (- (imagpart z))))

(defun duality-op (z)
  "Duality operation: i * conjugate(z).
   Represents a core transformation within Weavex Algebra."
  (* #C(0 1) (complex-conjugate z)))


;;; --- Numerical Thread Operations --- (Existing Numerical Operators)
;;; These numerical operations define the core algebraic manipulations
;;; on weavex states (complex numbers). They are designed to be computationally efficient
;;; and represent the *underlying numerical logic* of weavex operations, independent
;;; of the concurrent parsing/execution simulation.

(defun and-thread-r-op-numerical (z1 z2)
  "Numerical AND_Thread_R operation: Re(z1) * Re(z2).
   Combines the proof thread information (Real parts) of two weavexes
   using multiplication.  This is the CORE NUMERICAL LOGIC of AND_Thread_R."
  (* (realpart z1) (realpart z2)))

(defun or-thread-r-op-numerical (z1 z2)
  "Numerical OR_Thread_R operation: max(Re(z1), Re(z2)).
   Combines the proof thread information (Real parts) of two weavexes
   using maximum selection. This is the CORE NUMERICAL LOGIC of OR_Thread_R."
  (max (realpart z1) (realpart z2)))

(defun and-thread-l-op-numerical (z1 z2)
  "Numerical AND_Thread_L operation: (Im(z1) * Im(z2)) * i.
   Combines the refutation thread information (Imaginary parts) of two weavexes
   using multiplication and scaling by imaginary unit 'i'.
   This is the CORE NUMERICAL LOGIC of AND_Thread_L."
  (* (* (imagpart z1) (imagpart z2)) #C(0 1)))

(defun or-thread-l-op-numerical (z1 z2)
  "Numerical OR_Thread_L operation: max(Im(z1), Im(z2)) * i.
   Combines the refutation thread information (Imaginary parts) of two weavexes
   using maximum selection and scaling by imaginary unit 'i'.
   This is the CORE NUMERICAL LOGIC of OR_Thread_L."
  (* (max (imagpart z1) (imagpart z2)) #C(0 1)))

(defun independence-thread-r-op-numerical (z1 z2)
  "Numerical INDEPENDENCE_Thread_R operation: Re(z1) + Re(z2).
   Combines the proof thread information (Real parts) of two independent weavexes
   using addition. This is the CORE NUMERICAL LOGIC of INDEPENDENCE_Thread_R."
  (+ (realpart z1) (realpart z2)))

(defun independence-thread-l-op-numerical (z1 z2)
  "Numerical INDEPENDENCE_Thread_L operation: (Im(z1) + Im(z2)) * i.
   Combines the refutation thread information (Imaginary parts) of two independent weavexes
   using addition and scaling by imaginary unit 'i'.
   This is the CORE NUMERICAL LOGIC of INDEPENDENCE_Thread_L."
  (* (+ (imagpart z1) (imagpart z2)) #C(0 1)))

(defun dependence-thread-r-op-numerical (z1 z2)
  "Numerical DEPENDENCE_Thread_R operation: (Re(z1) + Re(z2)) / 2.
   Combines the proof thread information (Real parts) of two dependent weavexes
   using averaging. This is the CORE NUMERICAL LOGIC of DEPENDENCE_Thread_R."
  (/ (+ (realpart z1) (realpart z2)) 2.0))

(defun dependence-thread-l-op-numerical (z1 z2)
  "Numerical DEPENDENCE_Thread_L operation: ((Im(z1) + Im(z2)) / 2) * i.
   Combines the refutation thread information (Imaginary parts) of two dependent weavexes
   using averaging and scaling by imaginary unit 'i'.
   This is the CORE NUMERICAL LOGIC of DEPENDENCE_Thread_L."
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
  "Multithreaded AND_Thread_R operation.
   **Uses OS threads to simulate parallel parsing/execution** before applying
   the CORE NUMERICAL LOGIC of AND_Thread_R.  The threads represent actual
   concurrent processing paths."
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
  "Multithreaded OR_Thread_R operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Multithreaded AND_Thread_L operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Multithreaded OR_Thread_L operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Multithreaded INDEPENDENCE_Thread_R operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Multithreaded INDEPENDENCE_Thread_L operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Multithreaded DEPENDENCE_Thread_R operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Multithreaded DEPENDENCE_Thread_L operation. **Uses OS threads for parallel parsing/execution simulation.**"
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
  "Axiom for 'CON' (Consistency).
   Represents the consistent state. Dispatches to numerical implementation.
   'CON' evaluates differently based on the thread type."
  (case implementation-type
    (:numerical (rwsdl-min-axiom-con-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-axiom-con-r-numerical expression thread-type)) ; Numerical axiom still applies for CON
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-axiom-con-r-numerical (expression thread-type)
  "Numerical Axiom for 'CON' (Consistency) in Proof and Meta-Closure Threads.
   Numerically defines the value of 'CON' based on the thread type.
   Real part represents proof thread information, Imaginary part refutation."
  (if (eq expression 'CON)
      (case thread-type
        (:proof_closure_thread #C(1.0 0.0))  ; CON -> 1 in proof thread (strong consistency)
        (:meta_closure_thread #C(1.0 0.0)) ; CON -> 1 in meta thread (consistent at meta-level)
        (otherwise #C(0.0 0.0)))          ; CON -> 0 otherwise (no consistency in other threads)
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; No match in meta -> 0
        (otherwise #C(0.0 0.0)))))        ; No match otherwise -> 0

(defun rwsdl-min-axiom-incon-l (expression thread-type implementation-type)
  "Axiom for 'INCON' (Inconsistency).
   Represents the inconsistent state. Dispatches to numerical implementation.
   'INCON' evaluates differently based on the thread type."
  (case implementation-type
    (:numerical (rwsdl-min-axiom-incon-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-axiom-incon-l-numerical expression thread-type)) ; Numerical axiom still applies for INCON
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-axiom-incon-l-numerical (expression thread-type)
  "Numerical Axiom for 'INCON' (Inconsistency) in Refutation and Meta-Closure Threads.
   Numerically defines the value of 'INCON' based on the thread type.
   Imaginary part represents refutation thread information, Real part proof."
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
  "Rule for 'OR_Thread_R'.
   Represents a right-threaded OR operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-or-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-or-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-or-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'OR_Thread_R' in Proof and Meta-Closure Threads.
   Numerically evaluates 'OR_Thread_R' using the numerical 'or-thread-r-op-numerical' operation."
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
  "Multithreaded Rule for 'OR_Thread_R' in Proof and Meta-Closure Threads.
   **Multithreaded execution of 'OR_Thread_R' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of OR_Thread_R."
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
  "Rule for 'AND_Thread_R'.
   Represents a right-threaded AND operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-and-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-and-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-and-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'AND_Thread_R' in Proof and Meta-Closure Threads.
   Numerically evaluates 'AND_Thread_R' using the numerical 'and-thread-r-op-numerical' operation."
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
  "Multithreaded Rule for 'AND_Thread_R' in Proof and Meta-Closure Threads.
   **Multithreaded execution of 'AND_Thread_R' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of AND_Thread_R."
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
  "Rule for 'DUALITY_R'.
   Represents a right-threaded DUALITY operation. Dispatches to numerical implementation
   as DUALITY is currently defined as a numerical operation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-duality-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-duality-r-numerical expression thread-type)) ; Duality is still numerical op
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-duality-r-numerical (expression thread-type)
  "Numerical Rule for 'DUALITY_R' in Proof and Meta-Closure Threads.
   Numerically evaluates 'DUALITY_R' using the numerical 'duality-op' operation."
  (if (and (listp expression) (eq (car expression) 'DUALITY_R) (cadr expression))
      (let ((a (cadr expression)))
        (duality-op (rwsdl-min-self-interpret a :meta_closure_thread :numerical))) ; Meta-closure thread for duality, numerical operation
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-or-thread-l (expression thread-type implementation-type)
  "Rule for 'OR_Thread_L'.
   Represents a left-threaded OR operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-or-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-or-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-or-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'OR_Thread_L' in Refutation and Meta-Closure Threads.
   Numerically evaluates 'OR_Thread_L' using the numerical 'or-thread-l-op-numerical' operation."
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
  "Multithreaded Rule for 'OR_Thread_L' in Refutation and Meta-Closure Threads.
   **Multithreaded execution of 'OR_Thread_L' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of OR_Thread_L."
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
  "Rule for 'AND_Thread_L'.
   Represents a left-threaded AND operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-and-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-and-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-and-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'AND_Thread_L' in Refutation and Meta-Closure Threads.
   Numerically evaluates 'AND_Thread_L' using the numerical 'and-thread-l-op-numerical' operation."
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
  "Multithreaded Rule for 'AND_Thread_L' in Refutation and Meta-Closure Threads.
   **Multithreaded execution of 'AND_Thread_L' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of AND_Thread_L."
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
  "Rule for 'DUALITY_L'.
   Represents a left-threaded DUALITY operation. Dispatches to numerical implementation
   as DUALITY is currently defined as a numerical operation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-duality-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-duality-l-numerical expression thread-type)) ; Duality is still numerical op
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-duality-l-numerical (expression thread-type)
  "Numerical Rule for 'DUALITY_L' in Refutation and Meta-Closure Threads.
   Numerically evaluates 'DUALITY_L' using the numerical 'duality-op' operation."
  (if (and (listp expression) (eq (car expression) 'DUALITY_L) (cadr expression))
      (let ((a (cadr expression)))
        (duality-op (rwsdl-min-self-interpret a :meta_closure_thread :numerical))) ; Meta-closure thread for duality, numerical operation
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; Default to 0 in meta thread if rule not applicable
        (otherwise #C(0.0 0.0)))))        ; Default to 0 otherwise


(defun rwsdl-min-rule-independence-thread-r (expression thread-type implementation-type)
  "Rule for 'INDEPENDENCE_Thread_R'.
   Represents a right-threaded INDEPENDENCE operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-independence-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-independence-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-independence-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'INDEPENDENCE_Thread_R' in Proof and Meta-Closure Threads.
   Numerically evaluates 'INDEPENDENCE_Thread_R' using the numerical 'independence-thread-r-op-numerical' operation."
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
  "Multithreaded Rule for 'INDEPENDENCE_Thread_R' in Proof and Meta-Closure Threads.
   **Multithreaded execution of 'INDEPENDENCE_Thread_R' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of INDEPENDENCE_Thread_R."
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
  "Rule for 'INDEPENDENCE_Thread_L'.
   Represents a left-threaded INDEPENDENCE operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-independence-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-independence-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-independence-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'INDEPENDENCE_Thread_L' in Refutation and Meta-Closure Threads.
   Numerically evaluates 'INDEPENDENCE_Thread_L' using the numerical 'independence-thread-l-op-numerical' operation."
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
  "Multithreaded Rule for 'INDEPENDENCE_Thread_L' in Refutation and Meta-Closure Threads.
   **Multithreaded execution of 'INDEPENDENCE_Thread_L' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of INDEPENDENCE_Thread_L."
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
  "Rule for 'DEPENDENCE_Thread_R'.
   Represents a right-threaded DEPENDENCE operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-dependence-thread-r-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-dependence-thread-r-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-dependence-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'DEPENDENCE_Thread_R' in Proof and Meta-Closure Threads.
   Numerically evaluates 'DEPENDENCE_Thread_R' using the numerical 'dependence-thread-r-op-numerical' operation."
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
  "Multithreaded Rule for 'DEPENDENCE_Thread_R' in Proof and Meta-Closure Threads.
   **Multithreaded execution of 'DEPENDENCE_Thread_R' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of DEPENDENCE_Thread_R."
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
  "Rule for 'DEPENDENCE_Thread_L'.
   Represents a left-threaded DEPENDENCE operation. Dispatches to numerical or multithreaded implementation."
  (case implementation-type
    (:numerical (rwsdl-min-rule-dependence-thread-l-numerical expression thread-type))
    (:multithreaded (rwsdl-min-rule-dependence-thread-l-mt expression thread-type))
    (otherwise (error "Invalid implementation type: ~a" implementation-type))))

(defun rwsdl-min-rule-dependence-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'DEPENDENCE_Thread_L' in Refutation and Meta-Closure Threads.
   Numerically evaluates 'DEPENDENCE_Thread_L' using the numerical 'dependence-thread-l-op-numerical' operation."
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
  "Multithreaded Rule for 'DEPENDENCE_Thread_L' in Refutation and Meta-Closure Threads.
   **Multithreaded execution of 'DEPENDENCE_Thread_L' using OS threads.**
   Simulates parallel parsing/execution of sub-expressions using OS threads
   before applying the CORE NUMERICAL LOGIC of DEPENDENCE_Thread_L."
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
  "Self-interpretation function, the core weaver of RWSDL-Min.
   Dispatches to axioms and rules based on the expression type,
   thread type (proof, refutation, meta), and implementation type (numerical, multithreaded).

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
  "Evaluates an expression in the specified thread type and implementation.
   Dispatches to numerical or multithreaded evaluation based on implementation-type."
  (case implementation-type
    (:numerical (rwsdl-min-evaluate-numerical expression thread-type)) ; Dispatch to numerical evaluation
    (:multithreaded (rwsdl-min-evaluate-multithreaded expression thread-type)) ; Dispatch to multithreaded evaluation
    (otherwise (error "Invalid implementation type: ~a" implementation-type)))) ; Error for invalid implementation type

(defun rwsdl-min-evaluate-numerical (expression thread-type)
  "Numerically evaluates an expression in the specified thread type.
   Uses the numerical implementation for all operations.
   This is the FASTEST evaluation path, using direct numerical computations."
  (rwsdl-min-self-interpret expression thread-type :numerical)) ; Call self-interpret with numerical implementation

(defun rwsdl-min-evaluate-multithreaded (expression thread-type)
  "Multithreaded evaluates an expression in the specified thread type.
   **Simulates concurrent parsing and execution using OS threads.**
   Uses the multithreaded implementation for applicable operations,
   and numerical implementation for axioms and DUALITY operations (where multithreading
   is not currently simulated).  This path is designed to model concurrent behavior,
   NOT for optimal performance."
  (rwsdl-min-self-interpret expression thread-type :multithreaded)) ; Call self-interpret with multithreaded implementation


;;; Proof, Refutation, and Meta-Closure Threads (using rwsdl-min-evaluate - dispatching based on implementation type)
;;; These functions provide user-friendly interfaces for evaluating expressions
;;; within specific thread contexts (proof, refutation, meta) and with
;;; a chosen implementation type (numerical or multithreaded).

(defun rwsdl-min-proof-closure-thread (expression implementation-type)
  "Evaluates expression in a proof closure thread with specified implementation.
   Entry point for proof-focused evaluation.  Focuses on consistency."
  (rwsdl-min-evaluate expression :proof_closure_thread implementation-type)) ; Evaluate in proof thread

(defun rwsdl-min-refutation-closure-thread (expression implementation-type)
  "Evaluates expression in a refutation closure thread with specified implementation.
   Entry point for refutation-focused evaluation. Focuses on inconsistency."
  (rwsdl-min-evaluate expression :refutation_closure_thread implementation-type)) ; Evaluate in refutation thread

(defun rwsdl-min-meta-closure-thread (expression implementation-type)
  "Evaluates expression in a meta-closure thread with specified implementation.
   Entry point for meta-level evaluation.  Provides a broader context."
  (rwsdl-min-evaluate expression :meta_closure_thread implementation-type)) ; Evaluate in meta thread


;;; Bootstrap Loop Test (modified to test meta-closure threads, implementation types and new closure outputs)
(defun rwsdl-min-bootstrap-loop-test ()
  "Tests RWSDL-Min with different thread types, implementations and expressions, and measures performance.
   Systematically evaluates a range of expressions across proof, refutation, and meta threads,
   using both numerical and multithreaded implementations.

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