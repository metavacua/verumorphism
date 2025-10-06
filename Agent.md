# Jules Agent Protocol v1.0

This document outlines the master protocol for the AI agent "Jules" operating within this repository. Adherence to this protocol is mandatory for all tasks.

## 1. Temporal Orientation

**Objective:** To overcome knowledge cutoffs by consulting external, up-to-date information before beginning any task.

**Procedure:**
1.  Identify the core technologies and libraries relevant to the task (e.g., Common Lisp, SBCL, Quicklisp, Hunchentoot).
2.  Use available tools (`google_search`, `view_text_website`) to research the current state, best practices, and potential issues related to these technologies.
3.  Cache relevant findings in `knowledge_core/temporal_orientation.md`.

## 2. Contextualization

**Objective:** To build a comprehensive understanding of the existing codebase and its architecture.

**Procedure:**
1.  Analyze the repository structure using `list_files`.
2.  Review key files (`Preamble.lisp`, `refuter-api.lisp`, etc.) to understand the application's entry points and logic.
3.  Utilize static analysis tools (when available) to populate the `knowledge_core`:
    *   `knowledge_core/asts/`: Store Abstract Syntax Trees of parsed source files.
    *   `knowledge_core/symbols.json`: Map all identified code symbols.
    *   `knowledge_core/dependency_graph.json`: Document dependencies between code components.
4.  Consult `knowledge_core/llms.txt` for existing project-specific domain knowledge.

## 3. Information Retrieval (RAG)

**Objective:** To synthesize internal knowledge with just-in-time external research to inform planning.

**Procedure:**
1.  Combine the understanding from the **Contextualization** step with the information gathered during **Temporal Orientation**.
2.  Formulate targeted queries to resolve any remaining ambiguities about the task or implementation strategy.

## 4. Planning & Self-Correction

**Objective:** To generate and critically review a detailed, evidence-based action plan.

**Procedure:**
1.  Based on the synthesized information, create a step-by-step plan to accomplish the task.
2.  Use the `set_plan` tool to record the plan.
3.  Critically review the plan for potential flaws, edge cases, or omissions. Refine as necessary.
4.  If the plan changes significantly during execution, update it using `set_plan` and log the rationale.

## 5. Execution & Logging

**Objective:** To perform the planned tasks and meticulously record every action.

**Procedure:**
1.  Execute each step of the plan using the available tools.
2.  **Verify every change.** After each modification, use a read-only tool (e.g., `read_file`, `list_files`) to confirm the change was successful.
3.  Log every action taken (tool calls, observations, errors) in `logs/activity.log.jsonl`, adhering to the schema in `LOGGING_SCHEMA.md`.

### Project-Specific Execution Instructions

**Environment Setup:**
*   **Primary Technology:** Steel Bank Common Lisp (SBCL) with Quicklisp.
*   **Initial Setup:** Run the `setup.sh` script to install SBCL and Quicklisp.
    ```bash
    ./setup.sh
    ```
*   **Loading Dependencies:** The primary entry point is `Preamble.lisp`. Loading this file will handle all necessary dependencies.

**Running the Application:**
*   Start an SBCL session and execute:
    ```lisp
    (load "Preamble.lisp")
    ```
*   This will start the required web servers (Refuter API on port 8080 and a static file server).

**Running Tests:**
*   There is no automated test suite.
*   Manual tests can be found in files like `Weaver_system.lisp`. The `test-weaver-package` function is a key example. Future work should focus on creating a formal testing framework.

## 6. Post-Mortem & Learning

**Objective:** To analyze performance after completing a task and identify opportunities for improvement.

**Procedure:**
1.  After submitting the work, complete the `postmortem.md` template.
2.  Analyze the activity log to identify inefficiencies, errors, or successful strategies.
3.  Update the `knowledge_core/llms.txt` with new, hard-won knowledge to improve future performance.
4.  Reflect on the process and suggest improvements to this protocol.