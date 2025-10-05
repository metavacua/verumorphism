# Weaver: An Experimental Theorem Prover

## 1. Purpose

Weaver is a highly experimental theorem prover and refutation engine for a non-classical, distributed logic. The system is designed to explore concepts of dependence, independence, and duality within a logical framework built on a unique operator algebra. The core of the system is implemented in Common Lisp, with a web-based front-end for user interaction.

The project is architected with a distinction between the core logical library and the program execution layer, allowing for modular development and testing. It makes extensive use of Common Lisp's features, including macros, threading, and the Common Lisp Object System (CLOS), to implement its logic.

## 2. Setup

To set up and run the Weaver system, you will need a Common Lisp environment with SBCL and Quicklisp.

### 2.1. Prerequisites

*   **SBCL (Steel Bank Common Lisp):** A high-performance Common Lisp compiler. You can download it from [sbcl.org](http://www.sbcl.org/platform-table.html) or install it using your system's package manager (e.g., `sudo apt-get install sbcl` on Debian/Ubuntu, `brew install sbcl` on macOS).

*   **Quicklisp:** A library manager for Common Lisp. If you don't have it installed, follow the instructions at [quicklisp.beta.quicklisp.org](https://www.quicklisp.org/beta/).

### 2.2. Installation

1.  **Clone the Repository:**
    ```bash
    git clone <repository-url>
    cd <repository-directory>
    ```

2.  **Load Dependencies and Start the System:**
    The entire system is designed to be loaded and run from the `Preamble.lisp` file. This file handles the setup of all necessary dependencies and starts the required servers.

    Start an SBCL REPL from the project's root directory and load the `Preamble.lisp` file:
    ```lisp
    * (load "Preamble.lisp")
    ```
    This will:
    *   Load the necessary libraries (Hunchentoot for the web server, Jonathan for JSON parsing) via Quicklisp.
    *   Load all the core Lisp files for the theorem prover and refuter.
    *   Start the web server on `http://localhost:8080`.

## 3. Usage

Once the system is running, you can interact with it through the web interface or the API.

### 3.1. Web Interface

Open your web browser and navigate to `http://localhost:8080`. You will see the "Logical Formula Refuter" interface, where you can:

*   **Enter a Formula:** Type a formula in the text area, using S-expression syntax (e.g., `(dep (incon) (incon))`).
*   **Refute:** Click the "Refute Formula" button to send the formula to the back-end for processing. The result will be displayed in the "Refutation Result" text area.
*   **Server Controls:** Stop or restart the server directly from the UI.

### 3.2. API Endpoint

The system exposes a single API endpoint for programmatic access:

*   **Endpoint:** `/refute`
*   **Method:** `POST`
*   **Request Body:** A JSON object with a single key, `"formula"`, containing the S-expression as a string.
    ```json
    {
      "formula": "(dep (incon) (incon))"
    }
    ```
*   **Success Response:** A JSON object indicating the refutation status.
    ```json
    {
      "status": "success",
      "refuted": true,
      "refuted_formula": "(DEP (INCON) (INCON))"
    }
    ```

## 4. Project Structure

The project is organized into several key files and directories:

*   **`Preamble.lisp`**: The main entry point for the application. It loads all other necessary files and starts the servers.
*   **`refuter-api.lisp`**: Contains the Hunchentoot web server setup, including the API endpoint and static file serving logic.
*   **`*.lisp`, `*.lsp`, `*.LSD`**: These files contain the core logic of the Weaver theorem prover and refuter. They are organized into different versions and modules, reflecting the evolution of the project.
*   **`index.html`, `style.css`, `script.js`**: These files make up the single-page web application that serves as the front-end for interacting with the refuter.
*   **`QHJ.lisp`, `refuteloader.lisp`**: Utility files for loading dependencies and setting up the Lisp environment.
*   **`AGENTS.md`**: Provides instructions and guidelines for AI agents working on this codebase.