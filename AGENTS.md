# Weaver Agent Instructions

This document provides instructions for AI agents working on the Weaver codebase.

## Environment Setup

The project is implemented in Common Lisp and requires SBCL and Quicklisp.

1.  **SBCL:** The primary Common Lisp implementation. It must be installed on the system. The agent can install it using the appropriate package manager (e.g., `sudo apt-get install -y sbcl`).
2.  **Quicklisp:** The project uses Quicklisp for managing Lisp library dependencies. The dependencies themselves are loaded via the `QHJ.lisp` file, which is called from the main `Preamble.lisp`.

## Running the Application

The entire system, including the web server, is started by loading the `Preamble.lisp` file into an SBCL instance:

```bash
sbcl --no-sysinit --no-userinit --load Preamble.lisp
```

## Running Tests

The project contains several test files. To run the primary tests, load them after the main `Preamble.lisp` file:

```bash
sbcl --no-sysinit --no-userinit --load Preamble.lisp --eval '(load "MacroTests.lisp")' --eval '(load "IdentityCheck1_5.lisp")' --quit
```

## Project Structure

-   `Preamble.lisp`: Main entry point. Loads all necessary files and starts services.
-   `refuter-api.lisp`: The Hunchentoot web server and API logic.
-   `QHJ.lisp` / `refuteloader.lisp`: Scripts for loading Quicklisp dependencies.
-   `*.lisp`, `*.lsp`, `*.LSD`: Core logic for the theorem prover and refuter.
-   `index.html`, `style.css`, `script.js`: The static frontend application.
