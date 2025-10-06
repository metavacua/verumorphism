# Weaver Agent Instructions

This document provides instructions for agents, like Jules, to work with the Weaver project.

## Environment Setup

The primary development environment for this project is **Steel Bank Common Lisp (SBCL)**. Dependencies are managed by **Quicklisp**.

To set up the development environment, follow these steps:

1.  **Install SBCL and Quicklisp:**
    -   Run the `setup.sh` script located in the root of the repository. This will install SBCL and set up Quicklisp in the home directory.

2.  **Load Dependencies:**
    -   Start an SBCL session.
    -   Load the project's entry point by executing: `(load "Preamble.lisp")`
    -   This will automatically:
        -   Load Quicklisp.
        -   Download and load the required libraries (`Hunchentoot` and `Jonathan`).
        -   Start the necessary web servers.

## Running the Application

The application is started by loading the `Preamble.lisp` file in an SBCL session:

```lisp
(load "Preamble.lisp")
```

This will start two web servers:

1.  A **Refuter API server** on port 8080.
2.  A **static file server** for the frontend, also on a specified port.

The `*frontend-directory*` variable in `Preamble.lisp` should point to the directory containing the frontend files. This has been updated to use a relative path.

## Running Tests

This project does not yet have a formal, automated test suite. However, several files contain example usage and ad-hoc tests that can be run manually. For example, `Weaver_system.lisp` contains a `test-weaver-package` function that can be uncommented and run to test the `weaver-system` package.

Future work should include creating a more robust, automated testing framework.