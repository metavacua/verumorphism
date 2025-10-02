#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e

# --- Install SBCL ---
echo "Installing Steel Bank Common Lisp (SBCL)..."
sudo apt-get update -y
sudo apt-get install -y sbcl

# --- Install Quicklisp ---
echo "Installing Quicklisp..."

# Define the URL for the Quicklisp installer
QUICKLISP_URL="https://beta.quicklisp.org/quicklisp.lisp"

# Define the path for the Quicklisp setup file
QUICKLISP_LISP_PATH="/tmp/quicklisp.lisp"

# Download the Quicklisp installer
curl -L -o "$QUICKLISP_LISP_PATH" "$QUICKLISP_URL"

# Run the Quicklisp installation script using SBCL
# This will install Quicklisp into ~/quicklisp/
sbcl --no-sysinit --no-userinit --load "$QUICKLISP_LISP_PATH" \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --eval '(quit)'

echo "Setup complete. SBCL and Quicklisp are installed."