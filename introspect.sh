#!/bin/bash

# Linux Process Privilege Inspector Script (with Strace Diagnostic)
# This script inspects the privileges and context of a running process.
# It gathers information related to file permissions, capabilities,
# security contexts (SELinux/AppArmor), environment variables,
# resource limits, and file system namespaces.
# Now includes file access tests from the perspective of the script's user
# and a function to guide strace diagnostics.

# --- Configuration ---
# Set this to true to enable verbose output for environment variables
VERBOSE_ENV=false

# --- Helper Functions ---

# Function to find the PID of a process by name
find_pid_by_name() {
    local app_name="$1"
    # Use pgrep to find PIDs matching the full command line
    # -f: Match against the full command line
    pgrep -f "$app_name"
}

# Function to display information for a given PID
inspect_pid() {
    local pid="$1"
    echo "--- Inspecting Process PID: $pid ---"

    if [ -z "$pid" ] || [ ! -d "/proc/$pid" ]; then
        echo "Error: Invalid or non-existent process PID '$pid'."
        return 1
    fi

    echo ""
    echo "--- Executable Information ---"
    # Get the path to the executable using /proc/<PID>/exe (more reliable)
    executable_path=$(readlink /proc/$pid/exe)
    if [ -n "$executable_path" ]; then
        echo "Executable Path: $executable_path"
        echo "Executable Permissions:"
        # Use -L to follow symlinks if /proc/<PID>/exe is a symlink
        ls -lLdZ "$executable_path" 2>/dev/null || ls -lLd "$executable_path"
        # Check capabilities on the executable file
        echo "Executable Capabilities:"
        getcap "$executable_path" 2>/dev/null || echo "  (No capabilities found or getcap not available)"
    else
        echo "Could not determine executable path for PID $pid."
    fi

    echo ""
    echo "--- Process Security Context ---"
    # Check SELinux context if available
    echo "SELinux Context:"
    ps -p $pid -o label= 2>/dev/null || echo "  (SELinux not enabled or context not available)"

    # Check AppArmor status (requires sudo)
    echo "AppArmor Status:"
    if command -v apparmor_status &> /dev/null; then
        # Check if the process is confined by AppArmor
        sudo apparmor_status | grep "process $pid" 2>/dev/null || echo "  (No specific AppArmor profile for this process or requires sudo)"
    else
        echo "  (AppArmor not installed or apparmor_status command not found)"
    fi

    echo ""
    echo "--- File System Context ---"
    # Check current working directory
    echo "Current Working Directory (CWD):"
    cwd_path=$(readlink /proc/$pid/cwd)
    echo "$cwd_path"

    # Check root directory (usually '/' unless chrooted)
    echo "Root Directory:"
    root_path=$(readlink /proc/$pid/root)
    echo "$root_path"

    # Check permissions on the CWD and Root (requires sudo for root or restricted paths)
    echo "Permissions on CWD:"
    ls -ldZ "$cwd_path" 2>/dev/null || ls -ld "$cwd_path"
    echo "Permissions on Root:"
    ls -ldZ "$root_path" 2>/dev/null || ls -ld "$root_path"

    echo ""
    echo "--- User and Group Information ---"
    # Get process user and group IDs
    echo "User and Group IDs:"
    cat /proc/$pid/status | grep -E "Uid:|Gid:"

    # Get supplementary groups using 'id' command (requires sudo)
    echo "Supplementary Groups (using 'sudo id <pid>'):"
    if command -v id &> /dev/null; then
        sudo id "$pid" 2>/dev/null || echo "  (Requires sudo or id command not available)"
    else
        echo "  (id command not found)"
    fi

    echo ""
    echo "--- File Access Tests (from script's user) ---"
    echo "Attempting file access from the perspective of the user running this script."

    if [ -n "$cwd_path" ]; then
        echo "Listing contents of CWD ($cwd_path):"
        ls -l "$cwd_path"
        if [ $? -ne 0 ]; then
            echo "  Error listing CWD contents. Check permissions for user running script."
        fi

        # Assuming index.html is in the CWD for testing
        test_file="$cwd_path/index.html"
        echo "Attempting to read first line of $test_file:"
        if [ -f "$test_file" ]; then
            head -n 1 "$test_file"
            if [ $? -ne 0 ]; then
                 echo "  Error reading $test_file. Check permissions for user running script."
            fi
        else
            echo "  Test file $test_file not found by script."
        fi
    else
        echo "  Could not determine CWD to perform file access tests."
    fi


    echo ""
    echo "--- Environment Variables ---"
    echo "Environment Variables (showing count, set VERBOSE_ENV=true for full list):"
    # Count environment variables
    env_count=$(cat /proc/$pid/environ | tr '\0' '\n' | wc -l)
    echo "  Total variables: $env_count"
    if [ "$VERBOSE_ENV" = true ]; then
        echo "--- Full Environment ---"
        cat /proc/$pid/environ | tr '\0' '\n'
        echo "------------------------"
    else
        echo "  (Set VERBOSE_ENV=true in the script to see full list)"
    fi


    echo ""
    echo "--- Resource Limits ---"
    echo "Resource Limits (ulimit):"
    cat /proc/$pid/limits

    echo ""
    echo "--- Done Inspecting PID: $pid ---"
}

# Function to guide strace diagnostics
run_strace_diagnostic() {
    local app_name="$1"
    local pid="$2"
    local log_file="strace_${app_name}_${pid}.log"

    echo ""
    echo "--- Strace Diagnostic Guide ---"
    echo "To perform a low-level trace of system calls for PID $pid ($app_name):"
    echo "1. Open a NEW terminal window."
    echo "2. Run the following command (you may need sudo):"
    echo ""
    echo "   sudo strace -f -p $pid -o $log_file"
    echo ""
    echo "   - 'sudo': May be required to attach strace to the process."
    echo "   - '-f': Follow child processes (threads) if any."
    echo "   - '-p $pid': Attach to the running process with PID $pid."
    echo "   - '-o $log_file': Write the strace output to '$log_file'."
    echo ""
    echo "3. Go back to the terminal where the '$app_name' process is running (PID $pid)."
    echo "4. In that process's REPL/interface, perform the action that is failing (e.g., run the LISP directory or file access commands)."
    echo "5. Once the action has completed or failed, go back to the NEW terminal where strace is running and press Ctrl+C to stop strace."
    echo "6. Examine the generated log file ('$log_file') for system calls related to file access (e.g., openat, newfstatat, getdents64, access) targeting the problematic path (/home/xxx/Documents/Weaver/ or /Weaver/). Look for the return value (often -1) and the error name in parentheses (e.g., ENOENT, EACCES, EPERM)."
    echo ""
    echo "This log file will show exactly what the kernel is reporting when the process attempts file access."
    echo "-------------------------------"
}


# --- Main Script Logic ---

if [ $# -eq 0 ]; then
    echo "Usage: $0 <application_name_or_pid> [pid_if_name_ambiguous] [--strace]"
    echo "Example: $0 gnome-terminal"
    echo "Example: $0 sbcl"
    echo "Example: $0 12345 (where 12345 is the PID)"
    echo "Example: $0 sbcl --strace (Inspect and then guide strace)"
    echo "Example: $0 12345 --strace (Inspect PID 12345 and then guide strace)"
    exit 1
fi

app_input="$1"
run_strace=false

# Check for --strace argument
if [ "$#" -ge 2 ] && [ "$2" == "--strace" ]; then
    run_strace=true
    # If --strace is the second arg, shift to process the first arg as app_input
    if [ "$#" -eq 2 ]; then
      shift # Remove app_input
    else # If --strace is the third arg (e.g., app_name pid --strace)
      # Do nothing, app_input and pid are already in $1 and $2
      :
    fi
elif [ "$#" -ge 3 ] && [ "$3" == "--strace" ]; then
    run_strace=true
    # app_input is $1, pid is $2
    :
fi


# Check if the input is a PID (all digits)
if [[ "$app_input" =~ ^[0-9]+$ ]]; then
    pid_to_inspect="$app_input"
    app_name_for_strace="PID_${pid_to_inspect}" # Use PID for name if input was PID
    inspect_pid "$pid_to_inspect"
    if [ "$run_strace" = true ]; then
        run_strace_diagnostic "$app_name_for_strace" "$pid_to_inspect"
    fi
else
    # Input is an application name
    app_name_for_strace="$app_input"
    pids=$(find_pid_by_name "$app_input")

    # Filter out the script's own PID if running with -f
    current_script_pid=$$
    filtered_pids=()
    while IFS= read -r line; do
        if [ "$line" != "$current_script_pid" ] && [ -n "$line" ]; then
            filtered_pids+=("$line") # Add non-empty, non-script PID lines to an array
        fi
    done <<< "$pids"

    pid_count=${#filtered_pids[@]} # Get the count of valid PIDs

    if [ "$pid_count" -eq 0 ]; then
        echo "No running process found with name matching '$app_input'."
        exit 1
    elif [ "$pid_count" -eq 1 ]; then
        pid_to_inspect="${filtered_pids[0]}"
        echo "Found one process matching '$app_input'."
        inspect_pid "$pid_to_inspect"
        if [ "$run_strace" = true ]; then
            run_strace_diagnostic "$app_name_for_strace" "$pid_to_inspect"
        fi
    else
        echo "Found multiple processes matching '$app_input':"
        printf "%s\n" "${filtered_pids[@]}" # Print PIDs from the array
        echo ""
        echo "Please specify the PID you want to inspect as the second argument, optionally followed by --strace."
        echo "Example: $0 $app_input <PID>"
        echo "Example: $0 $app_input <PID> --strace"
        if [ -n "$2" ] && [[ "$2" =~ ^[0-9]+$ ]]; then
             pid_to_inspect="$2"
             inspect_pid "$pid_to_inspect"
             if [ "$run_strace" = true ]; then
                 run_strace_diagnostic "$app_name_for_strace" "$pid_to_inspect"
             fi
        fi
        exit 1
    fi
fi

exit 0

