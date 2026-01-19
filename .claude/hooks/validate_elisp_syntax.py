#!/usr/bin/env python3
# pylint: disable=broad-exception-caught,duplicate-code
"""
Claude Code Hook: Elisp Syntax Validator
=========================================
This hook validates Emacs Lisp syntax for Write, Edit and MultiEdit operations.
It uses Emacs' check-parens function to ensure well-formed S-expressions.

Configuration in settings.local.json:
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write",
        "hooks": [
          {
            "type": "command",
            "command": "python3 /path/to/validate-elisp-syntax.py"
          }
        ]
      },
      {
        "matcher": "Edit",
        "hooks": [
          {
            "type": "command",
            "command": "python3 /path/to/validate-elisp-syntax.py"
          }
        ]
      },
      {
        "matcher": "MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "python3 /path/to/validate-elisp-syntax.py"
          }
        ]
      }
    ]
  }
}
"""

import json
import os
import subprocess
import sys


def validate_elisp_syntax(content):
    """Validate Elisp syntax using Emacs' check-parens function."""
    # Create the Emacs batch command
    emacs_cmd = [
        "emacs",
        "-batch",
        "--eval",
        """(progn
             (insert-file-contents "/dev/stdin")
             (emacs-lisp-mode)
             (goto-char (point-min))
             (condition-case err
                 (progn
                   (check-parens)
                   (message "SYNTAX_OK"))
                 (error
                   (message "SYNTAX_ERROR: %s" err)
                   (kill-emacs 1))))""",
    ]

    try:
        result = subprocess.run(
            emacs_cmd,
            input=content,
            capture_output=True,
            text=True,
            timeout=5,
            check=False,
        )

        # Check if syntax is valid
        if result.returncode == 0 and "SYNTAX_OK" in result.stderr:
            return True, None
        # Extract error message
        error_msg = result.stderr.strip()
        if "SYNTAX_ERROR:" in error_msg:
            error_msg = error_msg.split("SYNTAX_ERROR:", 1)[1].strip()
        return False, error_msg
    except subprocess.TimeoutExpired:
        return False, "Validation timeout"
    except Exception as e:
        return False, f"Validation error: {e}"


def apply_edit(content, old_string, new_string, replace_all=False):
    """Apply an edit operation to content."""
    if not old_string:  # File creation
        return new_string

    if replace_all:
        return content.replace(old_string, new_string)
    # Replace only first occurrence
    idx = content.find(old_string)
    if idx == -1:
        raise ValueError("Could not find old_string in content")
    return content[:idx] + new_string + content[idx + len(old_string) :]


def get_resulting_content(tool_name, tool_input, current_content):
    """Apply the appropriate edits based on tool type and return resulting content."""
    if tool_name == "Write":
        # For Write tool, the content is directly provided
        return tool_input.get("content", "")

    if tool_name == "Edit":
        return apply_edit(
            current_content,
            tool_input.get("old_string", ""),
            tool_input.get("new_string", ""),
            tool_input.get("replace_all", False),
        )

    if tool_name == "MultiEdit":
        resulting_content = current_content
        for edit in tool_input.get("edits", []):
            resulting_content = apply_edit(
                resulting_content,
                edit.get("old_string", ""),
                edit.get("new_string", ""),
                edit.get("replace_all", False),
            )
        return resulting_content

    raise ValueError(f"Unexpected tool name: {tool_name}")


def main():
    """Entry point for the Elisp syntax validation hook."""
    try:
        input_data = json.load(sys.stdin)
    except Exception as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)

    tool_name = input_data.get("tool_name", "")
    tool_input = input_data.get("tool_input", {})
    file_path = tool_input.get("file_path", "")

    # Check if this is an Elisp file
    if not file_path.endswith(".el"):
        # Not an Elisp file, allow the edit
        sys.exit(0)

    # Read current file content if it exists
    current_content = ""
    if os.path.exists(file_path):
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                current_content = f.read()
        except Exception:
            # If we can't read the file, allow the edit
            # Claude will handle the actual file operation errors
            sys.exit(0)

    # Apply the edit(s) to get the resulting content
    try:
        resulting_content = get_resulting_content(
            tool_name, tool_input, current_content
        )
    except Exception:
        # If we can't apply the edit, let Claude handle it
        sys.exit(0)

    # Validate the resulting content
    is_valid, error_msg = validate_elisp_syntax(resulting_content)

    if not is_valid:
        # Block the edit
        output = {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "deny",
                "permissionDecisionReason": (
                    f"Blocked: This edit would create invalid Elisp syntax. "
                    f"Emacs check-parens reported: {error_msg}. "
                    f"Please ensure all parentheses, brackets, and quotes are balanced."
                ),
            }
        }
        print(json.dumps(output))
        sys.exit(0)

    # Allow the edit
    sys.exit(0)


if __name__ == "__main__":
    main()
