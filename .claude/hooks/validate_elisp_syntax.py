#!/usr/bin/env python3
# pylint: disable=broad-exception-caught,duplicate-code
"""
Claude and Copilot Hook: Elisp Syntax Validator
=========================================
This hook validates Write, Edit, MultiEdit and Copilot apply_patch operations.
It uses Emacs' check-parens function to ensure well-formed S-expressions.

Copilot CLI reads the shared .claude/settings.json hook registration.
Both snake_case and camelCase payloads are accepted, including JSON-encoded
arguments and raw apply_patch strings (also delivered under the Edit alias).
Patch contents are reconstructed in memory; source files are never written.
Unsupported or unreadable edit payloads are denied with an explicit reason.

Validation uses emacsclient and a temporary buffer, without loading user code
or terminating the server. An Emacs server must already be running. Set
ELISP_VALIDATION_SOCKET to use a dedicated validation server.

Run regression tests with:
  python3 -m unittest discover -s .claude/hooks -p 'test_validate_elisp_syntax.py'

Configuration in .claude/settings.json (shared by Claude Code and Copilot CLI):
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit|apply_patch|create|edit",
        "hooks": [
          {
            "type": "command",
            "command": "python3 .claude/hooks/validate_elisp_syntax.py"
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
import tempfile
from pathlib import Path


def validate_elisp_syntax(content):
    """Validate Elisp syntax using Emacs' check-parens function."""
    try:
        with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", suffix=".el") as source:
            source.write(content)
            source.flush()
            expression = f"""
(with-temp-buffer
  (insert-file-contents {json.dumps(source.name)})
  (emacs-lisp-mode)
  (goto-char (point-min))
  (condition-case err
      (progn (check-parens) "SYNTAX_OK")
    (error (concat "SYNTAX_ERROR: " (error-message-string err)))))
"""
            command = ["emacsclient", "--alternate-editor=false"]
            if os.environ.get("ELISP_VALIDATION_SOCKET"):
                command += ["--socket-name", os.environ["ELISP_VALIDATION_SOCKET"]]
            result = subprocess.run(
                command + ["--eval", expression], capture_output=True,
                text=True, timeout=5, check=False,
            )
        if result.returncode == 0 and result.stdout.strip() == '"SYNTAX_OK"':
            return True, None
        return False, (result.stderr or result.stdout).strip() or "No validation result"
    except subprocess.TimeoutExpired:
        return False, "Validation timeout"
    except Exception as e:
        return False, f"Validation error: {e}"


def apply_edit(content, old_string, new_string, replace_all=False):
    """Apply an edit operation to content."""
    if not old_string:  # File creation
        return new_string

    if replace_all:
        if old_string not in content:
            raise ValueError("Could not find old_string in content")
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


def patch_contents(patch, cwd):
    """Return proposed file contents without writing the apply_patch changes."""
    lines = patch.splitlines()
    if not lines or lines[0] != "*** Begin Patch" or lines[-1] != "*** End Patch":
        raise ValueError("Expected a complete apply_patch payload")
    changes = {}
    index = 1
    while index < len(lines) - 1:
        header = lines[index]
        index += 1
        operation, separator, filename = header.partition(": ")
        if not separator or operation not in (
                "*** Add File", "*** Update File", "*** Delete File"):
            raise ValueError(f"Invalid patch header: {header}")
        path = Path(cwd) / filename
        if operation == "*** Delete File":
            changes[path] = None
            continue
        if operation == "*** Add File":
            added = []
            while index < len(lines) - 1 and lines[index].startswith("+"):
                added.append(lines[index][1:])
                index += 1
            changes[path] = "\n".join(added) + "\n"
            continue
        destination = path
        if lines[index].startswith("*** Move to: "):
            destination = Path(cwd) / lines[index].split(": ", 1)[1]
            changes[path] = None
            index += 1
        content = changes.get(path)
        original = (content if content is not None else path.read_text(encoding="utf-8")).splitlines()
        position = 0
        while index < len(lines) - 1 and not lines[index].startswith("*** "):
            marker = lines[index]
            if marker != "@@" and not marker.startswith("@@ "):
                raise ValueError(f"Expected a hunk marker: {marker}")
            index += 1
            if marker.startswith("@@ "):
                context = marker[3:]
                position = original.index(context, position) + 1
            before, after = [], []
            while index < len(lines) - 1:
                line = lines[index]
                if line.startswith(("@@", "*** ")):
                    break
                if not line or line[0] not in " +-":
                    raise ValueError(f"Invalid patch line: {line}")
                if line[0] in " -":
                    before.append(line[1:])
                if line[0] in " +":
                    after.append(line[1:])
                index += 1
            at_end = lines[index] == "*** End of File"
            if at_end:
                index += 1
            candidates = range(position, len(original) - len(before) + 1)
            match = next((start for start in candidates
                          if original[start:start + len(before)] == before
                          and (not at_end or start + len(before) == len(original))), None)
            if match is None:
                raise ValueError(f"Cannot locate patch context in {filename}")
            original[match:match + len(before)] = after
            position = match + len(after)
        changes[destination] = "\n".join(original) + "\n"
    return changes


def proposed_contents(input_data):
    """Normalize Claude and Copilot payloads into proposed file contents."""
    tool_name = input_data.get("tool_name", input_data.get("toolName", ""))
    tool_name = tool_name.rsplit(".", 1)[-1]
    tool_input = input_data.get("tool_input", input_data.get("toolArgs", {}))
    cwd = input_data.get("cwd", os.getcwd())
    if isinstance(tool_input, str) and not tool_input.startswith("*** Begin Patch"):
        tool_input = json.loads(tool_input)
    if tool_name == "apply_patch" or (
            isinstance(tool_input, str) and tool_input.startswith("*** Begin Patch")):
        patch = tool_input if isinstance(tool_input, str) else tool_input.get("input")
        if not isinstance(patch, str):
            raise ValueError("apply_patch requires a patch string")
        return patch_contents(patch, cwd)
    names = {"write": "Write", "create": "Write", "edit": "Edit"}
    tool_name = names.get(tool_name, tool_name)
    if tool_name not in ("Write", "Edit", "MultiEdit"):
        return {}
    if not isinstance(tool_input, dict):
        raise ValueError("Edit arguments must be an object")
    filename = tool_input.get("file_path", tool_input.get("path", ""))
    if not filename:
        raise ValueError("Missing edit file path")
    path = Path(cwd) / filename
    if path.suffix != ".el":
        return {}
    tool_input = dict(tool_input)
    for old, new in (("old_str", "old_string"), ("new_str", "new_string")):
        if old in tool_input:
            tool_input[new] = tool_input[old]
    current = path.read_text(encoding="utf-8") if path.exists() else ""
    return {path: get_resulting_content(tool_name, tool_input, current)}


def deny(reason, camel_case=False):
    """Emit the denial format used by the calling hook runtime."""
    decision = {"permissionDecision": "deny", "permissionDecisionReason": reason}
    output = decision if camel_case else {
        "hookSpecificOutput": {"hookEventName": "PreToolUse", **decision}}
    print(json.dumps(output))


def main():
    """Entry point for the Elisp syntax validation hook."""
    try:
        input_data = json.load(sys.stdin)
    except Exception as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)

    camel_case = "toolName" in input_data
    try:
        changes = proposed_contents(input_data)
        for path, content in changes.items():
            if path.suffix == ".el" and content is not None:
                valid, error_msg = validate_elisp_syntax(content)
                if not valid:
                    deny(f"Blocked: Elisp validation failed for {path}: {error_msg}",
                         camel_case)
                    return
    except (ValueError, OSError, TypeError, AttributeError) as err:
        deny(f"Blocked: Cannot validate the proposed edit: {err}", camel_case)


if __name__ == "__main__":
    main()
