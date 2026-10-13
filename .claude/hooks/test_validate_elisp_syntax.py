"""Regression tests for Claude and Copilot edit payloads."""

import contextlib
import importlib.util
import io
import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    "validator", Path(__file__).with_name("validate_elisp_syntax.py"))
validator = importlib.util.module_from_spec(spec)
spec.loader.exec_module(validator)


class ValidatorTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.source = self.root / "example.el"
        self.source.write_text("(first)\n(second)\n(third)\n", encoding="utf-8")

    def payload(self, name, arguments, camel=False):
        return {"cwd": str(self.root),
                "toolName" if camel else "tool_name": name,
                "toolArgs" if camel else "tool_input": arguments}

    def run_hook(self, payload, valid=True):
        output = io.StringIO()
        with patch("sys.stdin", io.StringIO(json.dumps(payload))), \
                contextlib.redirect_stdout(output), \
                patch.object(validator, "validate_elisp_syntax",
                             return_value=(valid, "Unmatched bracket")) as validate:
            validator.main()
        return output.getvalue(), validate

    def test_claude_operations(self):
        for name, args, expected in (
                ("Write", {"content": "(new)\n"}, "(new)\n"),
                ("Edit", {"old_string": "(second)", "new_string": "(new)"},
                 "(first)\n(new)\n(third)\n"),
                ("MultiEdit", {"edits": [
                    {"old_string": "(first)", "new_string": "(new)"},
                    {"old_string": "(third)", "new_string": "(last)"}]},
                 "(new)\n(second)\n(last)\n")):
            with self.subTest(name=name):
                args["file_path"] = str(self.source)
                self.assertEqual({self.source: expected},
                                 validator.proposed_contents(self.payload(name, args)))

    def test_copilot_edit_aliases_and_encoded_arguments(self):
        args = {"path": "example.el", "old_str": "(first)", "new_str": "(new)"}
        for value in (args, json.dumps(args)):
            self.assertEqual("(new)\n(second)\n(third)\n",
                             validator.proposed_contents(
                                 self.payload("functions.edit", value, True))[self.source])

    def test_patch_operations_multiple_hunks_and_edit_alias(self):
        raw = """*** Begin Patch
*** Add File: new.el
+(new)
*** Update File: example.el
@@
-(first)
+(one)
@@ (second)
-(third)
+(three)
*** End of File
*** Delete File: obsolete.el
*** End Patch"""
        for name, value, camel in (
                ("apply_patch", raw, False), ("Edit", raw, False),
                ("apply_patch", {"input": raw}, True),
                ("apply_patch", json.dumps({"input": raw}), True)):
            changes = validator.proposed_contents(self.payload(name, value, camel))
            self.assertEqual("(one)\n(second)\n(three)\n", changes[self.source])
            self.assertEqual("(new)\n", changes[self.root / "new.el"])
            self.assertIsNone(changes[self.root / "obsolete.el"])
        self.assertEqual("(first)\n(second)\n(third)\n", self.source.read_text())
        self.assertFalse((self.root / "new.el").exists())

    def test_move_into_elisp_is_validated(self):
        (self.root / "source.txt").write_text("(old)\n")
        raw = """*** Begin Patch
*** Update File: source.txt
*** Move to: destination.el
@@
-(old)
+(new)
*** End Patch"""
        output, validate = self.run_hook(self.payload("apply_patch", raw))
        self.assertEqual("", output)
        validate.assert_called_once_with("(new)\n")

    def test_invalid_syntax_denied_in_both_output_formats(self):
        for camel in (False, True):
            output, validate = self.run_hook(
                self.payload("Write", {"file_path": "example.el", "content": "("}, camel),
                valid=False)
            decision = json.loads(output)
            if not camel:
                decision = decision["hookSpecificOutput"]
            self.assertEqual("deny", decision["permissionDecision"])
            self.assertIn("Unmatched bracket", decision["permissionDecisionReason"])
            validate.assert_called_once_with("(")

    def test_every_elisp_file_checked(self):
        raw = """*** Begin Patch
*** Add File: one.el
+(valid)
*** Add File: two.el
+(
*** End Patch"""
        with patch.object(validator, "validate_elisp_syntax",
                          side_effect=[(True, None), (False, "Bad syntax")]) as validate:
            output = io.StringIO()
            with patch("sys.stdin", io.StringIO(json.dumps(
                    self.payload("apply_patch", raw)))), contextlib.redirect_stdout(output):
                validator.main()
        self.assertEqual(2, validate.call_count)
        self.assertIn("two.el", json.loads(output.getvalue())["hookSpecificOutput"]
                      ["permissionDecisionReason"])

    def test_non_elisp_and_unrelated_tools_need_no_server(self):
        for name, args in (
                ("bash", {"command": "pwd"}),
                ("Write", {"file_path": "plain.txt", "content": "("}),
                ("apply_patch", "*** Begin Patch\n*** Add File: plain.txt\n+(\n*** End Patch")):
            output, validate = self.run_hook(self.payload(name, args))
            self.assertEqual("", output)
            validate.assert_not_called()

    def test_bad_patch_and_missing_context_deny_without_traceback(self):
        for raw in ("not JSON", "*** Begin Patch\n*** Update File: example.el\n"
                    "@@\n-(missing)\n+(new)\n*** End Patch"):
            output, validate = self.run_hook(self.payload("apply_patch", raw, True))
            self.assertEqual("deny", json.loads(output)["permissionDecision"])
            validate.assert_not_called()

    def test_emacsclient_success_and_failure(self):
        for stdout, returncode, expected in (
                ('"SYNTAX_OK"\n', 0, True),
                ('"SYNTAX_ERROR: Unmatched bracket"\n', 0, False),
                ("", 1, False)):
            result = subprocess.CompletedProcess([], returncode, stdout, "")
            with patch.object(validator.subprocess, "run", return_value=result) as run:
                valid, _ = validator.validate_elisp_syntax("(test)")
            self.assertEqual(expected, valid)
            self.assertEqual("emacsclient", run.call_args.args[0][0])
            self.assertIn("with-temp-buffer", run.call_args.args[0][-1])
            self.assertNotIn("kill-emacs", run.call_args.args[0][-1])


if __name__ == "__main__":
    unittest.main()
