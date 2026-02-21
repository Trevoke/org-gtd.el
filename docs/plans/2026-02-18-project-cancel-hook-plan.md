# Project Cancel Hook Implementation Plan

**Design:** `2026-02-18-project-cancel-hook-design.md`
**Base SHA:** `4787ef5`

## Task 1: Guard variable and org-gtd-project-cancel with confirmation

**Goal**: Add the guard variable and rewrite `org-gtd-project-cancel` to prompt for confirmation, set heading to CNCL, and cancel children.

**Test**: Write tests for:
- Confirmed cancel: heading CNCL + children CNCL
- Declined cancel: no changes
- Already CNCL heading: still prompts, still cancels children
- No incomplete children: no error

**Implementation**:
- Add `defvar org-gtd-project--cancel-in-progress`
- Rewrite `org-gtd-project-cancel` with `yes-or-no-p`, guard, heading state change, child cancellation
- Use `unwind-protect` for guard cleanup

**Files**: `org-gtd-projects.el`, `test/unit/project-cancel-confirm-test.el`

## Task 2: Hook function org-gtd-project--maybe-cancel-from-hook

**Goal**: Create hook function that detects manual CNCL on project headings and either confirms+cancels or reverts.

**Test**: Write tests for:
- Manual CNCL confirmed → children cancelled
- Manual CNCL declined → heading reverts
- Guard prevents re-prompt on revert
- Guard prevents re-prompt on programmatic cancel
- Non-project heading → no action

**Implementation**:
- Add `org-gtd-project--maybe-cancel-from-hook` following `org-gtd-single-action--maybe-convert-to-delegated` pattern
- Register in `org-gtd--enable-org-gtd-mode` / `org-gtd--disable-org-gtd-mode`

**Files**: `org-gtd-projects.el`, `org-gtd-mode.el`, `test/unit/project-cancel-hook-test.el`

## Task 3: Simplify cancel-from-context

**Goal**: Remove redundant confirmation from `org-gtd-project-cancel-from-context`.

**Test**: Verify existing tests pass with single confirmation prompt.

**Implementation**: Remove `yes-or-no-p` wrapper, delegate directly to `org-gtd-project-cancel`.

**Files**: `org-gtd-projects.el`, existing integration tests
