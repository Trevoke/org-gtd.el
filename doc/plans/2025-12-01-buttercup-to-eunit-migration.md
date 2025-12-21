# Buttercup to E-Unit Migration Plan

**Goal**: Complete migration of all buttercup tests to e-unit, then remove buttercup dependency.

**Current Status**: 413 e-unit tests passing, 329 buttercup specs passing (19 files remaining)

## Remaining Files Analysis

| File | Lines | Spies | Sim-Input | Complexity |
|------|-------|-------|-----------|------------|
| autoload-test.el | 15 | 0 | 0 | Special (subprocess) |
| core-test.el | 49 | 0 | 0 | Simple (deprecated API) |
| agenda-property-loop-test.el | 149 | 0 | 0 | Simple |
| org-gtd-agenda-transient-test.el | 205 | 0 | 0 | Simple |
| org-gtd-graph-remove-task-test.el | 294 | 0 | 0 | Simple |
| id-overlay-test.el | 240 | 0 | 1 | Medium |
| extend-completed-project-test.el | 241 | 0 | 1 | Medium |
| org-gtd-graph-view-test.el | 340 | 0 | 1 | Medium |
| organizing-test.el | 196 | 3 | 1 | Medium (spies) |
| task-removal-test.el | 316 | 1 | 0 | Medium (spy) |
| reactivate-test.el | 230 | 7 | 0 | Medium (spies) |
| areas-of-focus.el | 204 | 0 | 6 | Complex (input issues) |
| configure-test.el | 229 | 0 | 10 | Complex (heavy input) |
| upgrades-test.el | 664 | 0 | 1 | Medium-Large |
| gtd-view-language-test.el | 938 | 0 | 0 | Large |
| end-to-end-test.el | 1093 | 13 | 0 | Large (many spies) |
| project-test.el | 1306 | 0 | 3 | Large |
| task-management-commands-test.el | 1118 | 22 | 0 | Large (many spies) |
| org-gtd-graph-transient-test.el | 1209 | 7 | 16 | Complex (spies + input) |

## Migration Phases

### Phase 1: Simple Files (No spies, no/minimal input simulation)
**Estimated: ~5 files, straightforward migration**

1. `agenda-property-loop-test.el` (149 lines)
2. `org-gtd-agenda-transient-test.el` (205 lines)
3. `org-gtd-graph-remove-task-test.el` (294 lines)
4. `gtd-view-language-test.el` (938 lines) - large but simple patterns

### Phase 2: Medium Files with Minimal Input Simulation
**Estimated: ~4 files, need careful with-simulated-input handling**

1. `id-overlay-test.el` (240 lines, 1 sim-input)
2. `extend-completed-project-test.el` (241 lines, 1 sim-input)
3. `org-gtd-graph-view-test.el` (340 lines, 1 sim-input)
4. `upgrades-test.el` (664 lines, 1 sim-input)

### Phase 3: Files Using spy-on (Requires e-unit-mock)
**Estimated: ~3 files, use with-spy macro**

1. `task-removal-test.el` (316 lines, 1 spy)
2. `organizing-test.el` (196 lines, 3 spies)
3. `reactivate-test.el` (230 lines, 7 spies)

### Phase 4: Large Files
**Estimated: ~2 files, time-consuming but straightforward**

1. `project-test.el` (1306 lines, 3 sim-input)
2. `gtd-view-language-test.el` (938 lines) - if not done in Phase 1

### Phase 5: Complex Files (Heavy spies and/or input simulation)
**Estimated: ~4 files, may need special handling**

1. `areas-of-focus.el` (204 lines, 6 sim-input) - had issues, needs investigation
2. `configure-test.el` (229 lines, 10 sim-input)
3. `end-to-end-test.el` (1093 lines, 13 spies)
4. `task-management-commands-test.el` (1118 lines, 22 spies)
5. `org-gtd-graph-transient-test.el` (1209 lines, 7 spies, 16 sim-input)

### Phase 6: Special Cases
**Handle separately**

1. `autoload-test.el` - runs subprocess, may need different approach
2. `core-test.el` - tests deprecated `with-org-gtd-context`, evaluate if still needed

## E-Unit Mock Patterns Reference

### Buttercup spy-on → e-unit-mock with-spy
```elisp
;; Buttercup
(spy-on 'some-function :and-call-through)
(expect 'some-function :to-have-been-called)

;; E-unit
(with-spy some-function
  (do-something)
  (assert-true (spy-called-p 'some-function)))
```

### Buttercup spy-on with return → e-unit-mock with-stub
```elisp
;; Buttercup
(spy-on 'some-function :and-return-value 42)

;; E-unit
(with-stub some-function 42
  (assert-equal 42 (some-function)))
```

### Buttercup :to-have-been-called-with → e-unit-mock spy-called-with-p
```elisp
;; Buttercup
(expect 'save-some-buffers :to-have-been-called-with t #'org-gtd-buffer-p)

;; E-unit
(with-spy save-some-buffers
  (do-something)
  (assert-true (spy-called-with-p 'save-some-buffers t #'org-gtd-buffer-p)))
```

## Final Cleanup

After all tests migrated:

1. Remove buttercup from Eldev dependencies
2. Remove test/helpers/setup.el (buttercup-specific)
3. Remove test/helpers/prelude.el (buttercup-specific)
4. Delete empty test/ directory
5. Update CLAUDE.md testing commands
6. Update CI configuration if applicable

## Known Issues

1. **with-simulated-input timing**: Some tests timeout waiting for input. May need to ensure variables are set with `setq` not `let` before simulated input runs.

2. **areas-of-focus tests**: Failed migration attempt due to input simulation issues. Need to investigate `execute-kbd-macro` and `with-simulated-input` behavior differences between buttercup and e-unit contexts.
