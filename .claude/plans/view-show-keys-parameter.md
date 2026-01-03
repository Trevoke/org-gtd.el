# Detailed Implementation Plan: Add KEYS Parameter to org-gtd-view-show

## Overview
Add an optional KEYS parameter to `org-gtd-view-show` that gets passed to `org-agenda`, enabling users with `org-agenda-sticky` to open multiple independent agenda views.

---

## Task 1: Update org-gtd-view-show Function Signature
**File:** `org-gtd-view-language.el`

### Current Code (lines 1202-1257):
```elisp
(defun org-gtd-view-show (view-spec-or-specs)
  "Display an org-gtd agenda view from VIEW-SPEC-OR-SPECS.
  ..."
  (interactive)
  (let* ((view-specs ...)
         (expanded-specs ...)
         (title ...)
         (org-agenda-custom-commands
          (org-gtd-view-lang--create-custom-commands
           expanded-specs
           "g"      ;; <-- hardcoded key
           title)))
    (org-agenda nil "g")  ;; <-- hardcoded key
    (goto-char (point-min))))
```

### Changes:
1. Add `&optional keys` parameter to function signature
2. Default to "g" when keys is nil
3. Use the key variable in both places
4. Update docstring to document the new parameter

### New Code:
```elisp
(defun org-gtd-view-show (view-spec-or-specs &optional keys)
  "Display an org-gtd agenda view from VIEW-SPEC-OR-SPECS.

VIEW-SPEC-OR-SPECS can be either:
- A single view specification alist
- A list of view specification alists (shown as multiple blocks)

KEYS is an optional string used as the agenda dispatch key.
Defaults to \"g\".  When `org-agenda-sticky' is non-nil, using
different KEYS values allows multiple independent agenda views
to be displayed simultaneously, each in its own buffer named
*Org Agenda(KEYS)*.

Example with multiple views:

  (setq org-agenda-sticky t)
  (org-gtd-view-show \\='((name . \"Actions\") (type . next-action)) \"a\")
  (org-gtd-view-show \\='((name . \"Calendar\") (type . calendar)) \"c\")
  ;; Creates buffers *Org Agenda(a)* and *Org Agenda(c)*

..."
  (interactive)
  (let* ((key (or keys "g"))
         (view-specs (if (and (listp view-spec-or-specs)
                              (listp (car view-spec-or-specs))
                              (symbolp (caar view-spec-or-specs)))
                         (list view-spec-or-specs)
                       view-spec-or-specs))
         (expanded-specs (mapcar #'org-gtd-view-lang--expand-implicit-blocks view-specs))
         (title (alist-get 'name (car expanded-specs)))
         (org-agenda-custom-commands
          (org-gtd-view-lang--create-custom-commands
           expanded-specs
           key
           title)))
    (org-agenda nil key)
    (goto-char (point-min))))
```

### Verification:
```bash
~/bin/eldev compile org-gtd-view-language.el
```

---

## Task 2: Update Existing Test to Pass
**File:** `test/integration/view-filters-test.el`

### Current Test (fails):
```elisp
(deftest view-filters/two-view-show-calls-create-two-buffers ()
  "..."
  (org-gtd-view-show '((name . "First View") (type . next-action)))
  (let ((first-buffer (current-buffer)))
    (org-gtd-view-show '((name . "Second View") (type . calendar)))
    (let ((second-buffer (current-buffer)))
      (assert-not-equal first-buffer second-buffer)
      (assert-true (buffer-live-p first-buffer))
      (assert-true (buffer-live-p second-buffer)))))
```

### Updated Test:
```elisp
(deftest view-filters/two-view-show-calls-create-two-buffers ()
  "Calling org-gtd-view-show twice with different KEYS creates separate buffers.
Requires org-agenda-sticky to be non-nil."
  (let ((org-agenda-sticky t))
    (org-gtd-view-show '((name . "First View") (type . next-action)) "a")
    (let ((first-buffer (current-buffer)))
      (org-gtd-view-show '((name . "Second View") (type . calendar)) "b")
      (let ((second-buffer (current-buffer)))
        ;; With sticky mode and different keys, we get separate buffers
        (assert-not-equal first-buffer second-buffer)
        (assert-true (buffer-live-p first-buffer))
        (assert-true (buffer-live-p second-buffer))))))
```

### Verification:
```bash
~/bin/eldev etest
```

---

## Task 3: Add Test for Default Key Behavior
**File:** `test/integration/view-filters-test.el`

### New Test:
```elisp
(deftest view-filters/view-show-uses-default-key-when-not-specified ()
  "org-gtd-view-show uses 'g' as default key when KEYS not provided."
  (org-gtd-view-show '((name . "Default Key View") (type . next-action)))
  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    ;; Buffer name should be *Org Agenda* (not *Org Agenda(g)*) when not sticky
    (assert-equal "*Org Agenda*" (buffer-name agenda-buffer))))
```

---

## Task 4: Add Test for Same Key Reuses Buffer
**File:** `test/integration/view-filters-test.el`

### New Test:
```elisp
(deftest view-filters/same-key-reuses-buffer-with-sticky ()
  "Using same KEYS value reuses the same buffer even with org-agenda-sticky."
  (let ((org-agenda-sticky t))
    (org-gtd-view-show '((name . "View 1") (type . next-action)) "x")
    (let ((first-buffer (current-buffer)))
      (org-gtd-view-show '((name . "View 2") (type . calendar)) "x")
      (let ((second-buffer (current-buffer)))
        ;; Same key = same buffer (content replaced)
        (assert-equal first-buffer second-buffer)))))
```

---

## Task 5: Update Documentation
**File:** `doc/org-gtd.org`

### Find section documenting org-gtd-view-show and add:

```org
**** Using Multiple Concurrent Views

By default, calling ~org-gtd-view-show~ multiple times replaces the
previous agenda buffer.  To display multiple views simultaneously:

1. Enable sticky agenda buffers:
   #+begin_src emacs-lisp
   (setq org-agenda-sticky t)
   #+end_src

2. Pass different KEYS to each call:
   #+begin_src emacs-lisp
   (defun my-dashboard ()
     "Show my GTD dashboard with multiple views."
     (interactive)
     (delete-other-windows)
     (org-gtd-view-show '((name . "Next Actions") (type . next-action)) "a")
     (split-window-right)
     (other-window 1)
     (org-gtd-view-show '((name . "Calendar") (type . calendar)) "c"))
   #+end_src

Each view will appear in its own buffer: ~*Org Agenda(a)*~, ~*Org Agenda(c)*~, etc.
```

---

## Summary

| Task | File | Type |
|------|------|------|
| 1 | org-gtd-view-language.el | Modify function |
| 2 | test/integration/view-filters-test.el | Fix failing test |
| 3 | test/integration/view-filters-test.el | Add test |
| 4 | test/integration/view-filters-test.el | Add test |
| 5 | doc/org-gtd.org | Update docs |

## Dependencies

```
Task 1 ──┬── Task 2
         ├── Task 3
         └── Task 4
Task 5 (independent)
```

Tasks 2, 3, 4 depend on Task 1.
Task 5 is independent (documentation).
