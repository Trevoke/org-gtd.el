# View DSL Simplification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Simplify the GTD view DSL by removing the `filters` wrapper, adding block-based multi-view structure, making types include their TODO keywords, and using semantic time filters (`:when`).

**Architecture:** The view language parser in `org-gtd-view-language.el` will detect whether a spec is single-block (flat structure) or multi-block (has `blocks` key). Type filters will be extended to include TODO keyword conditions. A new `calendar-day` block type will use native org-agenda with skip functions.

**Tech Stack:** Emacs Lisp, org-ql, org-agenda, Buttercup testing framework

---

## Task 1: Add `(when . today)` filter support

**Files:**
- Modify: `org-gtd-view-language.el:262-269` (timestamp filter function)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

Add to `test/gtd-view-language-test.el` in the timestamp filter tests section:

```elisp
(it "translates timestamp today filter"
  (let ((gtd-view-spec
         '((name . "Today Items")
           (filters . ((type . calendar)
                       (timestamp . today))))))
    (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
            :to-equal
            `(and (property "ORG_GTD" "Calendar")
                  (property-ts= ,org-gtd-timestamp ,(format-time-string "%Y-%m-%d"))))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "translates timestamp today filter"`
Expected: FAIL - unknown timestamp spec

**Step 3: Write minimal implementation**

In `org-gtd-view-language.el`, update `org-gtd-view-lang--translate-timestamp-filter`:

```elisp
(defun org-gtd-view-lang--translate-timestamp-filter (time-spec)
  "Translate timestamp TIME-SPEC to org-ql time filter."
  (cond
   ((eq time-spec 'past)
    (list `(property-ts< ,org-gtd-timestamp "today") '(not (done))))
   ((eq time-spec 'today)
    (list `(property-ts= ,org-gtd-timestamp ,(format-time-string "%Y-%m-%d"))))
   ((eq time-spec 'future)
    (list `(property-ts> ,org-gtd-timestamp "today")))
   (t (error "Unknown timestamp spec: %s" time-spec))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "translates timestamp today filter"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "feat(view-lang): add timestamp today filter"
```

---

## Task 2: Extend type filter to include TODO keywords for next-action

**Files:**
- Modify: `org-gtd-view-language.el:357-386` (type filter function)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

```elisp
(it "translates next-action type to include NEXT keyword"
  (let ((gtd-view-spec
         '((name . "Next Actions")
           (filters . ((type . next-action))))))
    (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
            :to-equal
            `(and (property "ORG_GTD" "Actions")
                  (todo ,(org-gtd-keywords--next))))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "translates next-action type to include NEXT keyword"`
Expected: FAIL - result only has property filter, no todo

**Step 3: Write minimal implementation**

In `org-gtd-view-language.el`, update `org-gtd-view-lang--translate-type-filter`:

```elisp
(defun org-gtd-view-lang--translate-type-filter (type-name)
  "Translate TYPE-NAME to org-ql property filter using org-gtd-types.
TYPE-NAME should be a symbol like \\='next-action, \\='delegated, \\='calendar, etc.
Also supports computed types:
  - \\='stuck-project - Projects with no NEXT/WAIT tasks
  - \\='active-project - Projects with at least one active task
  - \\='completed-project - Projects with all tasks done
  - \\='incubated-project - Incubated items that were projects"
  (cond
   ;; Computed project types
   ((eq type-name 'stuck-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (project-is-stuck))))
   ((eq type-name 'active-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (project-has-active-tasks))))
   ((eq type-name 'completed-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (not (project-has-active-tasks)))))
   ((eq type-name 'incubated-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-incubate)
                (property ,org-gtd-prop-previous-category ,org-gtd-projects))))
   ;; Types with implied TODO keywords
   ((eq type-name 'next-action)
    (list `(property "ORG_GTD" ,(org-gtd-type-org-gtd-value 'next-action))
          `(todo ,(org-gtd-keywords--next))))
   ((eq type-name 'delegated)
    (list `(property "ORG_GTD" ,(org-gtd-type-org-gtd-value 'delegated))
          `(todo ,(org-gtd-keywords--wait))))
   ;; Standard types from org-gtd-types
   (t
    (let ((org-gtd-val (org-gtd-type-org-gtd-value type-name)))
      (unless org-gtd-val
        (user-error "Unknown GTD type: %s" type-name))
      (list `(property "ORG_GTD" ,org-gtd-val))))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "translates next-action type to include NEXT keyword"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "feat(view-lang): next-action and delegated types include TODO keywords"
```

---

## Task 3: Remove level 2 constraint from stuck-project and completed-project

**Files:**
- Modify: `org-gtd-view-language.el:357-386` (already modified in Task 2)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

```elisp
(it "translates stuck-project without level constraint"
  (let ((gtd-view-spec
         '((name . "Stuck Projects")
           (filters . ((type . stuck-project))))))
    (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
            :to-equal
            `(and (and (property ,org-gtd-prop-category ,org-gtd-projects)
                       (project-is-stuck))))))

(it "translates completed-project without level constraint"
  (let ((gtd-view-spec
         '((name . "Completed Projects")
           (filters . ((type . completed-project))))))
    (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
            :to-equal
            `(and (and (property ,org-gtd-prop-category ,org-gtd-projects)
                       (not (project-has-active-tasks)))))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "stuck-project without level"`
Expected: FAIL - result includes (level 2)

**Step 3: Write minimal implementation**

The implementation was already done in Task 2 - the updated `org-gtd-view-lang--translate-type-filter` removes `(level 2)` from both `stuck-project` and `completed-project`.

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "stuck-project without level"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "fix(view-lang): remove level 2 constraint from project type filters"
```

---

## Task 4: Support flat filter structure (no `filters` wrapper)

**Files:**
- Modify: `org-gtd-view-language.el:400-416` (translate-to-org-ql function)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

```elisp
(it "supports flat filter structure without filters wrapper"
  (let ((gtd-view-spec
         '((name . "Stuck Projects")
           (type . stuck-project))))
    (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
            :to-equal
            `(and (and (property ,org-gtd-prop-category ,org-gtd-projects)
                       (project-is-stuck))))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "flat filter structure"`
Expected: FAIL - filters is nil, no conditions generated

**Step 3: Write minimal implementation**

Update `org-gtd-view-lang--translate-to-org-ql`:

```elisp
(defun org-gtd-view-lang--translate-to-org-ql (gtd-view-spec)
  "Translate GTD-VIEW-SPEC to an org-ql query expression.
GTD-VIEW-SPEC should be an alist with \\='name and either:
- \\='filters key containing filter alist (legacy format)
- Filter keys directly at top level (new flat format)"
  (let* ((explicit-filters (alist-get 'filters gtd-view-spec))
         ;; Extract filters: either from 'filters key or from top-level keys
         (filters (or explicit-filters
                      (seq-filter (lambda (pair)
                                    (not (memq (car pair) '(name blocks block-type prefix-format))))
                                  gtd-view-spec)))
         ;; Extract type filter for semantic property resolution
         (type-filter (seq-find (lambda (f) (eq (car f) 'type)) filters))
         (org-gtd-view-lang--current-type (when type-filter (cdr type-filter)))
         (has-future-time-filter (seq-some (lambda (filter)
                                             (and (memq (car filter) '(deadline scheduled when timestamp))
                                                  (eq (cdr filter) 'future)))
                                           filters))
         (all-conditions (apply #'append (mapcar #'org-gtd-view-lang--translate-filter filters)))
         (not-done-conditions (seq-filter (lambda (cond) (equal cond '(not (done)))) all-conditions))
         (other-conditions (seq-remove (lambda (cond) (equal cond '(not (done)))) all-conditions)))
    (if has-future-time-filter
        `(and ,@other-conditions)
      `(and ,@other-conditions ,@not-done-conditions))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "flat filter structure"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "feat(view-lang): support flat filter structure without filters wrapper"
```

---

## Task 5: Add `when` filter that uses semantic property lookup

**Files:**
- Modify: `org-gtd-view-language.el:184-225` (translate-filter function)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

```elisp
(it "translates when filter using semantic property lookup"
  (let ((gtd-view-spec
         '((name . "Delegated Due Today")
           (type . delegated)
           (when . today))))
    (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
            :to-equal
            `(and (property "ORG_GTD" "Delegated")
                  (todo ,(org-gtd-keywords--wait))
                  (property-ts= ,org-gtd-timestamp ,(format-time-string "%Y-%m-%d"))))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "when filter using semantic"`
Expected: FAIL - when not recognized as filter

**Step 3: Write minimal implementation**

Add `when` case to `org-gtd-view-lang--translate-filter`:

```elisp
((eq filter-type 'when)
 (org-gtd-view-lang--translate-when-filter filter-value))
```

Add new function:

```elisp
(defun org-gtd-view-lang--translate-when-filter (time-spec)
  "Translate when TIME-SPEC using semantic property lookup.
Requires a type filter to be present for property resolution."
  (unless org-gtd-view-lang--current-type
    (user-error "The 'when' filter requires a 'type' filter"))
  (let ((org-prop (org-gtd-type-property org-gtd-view-lang--current-type :when)))
    (unless org-prop
      (user-error "Type %s does not have a :when property" org-gtd-view-lang--current-type))
    (cond
     ((eq time-spec 'past)
      (list `(property-ts< ,org-prop "today") '(not (done))))
     ((eq time-spec 'today)
      (list `(property-ts= ,org-prop ,(format-time-string "%Y-%m-%d"))))
     ((eq time-spec 'future)
      (list `(property-ts> ,org-prop "today")))
     (t (user-error "Unknown when spec: %s" time-spec)))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "when filter using semantic"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "feat(view-lang): add 'when' filter with semantic property lookup"
```

---

## Task 6: Add block-based multi-view structure support

**Files:**
- Modify: `org-gtd-view-language.el:500-543` (org-gtd-view-show function)
- Modify: `org-gtd-view-language.el:170-182` (create-custom-commands function)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

```elisp
(it "creates agenda blocks from multi-block view spec"
  (let ((multi-block-spec
         '((name . "Multi View")
           (blocks . (((name . "Block 1")
                       (type . next-action))
                      ((name . "Block 2")
                       (type . delegated)))))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec))))
      (expect (length (caddr (car commands))) :to-equal 2))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "multi-block view spec"`
Expected: FAIL

**Step 3: Write minimal implementation**

Update `org-gtd-view-lang--create-custom-commands`:

```elisp
(defun org-gtd-view-lang--create-custom-commands (view-specs &optional key title)
  "Create org-agenda-custom-commands from VIEW-SPECS list.
KEY defaults to \"g\", TITLE defaults to \"GTD Views\"."
  (let* ((command-key (or key "g"))
         (command-title (or title "GTD Views"))
         (blocks (mapcan (lambda (view-spec)
                           (let ((blocks-list (alist-get 'blocks view-spec)))
                             (if blocks-list
                                 ;; Multi-block spec: process each block
                                 (mapcar #'org-gtd-view-lang--create-agenda-block blocks-list)
                               ;; Single-block spec: process as-is
                               (let ((main-block (org-gtd-view-lang--create-agenda-block view-spec))
                                     (additional-blocks (org-gtd-view-lang--create-additional-blocks view-spec)))
                                 (if additional-blocks
                                     (cons main-block additional-blocks)
                                   (list main-block))))))
                         view-specs)))
    `((,command-key ,command-title ,blocks))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "multi-block view spec"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "feat(view-lang): add block-based multi-view structure support"
```

---

## Task 7: Add calendar-day block type

**Files:**
- Modify: `org-gtd-view-language.el:123-138` (create-agenda-block function)
- Add: `org-gtd-view-language.el` (new skip function)
- Test: `test/gtd-view-language-test.el`

**Step 1: Write the failing test**

```elisp
(it "creates native agenda block for calendar-day block type"
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec)))
      (expect (car block) :to-equal 'agenda))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "calendar-day block type"`
Expected: FAIL - block-type not recognized

**Step 3: Write minimal implementation**

Add skip function:

```elisp
(defun org-gtd-view-lang--skip-unless-calendar-or-habit ()
  "Skip function for calendar-day block type.
Only shows items with ORG_GTD=Calendar or ORG_GTD=Habit."
  (let ((org-gtd-value (org-entry-get nil "ORG_GTD")))
    (unless (member org-gtd-value (list org-gtd-calendar org-gtd-habit))
      (org-end-of-subtree t))))
```

Update `org-gtd-view-lang--create-agenda-block`:

```elisp
(defun org-gtd-view-lang--create-agenda-block (gtd-view-spec)
  "Create an agenda block from GTD-VIEW-SPEC.
If block-type is \\='calendar-day, creates a native agenda block for today.
If view-type is \\='agenda, creates a native agenda block.
If view-type is \\='tags-grouped, creates grouped views.
Otherwise creates an org-ql agenda block."
  (let* ((name (alist-get 'name gtd-view-spec))
         (block-type (alist-get 'block-type gtd-view-spec))
         (view-type (alist-get 'view-type gtd-view-spec)))
    (cond
     ((eq block-type 'calendar-day)
      (org-gtd-view-lang--create-calendar-day-block gtd-view-spec))
     ((eq view-type 'agenda)
      (org-gtd-view-lang--create-native-agenda-block gtd-view-spec))
     ((eq view-type 'tags-grouped)
      (org-gtd-view-lang--create-grouped-views gtd-view-spec))
     (t
      (let ((query (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)))
        `(org-ql-block ',query
                       ((org-ql-block-header ,name))))))))

(defun org-gtd-view-lang--create-calendar-day-block (gtd-view-spec)
  "Create a native agenda day block filtered to Calendar and Habit items."
  (let ((name (or (alist-get 'name gtd-view-spec) "Today's Calendar")))
    `(agenda ""
             ((org-agenda-span 1)
              (org-agenda-start-day nil)
              (org-agenda-overriding-header ,name)
              (org-agenda-skip-function 'org-gtd-view-lang--skip-unless-calendar-or-habit)))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "calendar-day block type"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/gtd-view-language-test.el
git commit -m "feat(view-lang): add calendar-day block type with native agenda"
```

---

## Task 8: Update org-gtd-engage.el to use new DSL

**Files:**
- Modify: `org-gtd-engage.el:51-75`
- Test: Run existing engage tests

**Step 1: Update engage view spec**

```elisp
(defun org-gtd-engage-view-spec ()
  "Return GTD view specification for the engage view."
  (let ((project-format-prefix
         (format " %%i %%-%d:(org-gtd-agenda--prefix-format %d) "
                 org-gtd-engage-prefix-width
                 org-gtd-engage-prefix-width)))
    `((name . "GTD Engage View")
      (blocks . (((name . "Today's Calendar")
                  (block-type . calendar-day))

                 ((name . "Items to Review")
                  (type . incubated)
                  (when . today))

                 ((name . "Delegated Check-ins")
                  (type . delegated)
                  (when . today))

                 ((name . "Next Actions")
                  (type . next-action)
                  (prefix-format . ,project-format-prefix)))))))

(defun org-gtd-engage-grouped-by-context-view-spec ()
  "Return GTD view specification for the grouped by context engage view."
  `((name . "Actions by Context")
    (view-type . tags-grouped)
    (group-by . context)
    (tags-match . "{^@}")
    (type . next-action)))

(defun org-gtd-show-all-next-view-spec ()
  "Return GTD view specification for showing all next actions."
  `((name . "All Next Actions")
    (type . next-action)))
```

**Step 2: Run tests**

Run: `~/.local/bin/eldev test -B`
Expected: All tests pass

**Step 3: Commit**

```bash
git add org-gtd-engage.el
git commit -m "refactor(engage): use simplified view DSL"
```

---

## Task 9: Update org-gtd-reflect.el to use new DSL

**Files:**
- Modify: `org-gtd-reflect.el` (all view specs)

**Step 1: Update area-of-focus view specs**

```elisp
(defun org-gtd-reflect--area-of-focus-view-specs (area)
  "Create GTD view specifications for reflecting on AREA of focus."
  `(((name . "Active projects")
     (type . project)
     (area-of-focus . ,area))

    ((name . "Incubated projects")
     (type . incubated-project)
     (area-of-focus . ,area)
     (prefix-format . "  Incubated: "))

    ((name . "Next actions")
     (type . next-action)
     (area-of-focus . ,area))

    ((name . "Reminders")
     (type . calendar)
     (area-of-focus . ,area))

    ((name . "Routines")
     (type . habit)
     (area-of-focus . ,area))

    ((name . "Incubated items")
     (type . incubated)
     (when . future)
     (area-of-focus . ,area))))
```

**Step 2: Update missed-items view specs**

```elisp
(defconst org-gtd-reflect-missed-items-view-specs
  '(((name . "Missed calendar events")
     (type . calendar)
     (when . past))

    ((name . "Incubated events to review")
     (type . incubated)
     (when . past))

    ((name . "Missed delegated events")
     (type . delegated)
     (when . past)))
  "GTD view specifications for reflecting on missed items.")
```

**Step 3: Update missed-engagements view specs**

```elisp
(defconst org-gtd-reflect-missed-engagements-view-specs
  '(((name . "Missed check-ins on delegated items")
     (type . delegated)
     (when . past))

    ((name . "Missed appointments")
     (type . calendar)
     (when . past))

    ((name . "Projects that should have finished")
     (type . project)
     (deadline . past))

    ((name . "Projects that should have started")
     (type . project)
     (scheduled . past)
     (not-habit . t)))
  "GTD view specifications for missed engagement reflections.")
```

**Step 4: Update upcoming-delegated view spec**

```elisp
(defconst org-gtd-reflect-upcoming-delegated-view-spec
  '((name . "Upcoming check-ins on delegated items")
    (type . delegated)
    (when . future)
    (not-done . t))
  "GTD view specification for upcoming delegated item check-ins.")
```

**Step 5: Update stuck item view functions**

```elisp
(defun org-gtd-reflect-stuck-calendar-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Calendar Items")
     (type . calendar)
     (invalid-timestamp . t))))

(defun org-gtd-reflect-stuck-delegated-items ()
  "Agenda view with all invalid delegated actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Delegated Items")
     (type . delegated)
     (invalid-timestamp . t))))

(defun org-gtd-reflect-stuck-habit-items ()
  "Agenda view with all invalid habit actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Habit Items")
     (type . habit)
     (invalid-timestamp . t))))

(defun org-gtd-reflect-stuck-incubated-items ()
  "Agenda view with all invalid incubated actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Incubated Items")
     (type . incubated)
     (invalid-timestamp . t))))

(defun org-gtd-reflect-stuck-projects ()
  "Show all projects that do not have a next action."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Projects")
     (type . stuck-project))))

(defun org-gtd-reflect-stuck-single-action-items ()
  "Agenda view with all invalid single action items."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Single Action Items")
     (type . next-action)
     (invalid-timestamp . t))))
```

**Step 6: Update completed items view**

```elisp
(defun org-gtd-reflect-completed-items (&optional days-back)
  "Show items completed in the last DAYS-BACK days (default 7)."
  (interactive "p")
  (let* ((days (or days-back 7))
         (time-spec (cond
                     ((= days 1) 'past-day)
                     ((= days 7) 'past-week)
                     ((= days 30) 'past-month)
                     ((= days 365) 'past-year)
                     (t 'recent))))
    (org-gtd-view-show
     `((name . ,(format "Completed in Last %d Days" days))
       (done . t)
       (closed . ,time-spec)))))

(defun org-gtd-reflect-completed-projects ()
  "Show all completed projects."
  (interactive)
  (org-gtd-view-show
   '((name . "Completed Projects")
     (type . completed-project))))
```

**Step 7: Run tests**

Run: `~/.local/bin/eldev test -B`
Expected: All tests pass

**Step 8: Commit**

```bash
git add org-gtd-reflect.el
git commit -m "refactor(reflect): use simplified view DSL"
```

---

## Task 10: Update test/gtd-view-language-test.el to use new DSL

**Files:**
- Modify: `test/gtd-view-language-test.el`

**Step 1: Update existing tests to use flat structure**

Change all tests from:
```elisp
(filters . ((type . X)))
```
To:
```elisp
(type . X)
```

Also change `(timestamp . past)` to `(when . past)` where appropriate (when a type filter is present).

**Step 2: Run all tests**

Run: `~/.local/bin/eldev test -B`
Expected: All tests pass

**Step 3: Commit**

```bash
git add test/gtd-view-language-test.el
git commit -m "test(view-lang): update tests to use simplified DSL"
```

---

## Task 11: Update documentation

**Files:**
- Modify: `doc/org-gtd.org` (view language section)

**Step 1: Update view language examples**

Replace all `(filters . ((type . X)))` with `(type . X)` in documentation.

Update the engage view example to show the new block-based structure.

Add documentation for `when` filter and `calendar-day` block type.

**Step 2: Regenerate info file**

Run in Emacs: `M-x org-texinfo-export-to-info` in doc/org-gtd.org

**Step 3: Commit**

```bash
git add doc/org-gtd.org org-gtd.info
git commit -m "docs: update view DSL documentation"
```

---

Plan complete and saved to `doc/plans/2025-11-27-view-dsl-simplification-implementation.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
