# User Type Customization and Documentation Update

## Overview

Add `org-gtd-user-types` for user customization of built-in GTD types, and update documentation to reflect the new type system.

## Design Decisions

- Users can override properties of existing types, not add new types
- Properties merge by semantic name (`:who`, `:when`, etc.)
- New `:input-fn` attribute allows custom input functions
- `:org-gtd` values cannot be overridden (data integrity)
- Unknown type names in `org-gtd-user-types` are silently ignored

## Implementation Tasks

### Task 1: Add org-gtd-user-types defcustom

File: `org-gtd-types.el`

Add after `org-gtd-types` defconst:

```elisp
(defcustom org-gtd-user-types '()
  "User customizations for built-in GTD types.

This alist allows overriding properties of existing types defined in
`org-gtd-types'. You cannot add new types, only customize existing ones.

Each entry is (TYPE-NAME . PLIST) where TYPE-NAME must be one of the
built-in types: next-action, delegated, calendar, incubated, project,
habit, reference, trash, quick-action.

PLIST can contain:
- :properties - List of property definitions to merge/override
- :state - Override the TODO state semantic (rarely needed)

Property definitions support these attributes:
- :org-property - The org-mode property name (string)
- :type - Input type: text, timestamp, repeating-timestamp
- :required - Whether property is required (t or nil)
- :prompt - Prompt string for interactive input
- :default - Default value (skips prompting)
- :input-fn - Custom function for input (receives prompt, returns value)

Example: Use EBDB contacts for delegation:

  (setq org-gtd-user-types
        \\='((delegated
           :properties
           ((:who :org-property \"DELEGATED_TO\" :type text :required t
                  :prompt \"Delegate to\"
                  :input-fn my/ebdb-completing-read)))))"
  :group 'org-gtd
  :type '(alist :key-type symbol :value-type plist))
```

### Task 2: Modify org-gtd-type-get to merge user overrides

File: `org-gtd-types.el`

Update `org-gtd-type-get` to:
1. Look up built-in type
2. Look up user override (if any)
3. Merge properties by semantic name
4. Return merged result

```elisp
(defun org-gtd-type-get (type-name)
  "Get type definition for TYPE-NAME with user overrides merged.
Returns the full type entry (TYPE-NAME . PLIST) or nil if not found."
  (when-let ((builtin (assq type-name org-gtd-types)))
    (let ((user-override (assq type-name org-gtd-user-types)))
      (if user-override
          (org-gtd--merge-type-definitions builtin user-override)
        builtin))))
```

### Task 3: Add property merge helper

File: `org-gtd-types.el`

```elisp
(defun org-gtd--merge-type-definitions (builtin user)
  "Merge USER type definition into BUILTIN.
Properties are merged by semantic name. User properties override builtin.
:org-gtd is never overridden from user config."
  (let* ((builtin-plist (cdr builtin))
         (user-plist (cdr user))
         (type-name (car builtin))
         ;; Never allow overriding :org-gtd
         (org-gtd-val (plist-get builtin-plist :org-gtd))
         ;; Allow overriding :state
         (state (or (plist-get user-plist :state)
                    (plist-get builtin-plist :state)))
         ;; Merge properties
         (builtin-props (plist-get builtin-plist :properties))
         (user-props (plist-get user-plist :properties))
         (merged-props (org-gtd--merge-properties builtin-props user-props)))
    (cons type-name
          (list :org-gtd org-gtd-val
                :state state
                :properties merged-props))))

(defun org-gtd--merge-properties (builtin-props user-props)
  "Merge USER-PROPS into BUILTIN-PROPS by semantic name.
User properties with same semantic name replace builtin ones."
  (if (null user-props)
      builtin-props
    (let ((result (copy-sequence builtin-props)))
      (dolist (user-prop user-props)
        (let* ((semantic-name (car user-prop))
               (existing (seq-find (lambda (p) (eq (car p) semantic-name)) result)))
          (if existing
              ;; Replace existing property
              (setq result (mapcar (lambda (p)
                                     (if (eq (car p) semantic-name)
                                         user-prop
                                       p))
                                   result))
            ;; Add new property
            (push user-prop result))))
      result)))
```

### Task 4: Update prompt function to use :input-fn

File: `org-gtd-configure.el`

Modify `org-gtd--prompt-for-property-type` or the calling code in `org-gtd-configure-as-type` to check for `:input-fn`:

```elisp
;; In org-gtd-configure-as-type, change the value lookup:
(value (or (alist-get semantic-name values)
           default-val
           (if-let ((input-fn (plist-get (cdr prop) :input-fn)))
               (funcall input-fn prompt)
             (org-gtd--prompt-for-property-type prop-type prompt))))
```

### Task 5: Update documentation

File: `doc/org-gtd.org`

Replace lines 3598-3651 (the `org-gtd-user-item-config` section) with new `org-gtd-user-types` documentation.

#### New content:

```org
*** Advanced Item Configuration

**** ~org-gtd-user-types~

*Type*: Alist

*Default*: ~'()~

*Description*: Customize property prompts and input methods for built-in GTD types.

This alist allows you to override how org-gtd prompts for properties when organizing items. You can customize existing types but cannot add new ones.

Each entry is ~(TYPE-NAME . PLIST)~ where TYPE-NAME must be one of: ~next-action~, ~delegated~, ~calendar~, ~incubated~, ~project~, ~habit~, ~reference~, ~trash~, ~quick-action~.

*Property attributes you can set:*

| Attribute     | Description                                      |
|---------------+--------------------------------------------------|
| ~:prompt~     | The prompt string shown to the user              |
| ~:input-fn~   | Custom function for input (overrides default)    |
| ~:default~    | Default value (skips prompting if set)           |
| ~:type~       | Input type: ~text~, ~timestamp~, ~repeating-timestamp~ |
| ~:required~   | Whether the property is required (~t~ or ~nil~)  |
| ~:org-property~ | The org-mode property name (string)            |

*Example: Integrating Contact Managers with Delegation*

BBDB and EBDB are Emacs contact management packages. You can configure org-gtd to use completion from your contacts database when delegating tasks.

For EBDB integration:
#+begin_src emacs-lisp
;; Define function to complete from EBDB contacts
(defun my/ebdb-completing-read (prompt)
  "Read a contact name from EBDB with completion."
  (completing-read prompt
                   (mapcar (lambda (rec) (ebdb-record-name-string rec))
                           (ebdb-records))
                   nil t))

;; Configure delegation to use EBDB completion
(setq org-gtd-user-types
      '((delegated
         :properties
         ((:who :org-property "DELEGATED_TO" :type text :required t
                :prompt "Delegate to"
                :input-fn my/ebdb-completing-read)))))
#+end_src

For BBDB integration, use ~bbdb-records~ and ~bbdb-record-name~ instead:
#+begin_src emacs-lisp
(defun my/bbdb-completing-read (prompt)
  "Read a contact name from BBDB with completion."
  (completing-read prompt
                   (mapcar #'bbdb-record-name (bbdb-records))
                   nil t))
#+end_src

This pattern works for any completion source (org-contacts, custom databases, etc.).

*Example: Custom prompt text*

Change the default prompts without changing input behavior:
#+begin_src emacs-lisp
(setq org-gtd-user-types
      '((delegated
         :properties
         ((:who :org-property "DELEGATED_TO" :type text :required t
                :prompt "Assign to team member")
          (:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
                 :prompt "Follow-up date")))))
#+end_src
```

### Task 6: Write tests

File: `test/types-test.el`

Add tests for:
1. `org-gtd-type-get` returns builtin when no user override
2. `org-gtd-type-get` merges user properties correctly
3. User property replaces builtin property by semantic name
4. User can add new properties to a type
5. `:org-gtd` is never overridden from user config
6. Unknown type names in user-types are ignored
7. `:input-fn` is called when present (in configure-test.el)

## Test Commands

```bash
# Run all tests
eldev test -B

# Run specific test file
eldev test -B --file="test/types-test.el"
eldev test -B --file="test/configure-test.el"

# Run specific test
eldev test -B "user types"
```

## Verification

1. All existing tests pass
2. New tests pass
3. EBDB example from docs works when EBDB is installed
4. Compilation succeeds without new warnings
