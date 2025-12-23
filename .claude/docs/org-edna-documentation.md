# Org Edna Documentation

## Overview
Edna provides "an extensible means of specifying conditions which must be fulfilled before a task can be completed and actions to take once it is".

Org Edna stands for "Extensible Dependencies 'N' Actions (EDNA) for Org Mode tasks" and "runs when either the BLOCKER or TRIGGER properties are set on a heading, and when it is changing from a TODO state to a DONE state".

## Installation
```elisp
;; Only necessary if installing from source
(add-to-list 'load-path "/full/path/to/org-edna/")
(require 'org-edna)
;; Always necessary  
(org-edna-mode)
```

## Extending Org Edna

### Principles
- Edna searches for functions following specific naming conventions
- Three main extension types: Finders, Actions, and Conditions

### Naming Conventions
- Conditions end with '?'
- Actions end with '!'
- Avoid special characters at the end of finder names

### Creating Custom Finders
```elisp
(defun org-edna-finder/test-finder ()
  (list (point-marker)))
```
- Must return a list of markers or nil
- Represents targets for triggers or blockers

### Creating Custom Actions
```elisp
(defun org-edna-action/test-action! (last-entry arg1 arg2)
  ;; Implementation details
)
```
- First argument is always `last-entry` (marker for current entry)
- Additional arguments based on your specific action needs

### Creating Custom Conditions
```elisp
(defun org-edna-condition/test-condition? (neg)
  (let ((condition (my-test-for-condition)))
    (when (org-xor condition neg)
      (string-for-blocking-entry-here))))
```
- Argument `neg` handles condition negation
- Return a string if the entry should be blocked

## Key Features

### Setting TODO states
- NEW-STATE may either be a string or a symbol denoting the new TODO state
- Can also be the empty string, in which case the TODO state is removed

### Archive functionality
- Archives all targets with confirmation
- Controlled by "org-edna-prompt-for-archive"

### Property copying
- Copies PROPERTY from the source entry to all targets
- Does nothing if the source heading has no property PROPERTY

## Example Use Case
```org
* TODO My Task
  :PROPERTIES:
  :TRIGGER: if self has-property?("REPEAT" "2") 
            then self set-property!("REPEAT" inc) 
            todo!("TODO") 
            endif
  :REPEAT: 0
  :END:
```

## Extension Guidelines
- Follow naming conventions
- Document new keywords
- Verify compatibility
- Test thoroughly