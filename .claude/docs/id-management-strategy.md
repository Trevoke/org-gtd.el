# ID Management Strategy for Clarify Buffer Workflow

## Current Clarify Workflow Analysis

### Buffer Management
- **WIP Buffer**: Temporary org-mode buffer (`org-gtd-wip-mode`) without file backing
- **Content Copy**: Original heading copied from source to WIP buffer via `org-copy-subtree`/`org-paste-subtree`
- **ID Preservation**: Source heading gets ID via `org-gtd-id-get-create` before copy
- **Clean Slate**: WIP buffer strips certain properties but preserves ID

### Key Insight
The clarify workflow already ensures the **main project heading has an ID** before entering the WIP buffer.

## ID Management Strategy

### For Main Project Heading
✅ **Already Handled**: `org-gtd-clarify-item` calls `org-gtd-id-get-create` on source heading

### For Sub-tasks Created During Clarification

#### Current Workflow Gap
When users manually add sub-headings in WIP buffer:
1. User adds `** First task`
2. User adds `** Second task`
3. **Problem**: Sub-tasks have NO IDs until relationship creation

#### Proposed Solutions

##### Option 1: Lazy ID Creation (Recommended)
```elisp
(defun org-gtd-task-ensure-id-before-relationship (pom)
  "Ensure heading at POM has an ID, creating if necessary."
  (unless (org-entry-get pom "ID")
    (org-entry-put pom "ID" (org-id-uuid))))
```

**Advantages:**
- Minimal intrusion - IDs only created when needed
- Preserves user workflow - no automatic ID generation
- Works with existing task selection functions

##### Option 2: Hook-Based Auto ID (Alternative)
```elisp
(defun org-gtd-wip-auto-id-hook ()
  "Add ID to newly created headings in WIP buffers."
  (when (and (derived-mode-p 'org-gtd-wip-mode)
             (not (org-entry-get (point) "ID")))
    (org-entry-put (point) "ID" (org-id-uuid))))

(add-hook 'org-insert-heading-hook #'org-gtd-wip-auto-id-hook)
```

**Disadvantages:**
- More invasive - all new headings get IDs
- May clutter buffer with unnecessary IDs

### Bidirectional Relationship Management

#### Enhanced Task Management Functions
```elisp
(defun org-gtd-task-add-blocker-enhanced ()
  "Add blocker with automatic ID management."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))

  ;; Ensure current task has ID
  (org-gtd-task-ensure-id-before-relationship (point))

  (let* ((current-id (org-entry-get (point) "ID"))
         (current-heading (nth 4 (org-heading-components)))
         (selected-id (org-gtd-task-management--select-task-id
                       (format "Select task that blocks '%s': " current-heading))))
    (when selected-id
      ;; Ensure target task has ID
      (org-gtd-task-ensure-id-before-relationship
       (org-gtd-task-management--find-task-position selected-id))

      ;; Add bidirectional relationship
      (org-entry-add-to-multivalued-property (point) "BLOCKED_BY" selected-id)
      (org-gtd-task-management--add-to-other-task-property selected-id "BLOCKS" current-id)

      (message "Added blocker relationship: %s blocks %s"
               (org-gtd-task-management--get-heading-for-id selected-id)
               current-heading))))
```

#### Key Enhancements
1. **Automatic ID Creation**: Both source and target tasks get IDs as needed
2. **Bidirectional Updates**: BLOCKED_BY and BLOCKS properties stay in sync
3. **Multi-value Support**: Use org-mode's built-in multi-value functions

### Integration Points

#### During Project Organization
```elisp
(defun org-gtd-project-new--apply-enhanced ()
  "Enhanced project creation with relationship support."
  ;; Main project setup (existing logic)
  (org-gtd-configure-item (point) :project-heading)

  ;; Enhanced sub-task configuration
  (org-map-entries
   (lambda ()
     ;; Ensure each sub-task has an ID for potential relationships
     (org-gtd-task-ensure-id-before-relationship (point))
     (org-gtd-configure-item (point) :project-task))
   "LEVEL=2"
   'tree)

  ;; Rest of existing logic...
  )
```

## Implementation Priority

### Phase 1: Core ID Management
1. Implement `org-gtd-task-ensure-id-before-relationship`
2. Update existing relationship functions to use lazy ID creation
3. Add bidirectional relationship support

### Phase 2: Enhanced UX
1. Improve task selection with better completing-read interface
2. Add relationship visualization in clarify buffer
3. Implement relationship removal functions

### Phase 3: Advanced Features
1. Relationship validation (detect cycles)
2. Visual dependency indicators
3. Auto-complete for task relationships

## Benefits

### User Experience
- **Seamless Workflow**: No manual ID management required
- **Natural Progression**: From simple project to complex relationships
- **Fail-Safe**: System handles edge cases automatically

### Technical Benefits
- **Consistent Data**: Bidirectional relationships stay in sync
- **Performance**: Lazy ID creation minimizes overhead
- **Extensibility**: Foundation for advanced dependency features
