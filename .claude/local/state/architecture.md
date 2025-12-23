# Flexible Task Dependencies - Architectural Design and Implementation Roadmap

## Executive Summary

This document provides a comprehensive architectural analysis and implementation roadmap for implementing flexible task dependencies in org-gtd.el. The analysis covers current state assessment, proposed architectural changes, technical design decisions, and a phased implementation strategy that respects existing org-gtd patterns while enabling sophisticated dependency management.

## Current State Analysis

### Existing Dependency Infrastructure

#### Current Implementation (`org-gtd-task-management.el`)
- **Property Model**: Uses `BLOCKED_BY` properties for simple blocking relationships
- **ID Management**: Basic ID creation via `org-gtd-id-get-create`
- **Task Selection**: Simple completing-read interface with all undone tasks
- **Relationship Scope**: Single blocker per command execution
- **Cross-File Support**: Uses org-id system for file-to-file relationships

#### Architectural Strengths
✅ **Clean Separation**: Task management isolated in dedicated module
✅ **ID System Integration**: Leverages org-gtd's existing ID generation
✅ **Cross-Buffer Support**: Works with WIP buffers and permanent files
✅ **Org-Mode Native**: Uses standard org-mode property system

#### Current Limitations
❌ **Unidirectional Properties**: Only `BLOCKED_BY` without corresponding `BLOCKS`
❌ **Single Relationship**: One blocker per command invocation
❌ **No Validation**: No circular dependency detection
❌ **Limited Management**: No removal or visualization commands
❌ **Property API Mismatch**: Not using org-mode's multivalued property functions

### Project Management Integration Analysis

#### Current Project Flow (`org-gtd-projects.el`)
```elisp
org-gtd-project-new--apply:
├── Configure main project heading
├── Configure all sub-tasks as project-task items
├── Fix TODO keywords (sequential assumption)
├── Add progress cookie
└── Refile to projects file
```

#### Org-Edna Integration Points
- **Current Triggers**: `org-gtd-next-project-action` and `org-gtd-update-project-task\!`
- **Sequential Assumption**: First non-done task becomes NEXT
- **Simple Model**: Linear task progression within projects

## Architectural Design

### Data Model for Flexible Task Relationships

#### Property Schema Evolution

**Current Schema:**
```org
* Task B
:PROPERTIES:
:BLOCKED_BY: task-a-id
:END:
```

**Proposed Bidirectional Schema:**
```org
* Task A                          * Task B
:PROPERTIES:                      :PROPERTIES:
:BLOCKS: task-b-id task-c-id      :DEPENDS_ON: task-a-id
:END:                             :END:
```

#### Property Management Strategy

**Core Decision: BLOCKS/DEPENDS_ON vs BLOCKED_BY/BLOCKS**

**Recommendation: Adopt BLOCKS/DEPENDS_ON Pattern**

**Rationale:**
1. **Semantic Clarity**: "A blocks B" and "B depends on A" express the same relationship from different perspectives
2. **GTD Alignment**: Users think "this task depends on that completion"  
3. **Org-Mode Convention**: DEPENDS_ON aligns with org-mode's dependency concepts
4. **API Consistency**: Leverages `org-entry-get-multivalued-property` functions

#### Bidirectional Consistency Model

```elisp
;; When adding: "Task B depends on Task A"
;; System ensures:
(org-entry-add-to-multivalued-property task-b-point "DEPENDS_ON" task-a-id)
(org-entry-add-to-multivalued-property task-a-point "BLOCKS" task-b-id)

;; Invariant: If A blocks B, then B depends on A
```

### ID Creation and Lifecycle Management

#### Lazy ID Creation Strategy

**Current Gap**: Sub-tasks in WIP buffers lack IDs until commit time
**Solution**: Demand-driven ID creation during relationship establishment

```elisp
(defun org-gtd-task-ensure-id-for-relationship (pom)
  "Ensure heading at POM has ID for relationship creation.
Creates org-gtd prefixed ID if none exists."
  (org-with-point-at pom
    (or (org-entry-get nil "ID")
        (let ((id (org-gtd-id--generate)))
          (org-entry-put nil "ID" id)
          ;; Update org-id location tracking for WIP buffers
          (when (org-gtd-wip-buffer-p)
            (org-id-add-location id (buffer-name)))
          id))))
```

#### ID Lifecycle Phases

1. **WIP Buffer Phase**: Temporary ID registration with buffer-local tracking
2. **Organization Phase**: ID promotion to permanent file locations
3. **Cross-Project Phase**: Full org-id system integration for persistence

### WIP Buffer vs Permanent File State Management

#### Buffer-Local Relationship Staging

**Challenge**: WIP buffer modifications must not affect permanent files until organization
**Solution**: Deferred relationship commitment with buffer-local staging

```elisp
;; WIP buffer local variables for staging relationships
(defvar-local org-gtd-wip--pending-relationships nil
  "Alist of (task-id . ((blocks . (id1 id2)) (depends-on . (id3 id4))))
   for relationships to commit during organization.")

(defun org-gtd-wip-stage-relationship (task-id relationship-type target-id)
  "Stage a relationship for commit during organization."
  (let ((task-entry (assoc task-id org-gtd-wip--pending-relationships)))
    (if task-entry
        (let ((rel-entry (assoc relationship-type (cdr task-entry))))
          (if rel-entry
              (setcdr rel-entry (cons target-id (cdr rel-entry)))
            (setcdr task-entry (cons (list relationship-type target-id) (cdr task-entry)))))
      (setq org-gtd-wip--pending-relationships
            (cons (list task-id (list relationship-type target-id))
                  org-gtd-wip--pending-relationships)))))
```

#### Commit-Time Integration

**Integration Point**: `org-gtd-project-new--apply`
**Enhancement Strategy**: Extend project organization to commit staged relationships

```elisp
(defun org-gtd-project--commit-staged-relationships ()
  "Commit all staged relationships during project organization."
  (dolist (task-rel org-gtd-wip--pending-relationships)
    (let ((task-id (car task-rel))
          (relationships (cdr task-rel)))
      (org-gtd-task--apply-relationship-changes task-id relationships))))
```

### Org-Edna Integration for Flexible Projects

#### Current Edna Usage Analysis

```elisp
;; Current Projects Template
:TRIGGER: org-gtd-next-project-action org-gtd-update-project-task\!
```

#### Enhanced Edna Integration Strategy

**Replace Sequential Logic with Dependency-Aware Logic**

```elisp
(defun org-gtd-projects--edna-next-available-tasks ()
  "Find tasks whose dependencies are all completed."
  (let ((available-tasks '()))
    (org-map-entries
     (lambda ()
       (when (org-gtd-task--dependencies-satisfied-p)
         (push (point-marker) available-tasks)))
     t 'tree)
    available-tasks))

(defalias 'org-edna-finder/org-gtd-next-available-tasks
  'org-gtd-projects--edna-next-available-tasks)
```

#### Dependency Resolution Algorithm

```elisp
(defun org-gtd-task--dependencies-satisfied-p ()
  "Check if all dependencies for current task are completed."
  (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
    (if depends-on
        (cl-every (lambda (dep-id)
                   (let ((dep-marker (org-id-find dep-id t)))
                     (when dep-marker
                       (with-current-buffer (marker-buffer dep-marker)
                         (goto-char dep-marker)
                         (org-entry-is-done-p)))))
                 depends-on)
      ;; No dependencies = ready to go (unless blocked by project structure)
      t)))
```

## Technical Design Decisions

### Emacs Lisp Best Practices Integration

#### Function Naming Conventions
**Pattern**: `org-gtd-task-{operation}-{object}s`
- `org-gtd-task-add-blockers` (plural for multi-select)
- `org-gtd-task-remove-blockers`
- `org-gtd-task-show-relationships`

#### Error Handling Strategy
**Approach**: Early validation with user-friendly error recovery

```elisp
(defun org-gtd-task-add-blockers ()
  "Add blocking tasks with comprehensive validation."
  (interactive)
  (org-gtd-task--validate-context)
  (condition-case err
      (org-gtd-task--execute-add-blockers)
    (org-gtd-circular-dependency
     (org-gtd-task--handle-circular-dependency err))
    (error
     (org-gtd-task--handle-generic-error err))))
```

#### Multi-Value Property Management
**Decision**: Use org-mode's native multivalued property functions
**Rationale**: Consistency with org-mode conventions, proper escaping, standard serialization

```elisp
;; Preferred approach
(org-entry-add-to-multivalued-property (point) "BLOCKS" target-id)
(org-entry-get-multivalued-property (point) "DEPENDS_ON")

;; Avoid manual string manipulation
;; (org-entry-put (point) "BLOCKS" (concat current-blocks " " new-id))
```

### Org-Mode Convention Alignment

#### Property Naming Standards
- **BLOCKS**: Space-separated list of task IDs that this task enables
- **DEPENDS_ON**: Space-separated list of task IDs that must complete first
- **Escaping**: Rely on org-mode's multivalued property handling for special characters

#### Agenda Integration
**Design**: Ensure new dependency properties integrate seamlessly with org-agenda

```elisp
(defun org-gtd-agenda--dependency-skip-function ()
  "Skip tasks that have unsatisfied dependencies."
  (when (and (not (org-entry-is-done-p))
             (not (org-gtd-task--dependencies-satisfied-p)))
    (or (outline-next-heading) (point-max))))
```

## Validation and Error Handling Architecture

### Circular Dependency Detection

#### Graph-Based Validation Algorithm

```elisp
(defun org-gtd-task--detect-circular-dependency (from-id to-id)
  "Detect if adding dependency FROM-ID -> TO-ID creates cycle."
  (let ((visited (make-hash-table :test 'equal))
        (rec-stack (make-hash-table :test 'equal)))
    (org-gtd-task--dfs-cycle-check to-id from-id visited rec-stack)))

(defun org-gtd-task--dfs-cycle-check (current target visited rec-stack)
  "DFS-based cycle detection in dependency graph."
  (puthash current t visited)
  (puthash current t rec-stack)
  
  ;; Check all tasks that current blocks
  (let ((blocks (org-gtd-task--get-blocks-for-id current)))
    (dolist (blocked-id blocks)
      (cond
       ((string= blocked-id target) t) ; Found cycle
       ((not (gethash blocked-id visited))
        (when (org-gtd-task--dfs-cycle-check blocked-id target visited rec-stack)
          (cl-return t)))
       ((gethash blocked-id rec-stack) t)))) ; Back edge found
  
  (puthash current nil rec-stack)
  nil)
```

### Relationship Consistency Validation

#### Bidirectional Integrity Checks

```elisp
(defun org-gtd-task--validate-relationship-integrity ()
  "Validate all BLOCKS/DEPENDS_ON relationships are bidirectional."
  (let ((inconsistencies '()))
    (org-map-entries
     (lambda ()
       (let ((task-id (org-entry-get (point) "ID")))
         (when task-id
           (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
             (dolist (blocked-id blocks)
               (unless (org-gtd-task--has-dependency blocked-id task-id)
                 (push (list :missing-dependency task-id blocked-id) inconsistencies)))))))
     t 'agenda)
    inconsistencies))
```

## Cross-Project Dependency Resolution

### Multi-File Relationship Management

#### Challenge: Maintaining consistency across org-gtd files
**Solution**: Centralized relationship update with file-local validation

```elisp
(defun org-gtd-task--update-cross-file-relationship (task-id property value)
  "Update relationship property handling cross-file scenarios."
  (let ((marker (org-id-find task-id t)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-entry-add-to-multivalued-property (point) property value)
          ;; Save buffer if it's a file-backed buffer
          (when (buffer-file-name)
            (save-buffer)))
      (user-error "Cannot find task with ID: %s" task-id))))
```

#### Agenda File Scope Management
**Design**: Limit relationship discovery to org-gtd controlled files

```elisp
(defun org-gtd-task--collect-relationship-candidates ()
  "Collect tasks available for relationship creation."
  (let ((candidates '()))
    ;; Current buffer tasks (WIP or permanent)
    (setq candidates (append candidates (org-gtd-task--collect-buffer-tasks)))
    ;; GTD agenda file tasks  
    (setq candidates (append candidates (org-gtd-task--collect-agenda-tasks)))
    candidates))
```

## Implementation Roadmap

### Phase 1: Core Dependency Foundation (Sprint 1)

#### Priority 1.1: Enhanced Property Management
**Duration**: 3-5 days
**Deliverables**:
- Replace `BLOCKED_BY` with `BLOCKS`/`DEPENDS_ON` schema
- Implement bidirectional relationship functions
- Add lazy ID creation for WIP buffers

**Key Files Modified**:
- `org-gtd-task-management.el`: Core relationship functions
- New: `org-gtd-task-relationships.el`: Relationship management utilities

```elisp
;; New function signatures
(org-gtd-task-add-blockers)           ; Multi-select interface
(org-gtd-task-add-dependents)         ; Reverse relationship creation  
(org-gtd-task-remove-blockers)        ; Relationship removal
```

#### Priority 1.2: Multi-Select User Interface  
**Duration**: 2-3 days
**Deliverables**:
- Enhanced completing-read with multi-select capability
- Task filtering and project context display
- Confirmation workflow for relationship changes

#### Priority 1.3: Circular Dependency Prevention
**Duration**: 3-4 days  
**Deliverables**:
- Graph-based cycle detection algorithm
- User-friendly error messages with cycle explanation
- Recovery suggestions for blocked operations

### Phase 2: Project Integration (Sprint 2)

#### Priority 2.1: Default Sequential Dependencies
**Duration**: 2-3 days
**Integration Point**: `org-gtd-project-new--apply`
**Logic**: Create sequential chain for tasks without explicit relationships

```elisp
(defun org-gtd-project--apply-default-sequential-dependencies ()
  "Create default sequential dependencies for unlinked tasks."
  (let ((tasks (org-gtd-project--collect-unlinked-tasks)))
    (cl-loop for (current next) on tasks
             when next do
             (org-gtd-task--create-dependency current next))))
```

#### Priority 2.2: Custom Dependencies Override
**Duration**: 2-3 days
**Enhancement**: Preserve user-defined relationships during organization

#### Priority 2.3: Org-Edna Integration Update
**Duration**: 3-4 days
**Deliverables**:
- Replace sequential project logic with dependency-aware logic
- New edna finders: `org-gtd-next-available-tasks`
- Enhanced project task state management

### Phase 3: Advanced Management Features (Sprint 3)

#### Priority 3.1: Relationship Visualization
**Duration**: 4-5 days
**Deliverables**:
- `org-gtd-task-show-relationships` command
- WIP buffer helper window (if `org-gtd-clarify-display-helper-buffer` enabled)
- Cross-project dependency labels

#### Priority 3.2: Direct Task Management 
**Duration**: 2-3 days
**Integration**: Enable relationship management from agenda and org files
**Challenge**: Immediate file commitment vs WIP buffer staging

#### Priority 3.3: System Validation and Health
**Duration**: 3-4 days
**Deliverables**:
- `org-gtd-validate-project-dependencies` health check
- Broken dependency detection and repair
- Orphaned task identification

## Risk Assessment and Mitigation

### Technical Risks

#### Risk: Breaking Existing Projects
**Mitigation**: 
- Implement backward compatibility layer for `BLOCKED_BY` properties
- Provide migration utility: `org-gtd-migrate-task-dependencies`
- Extensive regression testing with existing test fixtures

#### Risk: Performance Impact on Large Projects
**Mitigation**:
- Implement caching for dependency resolution
- Limit relationship traversal depth
- Profile performance with 50+ task projects

#### Risk: Complex User Interface
**Mitigation**: 
- Progressive disclosure: simple commands for basic use cases
- Comprehensive documentation with examples
- Interactive tutorial for advanced features

### User Experience Risks

#### Risk: Overwhelming Feature Complexity
**Mitigation**:
- Default to sequential dependencies (current behavior)
- Advanced features opt-in via customization variables
- Clear documentation of when to use advanced features

#### Risk: Broken User Workflows
**Mitigation**:
- Maintain all existing command interfaces
- Graceful degradation when features not configured
- Comprehensive upgrade guide

## Success Metrics

### Functional Metrics
- ✅ All 15 user stories implemented with acceptance criteria met
- ✅ Zero regression in existing org-gtd workflows  
- ✅ <2 second response time for relationship operations
- ✅ Circular dependency detection catches 100% of cycles

### User Experience Metrics  
- ✅ Reduced setup time for complex projects (baseline: current manual property editing)
- ✅ Maintained simplicity for users who don't need advanced features
- ✅ Clear error messages guide users to resolution
- ✅ Visual feedback shows relationship changes before commit

## Architectural Principles Applied

### Language Idioms & Paradigms (Emacs Lisp)
✅ **Function Composition**: Small, focused functions that compose cleanly  
✅ **Buffer-Local Variables**: Proper use of `defvar-local` for WIP buffer state
✅ **Condition System**: Structured error handling with condition-case
✅ **Interactive Design**: All user-facing functions properly marked interactive
✅ **Hook Integration**: Leverage existing org-gtd hook system for extensibility

### Domain-Driven Design (GTD Methodology)  
✅ **Ubiquitous Language**: "blocks", "depends on", "project", "task" align with GTD  
✅ **Bounded Context**: Task relationships contained within org-gtd's project scope
✅ **Domain Model**: Task dependencies reflect real-world project constraints
✅ **Business Logic Separation**: Dependency logic separate from UI and persistence

### Software Craftsmanship
✅ **Single Responsibility**: Each function has one clear purpose
✅ **Open/Closed**: New relationship types can be added without modifying core logic
✅ **Interface Segregation**: Separate interfaces for basic and advanced dependency management
✅ **Dependency Inversion**: Depend on org-mode abstractions, not implementation details

## Conclusion

This architecture provides a comprehensive foundation for implementing flexible task dependencies in org-gtd while respecting the existing codebase patterns and maintaining the simplicity that makes org-gtd effective. The phased implementation approach ensures that users can adopt advanced features gradually while maintaining full backward compatibility.

The design prioritizes user domain knowledge (expressing real dependencies) over system complexity, while providing the technical infrastructure needed for sophisticated project management when required.
EOF < /dev/null
