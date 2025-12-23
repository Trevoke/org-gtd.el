# Flexible Task Dependencies - User Stories for org-gtd

## User Personas and Goals

### Primary Persona: GTD Practitioner
- **Role**: Individual using org-gtd for personal productivity
- **Context**: Single-user system with existing GTD workflow
- **Goals**: 
  - Express real-world task dependencies accurately
  - Maintain project momentum with clear next actions
  - Evolve project understanding over time
  - Minimize manual overhead while maximizing control

### Secondary Persona: Complex Project Manager
- **Role**: Individual managing multi-faceted projects with interdependencies
- **Context**: Projects with 10+ tasks and cross-project dependencies
- **Goals**:
  - Model realistic project workflows
  - Track progress across dependent task chains
  - Visualize project structure and bottlenecks
  - Maintain system integrity as projects evolve

## Epic: Core Dependency Management

### Story 1: Add Task Blockers (Single Selection)
**Priority: HIGH**
**Complexity: MEDIUM**

As a GTD practitioner, I want to specify that one task blocks another so that I can model sequential dependencies in my projects.

**Acceptance Criteria:**
```gherkin
Given I have a task in a WIP buffer or existing project
When I position the cursor on the task
And I run "org-gtd-task-add-blockers"
Then I see a selection interface showing tasks from current buffer and existing agenda files
And when I select a blocking task
Then the system creates proper BLOCKS/DEPENDS_ON properties between the tasks
And the blocking task gets the selected task's ID in its BLOCKS property
And the current task gets the blocking task's ID in its DEPENDS_ON property
```

### Story 2: Add Task Blockers (Multiple Selection)
**Priority: HIGH**
**Complexity: MEDIUM**

As a GTD practitioner, I want to specify multiple tasks that block another task so that I can efficiently express complex dependencies like "Install decking depends on both permits AND materials".

**Acceptance Criteria:**
```gherkin
Given I have a task in a WIP buffer or existing project
When I position the cursor on the task
And I run "org-gtd-task-add-blockers"
Then I see a selection interface with multi-select capability
And when I select multiple blocking tasks
And I complete the selection (by selecting nothing or pressing a completion key)
Then all selected tasks get the current task's ID in their BLOCKS property
And the current task gets all selected task IDs in its DEPENDS_ON property
```

### Story 3: Remove Task Blockers
**Priority: MEDIUM**
**Complexity: LOW**

As a GTD practitioner, I want to remove blocking relationships so that I can adapt when project requirements change.

**Acceptance Criteria:**
```gherkin
Given I have a task with existing blocking relationships
When I position the cursor on the task
And I run "org-gtd-task-remove-blockers"
Then I see a selection interface showing only current blockers
And when I select blockers to remove
Then the system removes the corresponding BLOCKS/DEPENDS_ON properties
And both tasks are updated correctly
```

### Story 4: Add Task Dependents
**Priority: MEDIUM**
**Complexity: MEDIUM**

As a GTD practitioner, I want to specify what tasks depend on the current task so that I can express relationships from either direction.

**Acceptance Criteria:**
```gherkin
Given I have a task in a WIP buffer or existing project
When I position the cursor on the task
And I run "org-gtd-task-add-dependents"
Then I see a selection interface showing available tasks
And when I select dependent tasks
Then the current task gets the selected task IDs in its BLOCKS property
And the selected tasks get the current task's ID in their DEPENDS_ON property
```

## Epic: Cross-Project Dependencies

### Story 5: Cross-Project Blocking Relationships
**Priority: HIGH**
**Complexity: MEDIUM**

As a GTD practitioner, I want to create dependencies between tasks in different projects so that I can model realistic cross-project workflows like "deck building depends on HOA approval".

**Acceptance Criteria:**
```gherkin
Given I have a task in a WIP buffer or existing project
When I run "org-gtd-task-add-blockers"
Then I see tasks from all existing agenda files clearly labeled by project
And when I select a task from another project
Then the system creates proper cross-project BLOCKS/DEPENDS_ON properties
And both tasks are updated with correct IDs regardless of their file location
```

### Story 6: Direct Task Dependency Management
**Priority: MEDIUM**
**Complexity: MEDIUM**

As a GTD practitioner, I want to add dependencies directly from any task in my agenda or project files so that I don't need to use the clarify workflow for simple changes.

**Acceptance Criteria:**
```gherkin
Given I have a task in an agenda view or org file
When I position the cursor on the task
And I run "org-gtd-task-add-blockers"
Then I see the same selection interface as in WIP buffers
And when I select blocking tasks
Then the relationships are immediately committed to the files
And I see a confirmation message about the dependency update
```

## Epic: Project Organization Integration

### Story 7: Default Sequential Dependencies
**Priority: HIGH**
**Complexity: MEDIUM**

As a GTD practitioner, I want new projects to automatically have sequential dependencies so that I get sensible defaults without manual setup.

**Acceptance Criteria:**
```gherkin
Given I have a project with multiple tasks in a WIP buffer
When I organize it as a project
And I haven't specified any custom dependencies
Then the system creates a sequential chain where each task depends on the previous one
And only the first task is marked as NEXT
And each task has proper BLOCKS/DEPENDS_ON properties for the chain
```

### Story 8: Custom Dependencies Override Defaults
**Priority: HIGH**
**Complexity: MEDIUM**

As a GTD practitioner, I want my explicitly set dependencies to take precedence over default sequential ordering so that I maintain full control over project structure.

**Acceptance Criteria:**
```gherkin
Given I have a project with multiple tasks in a WIP buffer
And I have set custom dependencies using org-gtd-task-add-blockers
When I organize it as a project
Then the system preserves my custom dependencies
And only creates default sequential links for tasks without explicit relationships
And tasks with multiple dependencies are handled correctly
```

### Story 9: Lazy ID Creation
**Priority: MEDIUM**
**Complexity: LOW**

As a system user, I want IDs to be created automatically when needed so that I don't have to manage them manually.

**Acceptance Criteria:**
```gherkin
Given I have tasks in a WIP buffer without IDs
When I try to add blockers or dependents
Then the system automatically creates IDs for tasks that need them
And the IDs are properly formatted and unique
And the selection interface shows tasks with their newly created IDs
```

## Epic: Dependency Visualization and Feedback

### Story 10: Show Task Relationships
**Priority: MEDIUM**
**Complexity: HIGH**

As a GTD practitioner, I want to see current task relationships so that I can understand my project structure and verify my dependencies are correct.

**Acceptance Criteria:**
```gherkin
Given I have a project with dependencies
When I run "org-gtd-task-show-relationships" on a task
Then I see a clear display of what the task blocks and what blocks it
And cross-project dependencies are clearly labeled with project names
And the display is formatted for easy reading
```

### Story 11: Project Dependencies Helper Window
**Priority: LOW**
**Complexity: HIGH**

As a GTD practitioner, I want to see a live view of project dependencies while working in a WIP buffer so that I can make informed decisions about task relationships.

**Acceptance Criteria:**
```gherkin
Given I have a WIP buffer with multiple tasks
And org-gtd-clarify-display-helper-buffer is enabled
When the buffer has more than one task heading
Then a read-only side window opens automatically
And it shows "Project name: [first level heading]"
And each task shows as "(depends_on, ...) -> task -> (blocks, ...)"
And tasks without explicit relationships show default sequential dependencies
And an "orphaned tasks" section shows tasks without any relationships
```

### Story 12: Clear All Task Relationships
**Priority: LOW**
**Complexity: LOW**

As a GTD practitioner, I want to reset all relationships for a task so that I can start over when dependencies become complex or incorrect.

**Acceptance Criteria:**
```gherkin
Given I have a task with multiple dependencies
When I run "org-gtd-task-clear-relationships" on the task
Then all BLOCKS and DEPENDS_ON properties are removed from the task
And all related tasks have their corresponding properties updated
And I see a confirmation of the changes made
```

## Epic: System Integrity and Validation

### Story 13: Circular Dependency Detection
**Priority: HIGH**
**Complexity: HIGH**

As a GTD practitioner, I want the system to prevent circular dependencies so that my projects remain logically sound and actionable.

**Acceptance Criteria:**
```gherkin
Given I'm trying to create a dependency relationship
When the relationship would create a circular dependency
Then the system prevents the relationship creation
And shows me a clear error message explaining the circular path
And suggests how to resolve the conflict
```

### Story 14: Broken Project Detection
**Priority: MEDIUM**
**Complexity: MEDIUM**

As a GTD practitioner, I want to identify and fix broken dependencies so that my system remains reliable over time.

**Acceptance Criteria:**
```gherkin
Given I have projects with potential dependency issues
When I run a project health check
Then the system identifies projects with non-existent task references
And shows projects with archived task dependencies
And identifies orphaned tasks that should be part of projects
And provides guidance on fixing each type of issue
```

### Story 15: Automatic Next Action Updates
**Priority: HIGH**
**Complexity: HIGH**

As a GTD practitioner, I want tasks to automatically become available when their dependencies are completed so that I maintain project momentum without manual intervention.

**Acceptance Criteria:**
```gherkin
Given I have tasks with dependency relationships
When I mark a task as DONE
Then all tasks that were blocked by this task automatically become NEXT
And tasks that still have other incomplete dependencies remain waiting
And the org-agenda reflects these changes immediately
```

## Story Prioritization Summary

### Sprint 1 (Core Foundation - Highest Value)
1. **Story 1**: Add Task Blockers (Single) - Core functionality
2. **Story 2**: Add Task Blockers (Multiple) - Essential UX improvement
3. **Story 5**: Cross-Project Dependencies - Major user need
4. **Story 7**: Default Sequential Dependencies - Reduces setup overhead
5. **Story 13**: Circular Dependency Detection - System integrity

### Sprint 2 (Enhanced Management)
6. **Story 8**: Custom Dependencies Override - Maintains user control
7. **Story 15**: Automatic Next Actions - Core GTD workflow integration
8. **Story 3**: Remove Task Blockers - Basic relationship management
9. **Story 6**: Direct Task Management - Workflow efficiency

### Sprint 3 (Complete Management Interface)
10. **Story 4**: Add Task Dependents - Bidirectional relationship creation
11. **Story 9**: Lazy ID Creation - Technical foundation
12. **Story 10**: Show Task Relationships - Visibility and verification
13. **Story 14**: Broken Project Detection - System maintenance

### Future Iterations
14. **Story 11**: Project Dependencies Helper - Advanced visualization
15. **Story 12**: Clear All Relationships - Edge case management

## Technical Implementation Notes

### Architecture Constraints Respected:
- Single user system with no race conditions
- Bounded scope to WIP buffers + existing agenda files
- Commit-time relationships during organization step
- ID management via org-id functions

### Key Technical Requirements:
- Use `org-entry-get-multivalued-property` and related functions for property management
- Implement multi-select interfaces for efficient relationship creation
- Integrate with existing org-edna trigger system for automatic updates
- Maintain compatibility with current org-gtd workflow patterns

### Success Metrics:
- Reduced manual effort for complex project setup
- Maintained domain knowledge focus (users think about real dependencies)
- Backward compatibility with existing org-gtd projects
- Clear error handling and user guidance for edge cases

## Assumptions Requiring Stakeholder Validation:
1. Users prefer batch selection over repeated single selections
2. Default sequential ordering is acceptable for undefined relationships
3. Cross-project dependencies are a common enough use case to prioritize
4. Visual feedback is less important than functional completeness for initial release