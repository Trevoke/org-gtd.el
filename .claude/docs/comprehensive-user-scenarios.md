# Language

- a task "blocks" other tasks (we say A blocks B when A has to be complete before B can be started)
- a task "depends on" other tasks (we say C depends on B when B has to be complete before C can be started)

So a basic user interface would be `org-gtd-task-add-blockers` and `org-gtd-task-add-dependents`
And the org-mode properties would be `:BLOCKS:` and `:DEPENDS_ON:`

# User Scenarios: Flexible Task Dependencies (Constrained Architecture)

## Architecture Constraints
- **Single user system**: No interaction between WIP buffers, no race conditions
- **Bounded scope**: Relationships only between current WIP buffer + existing agenda files
- **Commit-time relationships**: Dependencies expressed during clarify step become real during organization step
- **ID location updates**: System handles ID management via org-id update functions

## User Scenarios Analysis

### Scenario 1: Simple Internal Project Dependencies

**User Journey:**
1. Clarify "Build a deck" item from inbox
2. Add sub-tasks in WIP buffer:
   ```org
   * Build a deck
   ** Get permits
   ** Buy materials
   ** Build foundation
   ** Install decking
   ```
3. Realize "Install decking" needs both permits and materials complete
4. **User action**: Position cursor on "Install decking"
5. **User action**: `M-x org-gtd-task-add-blockers`
6. **System shows**: Tasks from current buffer + existing projects
7. **User selects**: "Get permits"
8. **System shows**: Tasks from current buffer + existing projects
9. **User selects**: "Buy materials"
9. **System shows**: Tasks from current buffer + existing projects
10. **User selects**: end without selecting anything
11. **User action**: `M-x org-gtd-organize` → "This is a project"

**Result**:
- "Get Permits" does not depend on anything : it's a NEXT task
- "Buy materials" depends on "Get permits"
- "Build Foundation" depends on "Buy materials"
- "Install decking" depends on "Buy materials" and "Get permits"

Concretely in code:
- "Get Permits" doesn't have `:DEPENDS_ON:` but it has `:BLOCKS:` with the id of "Buy Materials"
- "Buy Materials" has `:DEPENDS_ON:` with id of "Get Permits" and `:BLOCKS:` with id of "Build Foundation" and id of "Install Decking"
- etc.

**Manual Effort Analysis:**
- **System setup effort**: 1 command to express 2 logical relationship (good)
- **Domain knowledge effort**: User expresses actual dependency needs (good)
- **Total actions**: 5 user actions to set up one "depends on multiple" relationship

**UX Problems:**
❌ No visual feedback showing current relationships while building them

### Scenario 2: Mixed Internal/External Dependencies

**User Journey:**
1. Clarify "Build a deck" item from inbox
2. Add internal sub-tasks
3. Realize "Get permits" depends on existing task "Get HOA approval" from "Neighborhood relations" project
4. **User action**: Position on "Get permits"
5. **User action**: `M-x org-gtd-task-add-blockers`
6. **System shows**: Current WIP tasks + all tasks from existing agenda files
7. **User selects**: "Get HOA approval [Neighborhood relations project]"
8. **System shows**: Current WIP tasks + all tasks from existing agenda files
9. **User selects**: end without selecting anything
10. **User action**: `M-x org-gtd-organize` → "This is a project"

**Manual Effort Analysis:**
- **System setup effort**: 1 command per external dependency (acceptable)
- **Domain knowledge effort**: User identifies real cross-project dependency (good)
- **Cognitive load**: User must identify which project external task belongs to

**UX Benefits:**
✅ Cross-project dependencies work naturally
✅ Single command for single relationship (reasonable)

### Scenario 3: Complex Project (10+ Tasks, Multiple Dependencies)

**User Journey:**
1. Clarify "Launch product" item from inbox with 10 tasks
2. User wants to set up realistic dependency web:
   - "Frontend dev" blocked by: "API design", "UI mockups"
   - "Backend dev" blocked by: "API design", "Database schema"
   - "Testing" blocked by: "Frontend dev", "Backend dev"
   - "Launch" blocked by: "Testing", "Marketing materials"

**Manual Effort Calculation:**
- 4 tasks with dependencies
- 8 total blocking relationships
- **Required actions**: 4 × `org-gtd-task-add-blockers` calls = 4 command
- **User time**: ~2-3 minutes of relationship setup

**Manual Effort Analysis:**
- **System setup effort**: 4 commands for 8 relationships (acceptable ratio)
- **Domain knowledge effort**: User thinks through actual project flow (good)
- **Scalability**: Linear relationship between complexity and effort

### Scenario 4: Relationship Evolution (Existing Project Changes)

**User Journey:**
1. `org-gtd-clarify-item` on existing "Website redesign" project
2. Project loads into WIP buffer with existing structure
3. User wants to add: "Deploy" depends on "Legal review" from "Compliance" project
4. **User action**: Position on "Deploy"
5. **User action**: `M-x org-gtd-task-add-blockers`
6. **System shows**: Current WIP tasks + external agenda tasks
7. **User selects**: "Legal review [Compliance project]"
8. **System shows**: Current WIP tasks + external agenda tasks
9. **User selects**: end without selecting anything
10. **User action**: `M-x org-gtd-organize` → project -> relationships updated

**Manual Effort Analysis:**
- **System setup effort**: 1 command to add 1 relationship (good)
- **Domain knowledge effort**: User recognizes new dependency need (good)
- **Change management**: Easy to evolve relationships over time

### Scenario 4b: Relationship Evolution (Existing Project changes, point on task)
**User Journey:**
1. `org-gtd-task-add-blockers` on "Deploy" (either on org-agenda or in org-mode file directly)
2. **System shows**: Current WIP tasks + external agenda tasks
3. **User selects**: "Legal review [Compliance project]"
4. **System shows**: Current WIP tasks + external agenda tasks
5. **User selects**: end without selecting anything
6. **System shows**: return to state prior to selecting the new blockers, message in minibuffer that dependencies have been updated

Impact: "Deploy" and "Legal review" both have their dependencies/blockers updated

### Scenario 5: Dependency Removal/Modification

Similar to Scenario 4 and 4b, this can happen as a "clarify project" flow or as a sub-flow directly from task heading, on agenda or in org-mode file

**User Journey:**
1. Clarify existing project
2. User realizes "Task A" no longer needs to wait for "Task B"
3. **User action**: `M-x org-gtd-task-remove-blockers`
4. **System shows**: All current blockers for Task A
5. **User selects**: "Task B"
6. **System shows**: All current blockers for Task A
7. **User selects**: end without selecting anything

**Manual Effort Analysis:**
- **Domain knowledge effort**: User knows dependency is no longer needed (good)
- **Missing feature**: Need `org-gtd-task-remove-blockers` command

## Overall User Experience Assessment

### What Users Do for Domain (Good)
1. **Express actual dependencies**: Users think through what really depends on what
2. **Identify cross-project connections**: Users recognize broader project relationships
3. **Evolve project understanding**: Users can add dependencies as understanding grows

### User Efficiency Metrics

#### Acceptable Scenarios:
- **1:1 relationships**: 1 command per dependency (reasonable)
- **Cross-project deps**: Finding external tasks works well
- **Project evolution**: Easy to add dependencies to existing projects

#### Problem Areas:
- **1:many relationships**: "A depends on B, C, D" requires 3 separate commands
- **Relationship removal**: No friendly command, requires property editing
- **Visual feedback**: No way to see current relationships while building them

## Recommended UX Improvements

### Priority 1: Reduce Command Overhead

`org-gtd-task-add-blocker` should become `org-gtd-task-add-blockers`; I think in emacs there may be a multi-input command that ends when the user enters nothing?

### Priority 2: Relationship Management Commands
- `org-gtd-task-add-dependents`: Friendly add interface
- `org-gtd-task-remove-blockers`: Friendly removal interface
- `org-gtd-task-remove-dependents`: Friendly removal interface
- `org-gtd-task-show-relationships`: Display current dependencies
- `org-gtd-task-clear-relationships`: Reset all relationships

### Priority 3: Visual Feedback
- Show current relationships
  - If there are more than one org heading in the WIP buffer AND `org-gtd-clarify-display-helper-buffer` is `t` then:
  - open a read-only side window
  - First line is "Project name : " followed by text in level 1 org heading
  - Each following line is for each level 2 heading and looks like this: `(depends_on, ...) -> task -> (blocks, ...)`
  - If there are no `:BLOCKS` or `:DEPENDS_ON:` then assume the prior level 2 org heading is the `:DEPENDS_ON:` and the next level 2 heading is the `:BLOCKS:`
  - If there are, then use those
  - Have an "orphaned tasks" section a few lines below for tasks (level 2 headings) that don't have at least one `:BLOCKS:` or one `:DEPENDS_ON:`
- Highlight related tasks when cursor on task with dependencies
- Preview relationship changes before commit

### Priority 4: further complexity
- We need a WIP-buffer-only command to specify tasks beyond the first level 2 heading that do not have parents (i.e. they are all tasks that are unblocked at the beginning of the project). These tasks wouldn't get a `:DEPENDS_ON:` property but may/would get the `:BLOCKS:` property


## Implementation details

Lazily create IDs for tasks in the WIP buffer; create IDs for the org-headings that don't have any when the user tries to add a blocker or a dependency, so that they can be selected as such
- obviously if the user doens't do that then create IDs when organizing and creating the default tree as described below

In the clarify buffer, assume sequentiality of tasks. This means that:
- If the user doesn't specify any relationships, then each task will create a straightforward doubly linked list from the first level 2 heading to the last one
- If the user has specified some relationships, they'll be added to the above
- If the user has fully specified relationships (if each task has `:BLOCKS:` and `:DEPENDS_ON:` except for the "starting tasks" (the first parent(s) ) then we don't override
- use `org-entry-get-multivalued-property`, `org-entry-put-multivalued-property`, and `org-entry-add-to-multivalued-property` to modify the `:BLOCKS:` and `:DEPENDS_ON:` properties
- Reuse the org-edna trigger names for project task auto-update but change the implementations so it looks for the next tasks from the one that just got marked as DONE, following the `:BLOCKS:` multivalue property, and sets all of these to the org-gtd-next keyword

Committing the relationships at organize time means that if any blockers or dependencies are selected by the user, whether to tasks internal or external to the project, this information is saved in a variable local to the WIP buffer and reused to actually make the changes when `org-gtd-organize` is called and `project` is selected.

## Validation needs
- Detect circular dependencies (A -> B -> A)
  - In WIP buffers and WIP buffers only, this requires verifying the implicit ordering described in the Implementation detail; it would be OK to fail the organization step and bring the user back to the WIP buffer, explaining what the circular dependency is
  - In regular task modification, this happens when trying to commit the change
- Detect broken projects and show the information in a buffer:
  - projects with non-existent tasks
  - projects with existing but archived tasks
  - tasks with non-existent parents (orphaned tasks, maybe supposed to be part of a project but unreachable)
