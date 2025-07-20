Feature: Org Heading Dependency Tracking
  As an org-gtd user
  I want hierarchical dependency tracking between org headings
  So that I can manage complex project workflows with task blocking relationships

  Background:
    Given I have an org-gtd file with multiple headings
    And the org-edna trigger system is enabled

  Scenario: Automatic ID assignment to org headings
    Given I create a new org heading "Setup development environment"
    When the system processes the heading
    Then the heading receives a unique ID property
    And the ID contains a date-based root component
    And the ID contains a human-friendly slug derived from the heading text
    And the ID format follows "YYYY-###-kebab-case-description"

  Scenario: Adding dependency properties to org headings
    Given I have an org heading with ID "2024-001-setup-dev"
    When I add dependency tracking properties
    Then the heading includes a "BLOCKS" property for child task IDs
    And the heading includes a "BLOCKED_BY" property for parent task IDs
    And both properties can store lists of task IDs

  Scenario: Establishing blocking relationships between tasks
    Given I have a task "2024-001-requirements" that must complete first
    And I have a task "2024-002-development" that depends on requirements
    When I establish the blocking relationship
    Then "2024-001-requirements" has "2024-002-development" in its BLOCKS list
    And "2024-002-development" has "2024-001-requirements" in its BLOCKED_BY list
    And the relationship is bidirectionally consistent

  Scenario: Automatic state transition when dependencies are resolved
    Given I have a task "2024-001-requirements" with status NEXT
    And I have a task "2024-002-development" with status TODO
    And "2024-001-requirements" blocks "2024-002-development"
    When I mark "2024-001-requirements" as DONE
    Then the org-edna trigger fires
    And the trigger finds "2024-002-development" in the BLOCKS list
    And the trigger removes "2024-001-requirements" from "2024-002-development" BLOCKED_BY list
    And "2024-002-development" status changes to NEXT because it has no remaining blockers

  Scenario: Multiple dependencies resolution
    Given I have task "2024-001-backend" blocking "2024-003-integration"
    And I have task "2024-002-frontend" also blocking "2024-003-integration"
    And "2024-003-integration" has status TODO
    When I complete "2024-001-backend"
    Then "2024-003-integration" remains TODO because "2024-002-frontend" still blocks it
    When I complete "2024-002-frontend"
    Then "2024-003-integration" changes to NEXT because all blockers are resolved

  Scenario: User interface for defining dependencies
    Given I am editing an org heading
    When I want to specify that this task blocks another task
    Then I can easily add the target task ID to the BLOCKS property
    And the system automatically updates the target task's BLOCKED_BY property
    And I receive confirmation that the relationship was established

  Scenario: User interface for visualizing dependencies
    Given I have multiple tasks with blocking relationships
    When I view a task with dependencies
    Then I can clearly see which tasks this task blocks
    And I can clearly see which tasks block this task
    And the dependency status is visually indicated

  Scenario: Preventing circular dependencies
    Given I have task "2024-001-A" that blocks "2024-002-B"
    And task "2024-002-B" blocks "2024-003-C"
    When I try to make "2024-003-C" block "2024-001-A"
    Then the system prevents the circular dependency
    And I receive a clear error message about the cycle
    And no properties are modified

  Scenario: Handling orphaned task references
    Given I have task "2024-001-parent" blocking "2024-002-child"
    And "2024-002-child" has "2024-001-parent" in its BLOCKED_BY list
    When I delete task "2024-001-parent"
    Then "2024-002-child" automatically removes the orphaned ID from BLOCKED_BY
    And "2024-002-child" transitions to NEXT if no other blockers remain

  Scenario: ID uniqueness across the org system
    Given I have multiple org files in my org-gtd system
    When the system generates IDs for new headings
    Then each ID is unique across all files
    And no collisions occur between different headings
    And the uniqueness is maintained during file operations

  Scenario: Data consistency during dependency modification
    Given I have established blocking relationships between multiple tasks
    When I modify a dependency relationship
    Then all affected tasks maintain consistent BLOCKS and BLOCKED_BY properties
    And no orphaned or incorrect references are created
    And the bidirectional relationship integrity is preserved

  Scenario: Performance with large dependency networks
    Given I have a complex project with 50+ interdependent tasks
    When the org-edna trigger processes task completions
    Then dependency resolution completes within acceptable time
    And org file operations remain responsive
    And memory usage stays within reasonable limits

  Scenario: Migration from existing org-gtd setup
    Given I have an existing org-gtd workflow without dependencies
    When I enable the dependency tracking feature
    Then existing tasks continue to work as before
    And I can gradually add dependency relationships
    And no existing functionality is broken