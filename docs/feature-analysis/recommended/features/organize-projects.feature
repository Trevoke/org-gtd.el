Feature: Organize - Projects
  Multi-step outcomes finishable within a year, indexed on one list,
  each always carrying a next action.

  @REC-PRJ-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: A multi-step outcome becomes a project on the index
    Given the user clarifies an item requiring more than one action step, finishable within about a year
    When they organize it as a project
    Then it appears as one line on the single Projects list
    And the Projects list remains an index — no plans, no priority ordering embedded
    And an outcome likely to take more than a year is parked at the Goals horizon instead

  @REC-PRJ-02 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: Every active project carries a next action
    Given an active project exists
    When its last next action is completed or removed
    Then the project becomes detectable as stuck
    And review (and optionally the dashboard flags) surfaces it for the user to define a new next action
    And each independently movable portion of a project keeps its own next action

  @REC-PRJ-03 @type-tool @strength-should @src-B1
  Scenario: Blocked subprojects are not flagged as stuck
    Given a project's support material holds a subproject waiting on another piece to finish
    When stuck-project detection runs
    Then that dependency-blocked subproject is not flagged
    But the top-level project itself still requires at least one current kick-start action at review

  @REC-PRJ-04 @type-tool @strength-should @src-B1
  Scenario Outline: Subproject representation is the user's choice
    Given a project with several components
    When the user chooses to represent components as <representation>
    Then the tool supports it without forcing the alternative
    And <action_rule>

    Examples:
      | representation                          | action_rule                                                        |
      | detail under one Projects-list entry    | parallel components each carry their own next action               |
      | separate entries on the Projects list   | a sequential chain exposes only the single linchpin action (edna)  |

  @REC-PRJ-05 @type-tool @strength-must @src-B1 @src-B2
  Scenario: Support material stays separate from reminders
    Given a project has accumulated plans, notes, links, and collateral
    When the user files them
    Then they attach to the project but never appear on action lists or the Projects index
    And support material never serves as the reminder of the project
    And active-project material is kept more accessible than pure reference

  @REC-PRJ-06 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario: Natural Planning supports a project that needs more thinking
    Given a project is still on the user's mind after outcome and next action were set
    When they invoke project planning
    Then the tool walks purpose/principles, vision of the successful outcome, brainstorm, organize, next actions
    And the project record can persist that thinking as a schema (purpose, outcome scenario, components, actions, waiting-fors, maybes, resources)
    And roughly 80% of projects are expected to skip this entirely

  @REC-PRJ-07 @type-tool @strength-should @src-B1 @src-B2
  Scenario: A planning step is a valid next action
    Given a project needs more clarity before real-world motion
    When the user defines its next action
    Then "draft ideas", "email X for input", or "set up planning session" are accepted as next actions
    And a Project Planning Trigger List is available to drive the brainstorm

  @REC-PRJ-08 @type-methodology @strength-should @src-B2
  Scenario: A commitment to decide is already a project
    Given the user commits to deciding about something with an unknown result ("research life coaches")
    When they clarify it
    Then it is organized as a real project, not parked as vague stuff

  @REC-PRJ-09 @type-tool @strength-may @src-B1
  Scenario: Project support splits into active and archive
    Given a long-running project accumulates material of mixed currency
    When the user organizes its support
    Then they can split it into Active and Archive compartments
    And the project remains scannable as one line on the Projects list

  @REC-PRJ-10 @type-tool @strength-should @src-B2
  Scenario: A verb checklist helps discover projects
    Given the user suspects their Projects list is incomplete
    When they run the project-identification checklist (finalize, implement, research, publish, resolve, ...)
    Then each verb prompts them to capture projects it brings to mind
    And discovered projects land in the inbox or directly on the Projects list

  @REC-PRJ-11 @type-tool @strength-should @src-B2
  Scenario: Personal projects are first-class
    Given the user has "Install new set of tires" and a major work deliverable
    When both are organized as projects
    Then they live on the same Projects list with the same review cadence
    And the tool never structurally segregates personal from professional (user-chosen subdivision remains possible)

  @REC-PRJ-12 @type-tool @strength-may @src-B2
  Scenario: A process project spawns a recurring habit
    Given the user completes the project "set up exercise routine"
    When the deliverable is a recurring procedure
    Then the tool supports spawning a recurring Habit from it
