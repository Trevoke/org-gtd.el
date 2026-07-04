Feature: Cross-cutting
  Invariants, attributes, and design constraints spanning all stages.

  @REC-X-01 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: Every open loop has a next action
    Given any actionable item or active project in the system
    Then it carries a defined next action
    And an item lacking one is detectable so Reflect can surface it
    And dependency-gated subprojects in support material are the sole exception

  @REC-X-02 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Actions carry a context attribute
    Given a next action requires a location, tool, or situation
    When the user tags it with a context
    Then the context is a user-defined value, and the engage view filters on it

  @REC-X-03 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Actions carry a time estimate
    Given the user wants to filter by time available
    When they set an effort estimate on an action
    Then views can select e.g. only ~10-minute actions

  @REC-X-04 @type-tool @strength-may @src-B1 @src-B2
  Scenario: Energy tagging is optional
    Given energy is an in-the-moment human selection criterion, not stored truth
    When the user opts in to an energy tag/property
    Then a low-horsepower inventory becomes filterable
    And the feature is off by default and never required

  @REC-X-05 @type-tool @strength-must @src-B1 @src-B2
  Scenario: Hard edges keep meanings apart
    Given a clarified item
    When it is organized
    Then it lands in exactly one meaning-category (outcomes, actions, incubating, support, reference, trash)
    And no location ever blends actionable items with reference or support material
    And being organized means location matches meaning — re-checked continuously, never a one-time event

  @REC-X-06 @type-tool @strength-must @src-B1 @src-B2
  Scenario: The taxonomy bends to the user
    Given the user's needs evolve
    When they create, rename, merge, or subdivide lists and contexts
    Then the tool supports it without hard-coded category walls
    And a personal/professional subdivision is allowed, with guidance against over-categorizing

  @REC-X-07 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Items re-route cheaply across their lifetime
    Given an item's meaning changes (Waiting For becomes At Computer becomes Waiting For again)
    When the user recategorizes it
    Then the move is a first-class, low-friction operation, repeatable any number of times

  @REC-X-08 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario: Items link to an Area of Focus
    Given a project assigned to an area
    Then its actions inherit the area by default (overridable only for standalone actions)
    And area-grouped views and coverage checks become possible

  @REC-X-09 @type-tool @strength-may @src-B1 @src-B2
  Scenario: Projects group into named sub-lists
    Given the user runs many similar projects (e.g. "Presentations")
    When they create a named sub-list
    Then projects group/filter under it
    And the sub-list can sort by an optional upcoming-event date

  @REC-X-10 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Old systems migrate through the inbox
    Given the user arrives with pre-existing lists and organizers
    When they onboard
    Then prior content re-feeds through the inbox as unprocessed "in", one area at a time
    And a standing meta-project ("Finalize personal management system setup") tracks the rollout

  @REC-X-11 @type-tool @strength-must @src-B1 @src-DA94
  Scenario: The five stages run as distinct modes
    Given capturing, clarifying, organizing, reflecting, and engaging are different jobs
    When the user works in any one stage
    Then the tool never forces the other stages into the same step
    Because "you can't organize what's incoming — you can only capture it and process it"

  @REC-X-12 @type-tool @strength-should @src-B2
  Scenario: The tool refuses to over-structure
    Given GTD software historically overshoots, "requiring too much mental effort to make life fit into supplied forms"
    When the user creates or edits any item
    Then only the minimal fields are required
    And all richer structure is opt-in
    Because unduly complicated structure is itself disorganization

  @REC-X-13 @type-tool @strength-may @src-B2 @src-DA94
  Scenario: Cross-linking is offered, never required
    Given the user wants to relate a project to people, dates, meetings, or reference
    When they add links
    Then the tool supports relational linking
    But no workflow ever gates on links existing
    Because diligent review substitutes for manual cross-references

  @REC-X-14 @type-tool @strength-should @src-B1
  Scenario: The system travels with the user
    Given the user is at home, at the office, or in transit
    When they need their lists and calendar
    Then the working system is accessible from each location
    And nothing about engagement assumes a single desk

  @REC-X-15 @type-methodology @strength-may @src-B1
  Scenario: A thin system is a warning sign
    Given the user's system holds well under ~50 next actions plus waiting-fors including agendas
    When onboarding or review evaluates completeness
    Then the tool may surface skepticism that capture is complete
    And the suggested remedy is a fuller mind sweep, never an artificial quota
