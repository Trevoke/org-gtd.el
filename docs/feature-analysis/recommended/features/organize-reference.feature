Feature: Organize - Reference
  Non-actionable but valuable material: fast in, easy out.

  @REC-KNO-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: Valuable non-actionable material files to Reference
    Given the user clarifies an item that is not actionable but worth keeping
    When they file it to Reference
    Then it lands in a topic-organized store, retrievable later
    And topic/area-specific stores coexist with a general catch-all

  @REC-KNO-02 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Filing takes less than a minute
    Given the user holds an item to file
    When they file it
    Then the whole operation completes in under ~60 seconds
    And the store presents a single flat A-Z index by default (multi-level alpha allowed)
    And a yearly purge can be scheduled via the tickler

  @REC-KNO-03 @type-methodology @strength-may @src-B1
  Scenario: Contacts are pure reference
    Given the user stores a person's phone, email, and birthday
    Then the contact record carries no embedded action triggers
    And actions concerning that person live on Agenda or Calls lists instead

  @REC-KNO-04 @type-tool @strength-should @src-B1
  Scenario: New reference categories cost nothing to create
    Given the user is filing an item that fits no existing category
    When they create a new category in the flow of filing
    Then creation is instant — no setup ceremony, no leaving the filing flow

  @REC-KNO-05 @type-tool @strength-should @src-B1
  Scenario: Retrieval is browsable, not search-only
    Given the user wants something filed months ago but cannot recall its keywords
    When they open the reference store
    Then a categorized overview/index lets them visually scan what exists
    And the system does not rely on search alone (which breeds write-only storage)

  @REC-KNO-06 @type-tool @strength-may @src-DA94
  Scenario: Reference lists take user-designed forms
    Given the user keeps structured reference lists
    When they design a list's fields/template
    Then the tool supports the custom form
    And an escape hatch to an external editor remains available
