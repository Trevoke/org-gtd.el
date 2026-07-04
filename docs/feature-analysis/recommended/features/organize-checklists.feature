Feature: Organize - Checklists
  Reusable lists that trigger thinking and verify routines.

  @REC-CHK-01 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario: Checklists are first-class, cheap to create and delete
    Given the user wants a travel checklist, a weekly-review checklist, or an area "ingredients" list
    When they create it
    Then it exists as a reviewable, reusable list
    And it can be surfaced on demand or attached to a review
    And deleting an obsolete checklist is as cheap as creating one
    And the Incompletion and Project Planning trigger lists ship as bundled examples

  @REC-CHK-02 @type-tool @strength-may @src-B1
  Scenario: A reflection-prompt list resurfaces on schedule
    Given the user keeps a checklist of people they want to stay connected to
    When they schedule it to resurface every quarter
    Then on schedule the checklist appears as a thinking prompt
    And ideas it triggers ("reconnect with X") are captured to the inbox
