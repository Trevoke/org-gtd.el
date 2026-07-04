Feature: Organize - Trash
  Discarding what has no action and no future value.

  @REC-TRA-01 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: A worthless item is discarded without residue
    Given an inbox item is non-actionable
    And the user judges it to have no future or reference value
    When the user marks it non-actionable with no future value
    Then it is discarded
    And the system keeps no further tracking, reminder, or residue of it
    And Trash is offered as a first-class destination of the clarify dispatch, equal to any other
