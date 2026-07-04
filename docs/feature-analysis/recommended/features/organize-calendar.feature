Feature: Organize - Calendar
  The sacred hard landscape: only what truly belongs to a day or time.

  @REC-CAL-01 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario Outline: Only three kinds of things land on the calendar
    Given the user organizes an item of kind "<kind>"
    Then the calendar <accepts>

    Examples:
      | kind                                   | accepts                                        |
      | time-specific appointment              | accepts it at its time                         |
      | day-specific action (no fixed time)    | accepts it on its day                          |
      | day-specific information               | accepts it as a note on its day                |
      | undated to-do                          | rejects it - it belongs on an action list      |
      | "hope to get done today" wish          | rejects it - the calendar is sacred territory  |

  @REC-CAL-01 @type-tool @strength-must @src-B2
  Scenario: Others can add to the user's calendar
    Given an external party sends a calendar commitment
    When the user accepts it
    Then it lands as a hard-landscape item like any self-created one

  @REC-CAL-02 @type-tool @strength-should @src-B1 @src-B2
  Scenario: A future trigger parks on a day
    Given the user wants to reconsider starting a project on March 1
    When they park a day-specific trigger on March 1
    Then on that day the trigger surfaces
    And the user activates it (e.g. onto the Projects list) or re-defers it
