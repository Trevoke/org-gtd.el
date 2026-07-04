Feature: Organize - Tickler
  Date-triggered re-surfacing of anything, on its day.

  @REC-TIC-01 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: Any item can be future-dated to resurface
    Given the user wants an item back on a specific future day
    When they tickle it for that date
    Then on that day it resurfaces in the inbox or agenda
    And the tickler is checked daily as part of engaging
    And before traveling, the user can look ahead across the days they will be away

  @REC-TIC-02 @type-tool @strength-should @src-B1 @src-DA94
  Scenario: Tickler entries recur and give lead time
    Given the annual sales conference happens every year
    When the user creates a recurring tickler with 6 weeks advance notification
    Then next year's instance re-surfaces automatically
    And the reminder appears 6 weeks ahead of the date, giving preparation lead time
