Feature: Organize - Someday/Maybe
  Incubating what the user is not committed to moving on now.

  @REC-SOM-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: An uncommitted item incubates on Someday/Maybe
    Given the user clarifies an item with no commitment to move now
    When they incubate it
    Then it joins the Someday/Maybe list with no next action attached (its defining trait)
    And the list may legitimately grow longer than the active Projects list, fantasy through realistic

  @REC-SOM-02 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario Outline: Each incubated item chooses its resurfacing mechanism
    Given the user incubates an item
    When they choose the "<mechanism>" mechanism
    Then the item resurfaces via <behavior>

    Examples:
      | mechanism            | behavior                                              |
      | someday list         | the weekly review scan of Someday/Maybe               |
      | future date          | the tickler on the exact date the user picked         |
      | recurring            | a repeating tickler at the chosen interval            |

  @REC-SOM-02 @type-tool @strength-should @src-DA94
  Scenario: An incubated item links to the ideal vision
    Given the user incubates a dream-scale item
    When they flag "include in ideal vision?"
    Then the item is linked to the vision horizon for higher-altitude review

  @REC-SOM-03 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Projects and Someday/Maybe trade places
    Given an active project will get no attention for months
    When the user demotes it during review
    Then it moves to Someday/Maybe intact
    And during a later review an incubated item can be promoted back to an active project

  @REC-SOM-04 @type-tool @strength-may @src-B1
  Scenario: Someday/Maybe subdivides into collection lists
    Given the user accumulates books to read, trips to take, and gift ideas
    When they subcategorize Someday/Maybe
    Then named sub-lists hold them ("soon when resources allow", "bucket list", special-interest lists)
    And collection lists are reviewed by urge or periodically, not on a forced cadence
