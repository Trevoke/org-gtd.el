Feature: Engage
  Choosing what to do (and not do) right now, with trust.

  @REC-ENG-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: Choices come from a total-life inventory
    Given the user's organized system holds 100-150+ actions, personal and professional
    When they engage
    Then all options are accessible from one trusted inventory
    And the tool never reduces engagement to a daily to-do list of "latest and loudest"

  @REC-ENG-02 @type-tool @strength-must @src-B1 @src-B2
  Scenario: The calendar is reviewed first
    Given the user starts engaging with their day
    When they open the engage view
    Then the day's hard landscape (calendar) is reviewed first
    And context-filtered action lists come after
    And after completing a calendar item, the user can see what else remains for the day

  @REC-ENG-03 @type-methodology @strength-must @src-B1 @src-B2
  Scenario: The four criteria order the choice
    Given the user must pick the next thing to do
    When they choose
    Then the criteria apply in order: context, then time available, then energy available, then priority
    And the tool's filters scaffold each criterion without ever choosing for the user

  @REC-ENG-04 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario: Context filtering with an escape hatch
    Given actions carry user-customizable context tags (Calls, At Computer, Errands, "At sailboat", ...)
    When the user engages from a given context
    Then the view filters or folds to actions doable here and now
    But a user with ~25 or fewer actions may run one undivided Next Actions list, and the tool supports that too

  @REC-ENG-05 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Time available narrows the list
    Given the user has ten minutes before a meeting
    When they filter by time available
    Then only actions with effort estimates fitting the window are shown

  @REC-ENG-06 @type-methodology @strength-should @src-B1 @src-B2
  Scenario Outline: Three modes of work are all legitimate
    Given the user is engaging
    When they choose to <mode>
    Then the tool treats it as legitimate engagement

    Examples:
      | mode                                              |
      | do predefined work from the lists                 |
      | do ad-hoc work as it appears                      |
      | define work (process inputs and clarify)          |

  @REC-ENG-07 @type-tool @strength-should @src-B2
  Scenario: Discretionary time is evident at a glance
    Given the user looks at the engage view in the morning
    Then appointments and day-specific items lead the view
    And the remaining discretionary time of the day is evident from it

  @REC-ENG-08 @type-tool @strength-may @src-B1
  Scenario: Weird time gets weird-time work
    Given the user has a random five-minute opening
    When they ask for micro-window actions
    Then the view surfaces only very short, low-effort actions suited to the window
