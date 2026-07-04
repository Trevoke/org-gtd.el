Feature: Reflect / Review
  Keeping the system current and trusted, and regaining perspective.

  @REC-REF-01 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: The Weekly Review is provided and schedulable
    Given the user runs GTD week over week
    When they schedule the Weekly Review as a recurring calendar event (~1-2 hours)
    Then the tool provides the review as a runnable session
    And completing it both updates system contents to match reality and restores higher-altitude perspective

  @REC-REF-02 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario Outline: The review walks three guided phases
    Given the user is in the "<phase>" phase of the Weekly Review
    Then the tool guides them through <steps>

    Examples:
      | phase        | steps                                                                                                          |
      | Get Clear    | collect loose ends into the inbox, process it to zero, empty the head                                         |
      | Get Current  | mark done items, mine the past calendar, scan the upcoming calendar, review Waiting For, review every project, review checklists |
      | Get Creative | promote or prune Someday/Maybe, capture new and bold ideas                                                    |

  @REC-REF-03 @type-tool @strength-must @src-B1 @src-B2
  Scenario: The project sweep enforces the core invariant
    Given the review reaches the Get Current phase
    When the user walks the complete Projects list one by one
    Then each project is checked for at least one current next action
    And each project's support material and notes are surfaced, scanned for inherent next actions to pull onto lists
    And stuck or malformed projects are corrected, and morphed items are converted into projects

  @REC-REF-04 @type-tool @strength-should @src-B1 @src-B2
  Scenario Outline: Review runs whenever it is needed
    Given the user experiences "<trigger>"
    When they invoke a review
    Then the tool runs it without waiting for the weekly slot

    Examples:
      | trigger                                          |
      | the regular weekly cadence                       |
      | key projects feel like they're lagging           |
      | grip on short-term priorities feels lost         |

  @REC-REF-04 @type-tool @strength-should @src-B1 @src-B2
  Scenario: A get-back-on-track recovery flow exists
    Given the user has fallen off the system
    When they run the recovery flow
    Then it re-empties the head, cleans the lists, and reclaims items that leaked outside the system

  @REC-REF-05 @type-tool @strength-should @src-B1 @src-B2
  Scenario Outline: Review cadence scales with horizon
    Given the "<horizon>" horizon
    Then its review reminder defaults to "<cadence>" (user-configurable)
    And the horizon is reviewable as its own dedicated process, one at a time, capturing associative ideas

    Examples:
      | horizon              | cadence             |
      | Runway / actions     | daily               |
      | Projects             | weekly              |
      | Areas of Focus       | monthly             |
      | Goals                | monthly-quarterly   |
      | Vision               | annually            |
      | Purpose & Principles | annually or longer  |

  @REC-REF-06 @type-tool @strength-should @src-B1 @src-B2
  Scenario: The system itself gets maintained
    Given system outdatedness starts grabbing the user's attention
    When they run a system-maintenance review
    Then filing is purged, list management rethought, tools refreshed
    And a project with a next action is spawned for any maintenance needing real work
    And elevated-horizon events (annual reviews, off-sites) are explicitly calendar-scheduled

  @REC-REF-07 @type-methodology @strength-should @src-B1
  Scenario: Every reminder location is reviewed equally
    Given reminders live in several locations (lists, queues, folders)
    When reviews run over time
    Then every location gets swept regularly — nothing entrusted to the system goes unseen
    And the tool's views make full coverage the path of least resistance

  @REC-REF-08 @type-methodology @strength-should @src-B1
  Scenario: Stale lists are healed by review, not by nagging
    Given a Calls list has drifted out of date
    Then the user can no longer trust the system from that list alone
    When the Weekly Review runs
    Then currency is restored as the prescribed remedy
    And any proactive staleness signal is an optional software affordance (see REC-UI-02), never a gate

  @REC-REF-09 @type-tool @strength-should @src-B2
  Scenario: The review blocks time for big actions
    Given the review surfaces an important action needing a large window
    When the user time-blocks it
    Then a calendar block is created in the coming week directly from the review flow

  @REC-REF-10 @type-tool @strength-may @src-B2
  Scenario: A year-end stock-taking feeds next year's goals
    Given the year (or any cycle) is ending
    When the user runs the annual review variant
    Then accomplishments are inventoried
    And the result feeds goal-setting at the 30k horizon
