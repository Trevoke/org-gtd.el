Feature: Software / UI ideas
  Design ideas from David Allen's 1994 software sketches. The books supersede
  1994 on methodology; these UI/software mechanisms stand on their own.

  @REC-UI-01 @type-tool @strength-may @src-DA94
  Scenario: A dashboard shows the system at a glance
    Given the user opens the home view
    Then projects, next actions, waiting-fors, and the calendar are visible together
    And each list opens into its full module

  @REC-UI-02 @type-tool @strength-should @src-DA94 @src-B1
  Scenario Outline: Integrity warnings surface proactively
    Given the system detects "<condition>"
    Then the dashboard shows the flag "<flag>"
    And clicking the flag takes the user to the fix

    Examples:
      | condition                                | flag                                                |
      | unprocessed inbox items                  | You have N items to process in your in-basket       |
      | active projects without a next action    | You have N projects with no next action defined     |
      | a past calendar item not marked done     | Previous calendar item not completed                |

  @REC-UI-03 @type-tool @strength-may @src-DA94
  Scenario: Coaching teaches while correcting
    Given the user takes an action that violates a GTD principle
    When the tool intervenes
    Then the dialog states the principle ("calendars are for items that expire only") while offering the fix
    And the coach's presentation adapts to what is being processed (overlay vs panel)

  @REC-UI-04 @type-tool @strength-should @src-DA94
  Scenario: Incomplete calendar items migrate with consent
    Given yesterday's calendar action was never marked done
    When the system processes the new day
    Then the item is moved to the Next Actions list by default
    And the user may veto via cancel or reschedule (picking a new date)
    And the calendar stays a pure record of what expires

  @REC-UI-05 @type-tool @strength-should @src-DA94
  Scenario: Marking done asks what is next
    Given the user marks a project-linked action done
    Then the tool asks "is the project complete?"
    And if not, it asks "what's the next action?" and routes the answer
    So that no project silently goes stuck at the moment of completion

  @REC-UI-06 @type-tool @strength-may @src-DA94
  Scenario Outline: An action carries exactly one date semantic
    Given an action scheduled as "<semantic>"
    Then no other date semantic can coexist on it

    Examples:
      | semantic                  |
      | can only do ON a date     |
      | need to do BY a date      |
      | need to START BY a date   |
      | as soon as appropriate    |

  @REC-UI-07 @type-tool @strength-may @src-DA94
  Scenario: Action entry starts from a verb
    Given the user composes a new next action
    When entry begins
    Then a verb menu (call, draft, brainstorm, review, edit, read, get/buy, organize, take) seeds the phrasing
    And a verb can drive routing (get/buy offers the errand sub-lists)

  @REC-UI-08 @type-tool @strength-may @src-DA94
  Scenario: Lists sort by user-chosen keys
    Given any list view
    When the user picks a sort key (due date, priority, project, age, size, personal vs professional)
    Then the list re-sorts accordingly

  @REC-UI-09 @type-tool @strength-may @src-DA94
  Scenario: Global find spans the system
    Given the user searches by person, keyword, date, date range, project, or area
    Then matches from every module are returned

  @REC-UI-10 @type-tool @strength-may @src-DA94
  Scenario: Every exit path closes its loop
    Given the user starts an outbound communication and abandons it midway
    Then a next action "finish <communication>" is generated automatically
    And sending a delegation can spawn its Waiting For in the same step
    And meeting outputs and person-notes route to the inbox rather than evaporating

  @REC-UI-11 @type-tool @strength-may @src-DA94
  Scenario: User-defined rules automate routine handling
    Given the user defines a rule of the form "whenever <event>, do <system action>"
    When the event occurs
    Then the system performs the action automatically
    # The "AA flight -> schedule 72hr upgrade" line in the source is an EXAMPLE of a rule, not a feature

  @REC-UI-12 @type-tool @strength-may @src-DA94
  Scenario: The system prints itself
    Given the user needs paper
    When they print
    Then any view prints in a chosen format
    And a complete, up-to-the-second hard-copy system can be generated
    And person briefs and blank note forms print on demand

  @REC-UI-13 @type-tool @strength-may @src-DA94
  Scenario: The past is an archive, not a void
    Given days have gone by
    When the user reviews past daily pages/calendar entries
    Then they can annotate and archive them
    And the retrospective log remains queryable

  @REC-UI-14 @type-tool @strength-may @src-DA94
  Scenario: Processing reaches other software without leaving
    Given clarifying an item requires another application (mail, editor, dialer)
    When the user acts on it
    Then the external tool is launched from within the workflow
    And the user returns to the processing flow where they left it

  @REC-UI-15 @type-tool @strength-may @src-DA94
  Scenario: Alarms and timers support the flow
    Given the user chooses "do it now" on a two-minute action
    Then an optional two-minute timer can run
    And general alarms can be attached where the user wants them

  @REC-UI-16 @type-tool @strength-may @src-DA94
  Scenario: Rollup views slice the system vertically
    Given the user picks an Area of Focus
    When they open its rollup
    Then the area's goals, active projects, next actions, someday/maybes, routines, reference, and resource people appear together
    And composite horizon "sets" (current commitments / areas / goals and objectives) are likewise available

  @REC-UI-17 @type-tool @strength-may @src-DA94
  Scenario: A project shows its ledger and metadata
    Given the user opens a project
    Then actions done and current next actions appear side by side as a ledger
    And entry dates, stakeholders (customers/recipients), and an upward link to a larger area/objective are recordable
    And due-dated projects sort first on the Projects list

  @REC-UI-18 @type-tool @strength-may @src-DA94
  Scenario: The calendar layers month, week, and day
    Given the user opens the calendar
    Then month, week, and day layers are navigable
    And the day view splits time-specific from day-specific (due-by vs start-by) with a day-notes box
    And a week strip can show each day's location or theme
