# Canonical GTD workflows — stage: ORGANIZE
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# "organized" is formally defined as meaning matching location.

Feature: Organize — parking clarified items where their meaning says they belong
  "Being organized simply means that where things are suits what they mean to
  you." The buckets stay pristinely distinct; blending them destroys trust in
  the whole system.

  @WF-11 @cadence-per-item @src-B1-W-11 @src-B2-W-09
  Rule: WF-11 — The bucket system with hard edges

    Background:
      Given clarified items need a home
      And meaning precedes organization

    Scenario: Maintain exactly the primary categories
      Then the system maintains exactly these primary buckets:
        | bucket                            |
        | Projects list                     |
        | project support material          |
        | calendar actions and information  |
        | next-action lists                 |
        | Waiting For list                  |
        | reference material                |
        | Someday/Maybe list                |
        | trash                             |
      And longer-horizon outcome lists (goals, vision, purpose, areas of focus) sit above the Projects list

    Scenario: Invariant — categories never blend
      Then the categories are kept visually, physically, and psychologically separate
      And reference mixed with read-me material creates numbness
      And calendar items mixed onto action lists destroy trust in the calendar
      And inactive projects on the Projects list dilute it
      And Waiting For items on action lists cause nonproductive rethinking

    Scenario: No priority scaffolding on lists
      When lists are built
      Then no priority codes are imposed on them
      And prioritizing happens intuitively at run time against the whole inventory

    Scenario: As simple as possible, but no simpler
      When the user is tempted to overcategorize
      Then the impulse is recognized as distrust of their own review habit
      When the user is tempted to delete a needed category
      Then it is recognized that deleting a category just re-blends its contents

    Scenario: Organized means meaning matches location
      Then every clarified item lives in exactly one bucket whose meaning matches the user's agreement with it

  @WF-12 @cadence-per-item @cadence-daily @src-B1-W-12 @src-B2-W-10 @src-DA94-W08
  Rule: WF-12 — Calendar discipline ("sacred territory" / the hard landscape)

    Background:
      Given a deferred action or piece of information may belong on the calendar

    Scenario: Only three things go on the calendar
      Then the calendar holds only:
        | type                     | meaning                                             |
        | time-specific actions    | appointments                                        |
        | day-specific actions     | must happen that day, any time during it            |
        | day-specific information | directions, lead-time triggers, "call after she returns" |
      And nothing else goes on the calendar

    Scenario: No daily to-do lists on the calendar
      When the user wants to note something they would merely like to do that day
      Then it goes to a next-action list, not the calendar
      And an informal "if I have time today" list is allowed but never confused with the have-tos

    Scenario: Calendar entries are kept absolute
      When something is written on a day
      Then it must get done that day or not at all
      And the only rewriting is for changed appointments

    Scenario: Uncompleted calendar items migrate with consent
      Given a calendar action was not recorded as done by the end of its day
      When the system detects it
      Then the user is told "Calendars are for items that expire only"
      And the item is moved to the next-action list unless the user cancels it or reschedules it to a new date

    Scenario: Invariant — the calendar is trusted at a glance
      Then a single glance at the calendar gives a trusted sense of what can and cannot be afforded at that moment

  @WF-13 @cadence-per-item @src-B1-W-13 @src-B2-W-09
  Rule: WF-13 — Context-sorted next-action lists

    Background:
      Given deferred actions exist that take more than two minutes, belong to the user, and are not date-bound

    Scenario: Sort action reminders by context
      When deferred actions are organized
      Then they are sorted by the tool, location, or situation required to do them
      And common contexts include Calls, At Computer, Errands, At Office, At Home, Anywhere, Agendas per person or meeting, and Read/Review
      And custom contexts are encouraged, such as "Before Trip" or "Brain Gone"

    Scenario: Granularity follows volume and context-switching
      Given about twenty-five or fewer tracked actions
      Then a single list is sufficient
      Given fifty to one hundred fifty tracked actions
      Then the lists are subdivided by context

    Scenario: Context conventions
      Then Calls items carry the phone number alongside
      And Errands items may carry sublists of things to get at each location
      And a separate Agenda list exists for each significant person and standing meeting
      And Read/Review is reserved strictly for reading longer than two minutes

    Scenario: Context sorting forces the next-action decision
      When an action cannot be filed on a context list
      Then that exposes that the next physical action has not actually been decided

    Scenario: Invariant — visibility per context
      Then at any moment, in any context, the user sees exactly the actions doable there and nothing else

  @WF-14 @cadence-per-item @cadence-weekly @src-B1-W-14 @src-B2-W-09 @src-DA94-W05
  Rule: WF-14 — Waiting For list management

    Background:
      Given things have been delegated, ordered, lent, or are otherwise pending from others

    Scenario: Track deliverables others owe you
      When an item enters the Waiting For list
      Then the entry records the deliverable, who has it, the date it was requested, and any due date
      And the user's responsibility is to track who's got it, confirm when they got it, and check its status

    Scenario: Review and follow up
      When the list is reviewed
      Then the user decides per entry whether to take a follow-up action
      And the list is at hand alongside the next-action lists
      And it is visible when meeting anyone responsible for an item

    Scenario: Invariant — a complete inventory of others' commitments
      Then the Waiting For list is the complete inventory of everything the user cares about that other people are supposed to be doing
      And every entry carries the date it was recorded

  @WF-15 @cadence-daily @src-B1-W-15
  Rule: WF-15 — E-mail "in" to zero

    Background:
      Given the user is processing the e-mail inbox

    Scenario: Process e-mail like any "in"
      When the inbox is processed
      Then deletable messages are deleted
      And keepers are filed into reference folders
      And replies of two minutes or less are done immediately

    Scenario: Park actionable and pending e-mail in dedicated folders
      Then an "@ACTION" folder holds e-mails requiring more than two minutes of action
      And an "@WAITING FOR" folder holds things others owe, including copies of the user's own delegations
      And both folders sort to the top of the folder list
      And the inbox itself is left empty

    Scenario: Review the folders like action lists
      Then "@ACTION" is treated as an extension of the At Computer list and reviewed like one
      And dispersed reminders are acceptable only if all locations are reviewed equally

    Scenario: Invariant — empty does not mean handled
      Then anything residing in the e-mail "in" is by definition unprocessed new input
      And getting "in" empty does not mean everything has been handled

  @WF-16 @cadence-per-item @cadence-daily @cadence-weekly @src-B1-W-16 @src-B1-W-17 @src-B1-W-18 @src-B2-W-11
  Rule: WF-16 — Incubation system: Someday/Maybe and date-triggered resurfacing

    Background:
      Given an item has been clarified as "no action now, but maybe or definitely later"

    Scenario: Choose the resurfacing structure per item
      When an item is incubated
      Then it goes into exactly one of two structures:
        | structure                      | when                                              |
        | Someday/Maybe list             | needs review with some regularity                  |
        | calendar entry or tickler file | needs to surface only on a specific future date    |
      And recurring resurfacing may be set as a repeating tickler

    Scenario: Invariant — Someday/Maybe items carry no next action
      Then the defining characteristic of a Someday/Maybe item is that no next action is attached to it
      And special-interest sublists such as "books to read" or "trips to take" are allowed

    Scenario: Decide not to decide, with a net
      When something needs reassessment at a known future date rather than now
      Then a day-specific information entry or tickler is set for that date
      And the principle holds: "It's OK to decide not to decide—as long as you have a decide-not-to-decide system"
      And when the trigger fires, the user either activates it onto the Projects list or re-decides

    Scenario: Demote projects when capacity is exceeded
      Given the active lists exceed real capacity
      When the user reviews each project consciously
      Then projects that will not get attention move to the Someday/Maybe holding tank
      And the move works only because the holding tank itself is trusted and reviewed

    Scenario: The tickler daily ritual
      Given a tickler file of forty-three folders, thirty-one daily and twelve monthly
      When each day begins
      Then that day's folder is emptied into the in-tray and refiled at the back for next month
      And on month rollover the monthly folder's contents are distributed into the daily folders
      And before travel the user checks the folders for the days they will be away

    Scenario: Invariant — the tickler is checked every day
      Then the tickler is checked and emptied every day without exception
      And a forgotten daily folder breaks trust in the whole system

    Scenario: Invariant — Someday/Maybe is reviewed weekly
      Then the Someday/Maybe list is reviewed in every Weekly Review, reactivating, deleting, or adding items
      And a "hold and review" pile that is held but never reviewed goes numb

  @WF-17 @cadence-monthly-plus @src-B1-W-19
  Rule: WF-17 — Checklists as external mind

    Background:
      Given a recurring procedure, unfamiliar responsibility, or fuzzy intention exists that is not a single project or action

    Scenario: Extract the project and action first
      When the user examines a fuzzy item such as "exercise more regularly"
      Then any inherent project and next action are extracted first
      And what remains becomes a checklist

    Scenario: Give every checklist a review trigger
      When a checklist is created
      Then it has a defined recurrence at which it is reviewed
      And the more novel the situation, the more checklist control is applied
      And checklists are retired once the area is on cruise control

    Scenario: Invariant — fuzzy commitments are never left unanchored
      Then every recurring or fuzzy commitment is either a project plus next action, or a checklist with a defined review trigger

  @WF-18 @cadence-per-item @src-B1-W-02 @src-B2-W-09
  Rule: WF-18 — General-reference filing (the sixty-second standard)

    Background:
      Given a nonactionable item with potential future value appears

    Scenario: File it in under a minute
      When the user files the item
      Then it takes less than one minute to pick it up out of "in" and finish storing it in the trusted system
      And it goes into its own labeled folder in a single A-to-Z general-reference system
      And it is not organized into multiple systems or by project or area

    Scenario: Structural success factors
      Then files are within reach, fresh folders are at hand, labels are typeset
      And drawers are kept less than three-quarters full
      And there are no "to-file" piles

    Scenario: Invariant — purge at least yearly
      Then the reference files are purged at least once a year
      And a purge day may be put in the tickler file

    Scenario: Failure mode
      Given filing takes longer than a minute
      Then the user will stack instead of file and will resist emptying "in"

  @WF-19 @cadence-per-item @cadence-weekly @src-B1-W-21 @src-B2-W-09
  Rule: WF-19 — Project support material handling

    Background:
      Given plans, notes, and ad hoc ideas accumulate around projects

    Scenario: Invariant — support material is never the action reminder
      Then the project lives on the Projects list and its actions live on action lists or Waiting For
      And support material remains purely adjunct data, pulled out only when doing the action or reviewing the project

    Scenario: Route stray project thoughts home
      When a project thought occurs anywhere
      Then it is captured in the nearest capture tool
      And it is then routed to the project's home: attached notes, a dedicated folder, or notebook pages

    Scenario: Weekly harvest
      When the Weekly Review reaches the project pass
      Then project plans and support material are browsed to harvest new actions
      And stale notes are purged
      And active project files are kept more accessible than archives
