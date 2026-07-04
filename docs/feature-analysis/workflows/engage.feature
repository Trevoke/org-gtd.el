# Canonical GTD workflows — stage: ENGAGE
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the canonical stage name is "engage" (not "do"), and priorities resolve via
# the six horizons plus the limiting factors — NOT via A-B-C / 1-2-3 /
# high-medium-low priority codes, which book 2 explicitly rejects.

Feature: Engage — choosing and doing actions from trust instead of hope
  At any moment of discretionary time, the user filters the complete action
  inventory through the limiting factors, then lets intuition informed by the
  horizons pick — and can feel as good about what they're not doing as about
  what they are doing.

  @WF-23 @cadence-per-item @src-B1-W-25 @src-B2-W-15
  Rule: WF-23 — Choosing the action of the moment (limiting factors, then horizons)

    Background:
      Given the user has a moment of discretionary time and more defined actions than can be done today

    Scenario: No simple priority codes
      When the user chooses what to do
      Then no simple priority grading method such as A, B, C, or 1, 2, 3, or High, Medium, Low is used
      And the choice combines the six horizons of commitments, the three limiting factors, and the threefold nature of work

    Scenario: Limiting factor 1 — context
      When the user filters options
      Then only actions doable here, with the tools at hand, are considered
      And the matching context list supplies them, because "At any single point in time, you can only do what you can do"

    Scenario: Limiting factor 2 — time available
      When the remaining options are filtered
      Then only actions that fit before the next hard commitment are considered
      And actions needing big chunks get time blocked in the Weekly Review

    Scenario: Limiting factor 3 — energy available
      When the remaining options are filtered
      Then the task at hand is matched to the current state of mind and body
      And a batch of simple little tasks is kept so that when energy is depleted the user can still be good for something

    Scenario: Priority last, by intuition informed by the horizons
      When the user asks "Out of all my remaining options, what is the most important thing for me to do?"
      Then the answer comes from a trusted combination of intelligence and intuition
      And that intuition is hard-wired by reflection at all six horizons

    Scenario: Default bias is motion
      When the user hesitates
      Then taking any action gives more of a sense of control than hanging back in hesitation

    Scenario: Invariant — trusted not-doing
      Then the user can feel as good about what they are not doing as about what they are doing at that moment

  @WF-24 @cadence-daily @src-B1-W-26 @src-B2-W-16
  Rule: WF-24 — The threefold nature of work (predefined / ad hoc / defining)

    Background:
      Given a workday in motion

    Scenario: The three kinds of work
      Then at any moment the user is doing exactly one of:
        | kind                              | meaning                                              |
        | doing predefined work             | working from next-action lists and the calendar       |
        | doing work as it shows up         | ad hoc demands and surprises                          |
        | defining the work                 | processing inputs, breaking projects into actions     |
      And all three get deliberate time

    Scenario: Ad hoc work must be a conscious choice
      When unexpected work shows up
      Then choosing it is a conscious judgment that it is more important than anything else against a trusted, complete inventory
      And the sacrifice of not doing defined work can be tolerated only if the user knows what they are not doing

    Scenario: Budget daily time for defining work
      Then processing in-trays, e-mail, and voice mail to zero takes thirty to ninety minutes a day
      And the in-tray never rots long enough for its contents to resurface as emergencies

    Scenario: Idle time goes to backlog
      When the user is idle
      Then backlog is cleaned up, so that when an unanticipated surprise hits there is as little residue as possible in the psyche

  @WF-25 @cadence-per-item @src-DA94-W04
  Rule: WF-25 — Action-completion closure loop (no project left without a next action)

    Background:
      Given the user marks a next action as done

    Scenario: Completion interrogates the project
      When the action belongs to a project
      Then the user is asked "Is the project complete?"
      And if the project is not complete, the user is asked "What's the next action?"
      And the answer is recorded into the system before moving on

    Scenario: Completing the project
      When the project is declared complete
      Then the project is closed and removed from the active Projects list

    Scenario: Invariant — no stuck projects
      Then no active project is ever left without a defined next action after one of its actions completes
