# Canonical GTD workflows — stage: REFLECT
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the canonical stage name is "reflect" (not "review"), the Weekly Review is
# formalized as Get Clear / Get Current / Get Creative, and per-horizon review
# intervals are codified.

Feature: Reflect — putting things on the mind appropriately to get them off it
  "An unused system is not a system." Reflection has a dual function: update
  the system's contents, and provide trusted perspective. "You can only feel
  good about what you're not doing when you know what you're not doing."

  @WF-20 @cadence-daily @src-B1-W-22 @src-B2-W-10 @src-B2-W-12
  Rule: WF-20 — Daily orientation review (calendar first, then context lists)

    Background:
      Given the user is starting the day, finishing a calendar item, or has a free window

    Scenario: Calendar and tickler are the first things each morning
      When the day begins
      Then the calendar's appointments and day-specific items are the first things the user takes note of
      And the daily tickler folder is emptied into "in"
      And this reveals how much discretionary time exists

    Scenario: Re-orient after every calendar item
      When the user concludes an action on the calendar
      Then they check what else remains to be done that day

    Scenario: Work from context lists in discretionary time
      When the calendar shows discretionary time
      Then the user works from the next-action lists for the current context
      And predefined options are evaluated against incoming work

    Scenario: Situational reviews
      Then the Agenda list for a person is reviewed before talking to that person
      And the Errands list is reviewed before going out
      And the Projects list is reviewed when scope questions arise

    Scenario: The governing question
      When deciding what to review
      Then the user asks "When do I need to see what, in what form, to get it off my mind?"
      And lists beyond calendar and actions are reviewed only as often as needed to stop wondering about them

  @WF-21 @cadence-weekly @src-B1-W-23 @src-B2-W-13 @src-DA94-W07
  Rule: WF-21 — The Weekly Review: Get Clear, Get Current, Get Creative

    Background:
      Given a weekly one-to-two-hour executive session with oneself, ideally at the end of the workweek
      And the function of the session is to get clear, current, and creative

    Scenario: Get Clear — collect loose papers and materials
      When the review begins
      Then strays — business cards, receipts, scraps, desk and briefcase contents — are gathered into the in-tray

    Scenario: Get Clear — get "in" to zero
      Then all outstanding papers, journal and meeting notes, voice mails, and e-mails are processed completely through the clarifying decision tree

    Scenario: Get Clear — empty your head
      Then any uncaptured new projects, action items, waiting-fors, and someday/maybes are put in writing and processed

    Scenario: Get Current — review action lists
      Then completed actions are marked off
      And reminders of further action steps are recorded

    Scenario: Get Current — review previous calendar data
      Then the past two to three weeks of calendar are reviewed for remaining or emergent actions and reference material
      And daily pages since the last review may be annotated and archived

    Scenario: Get Current — review upcoming calendar
      Then upcoming events are reviewed long- and short-term
      And actions and preparations they trigger are captured
      And time is blocked as needed

    Scenario: Get Current — review the Waiting For list
      Then follow-ups are recorded where needed
      And received items are checked off

    Scenario: Get Current — review the Projects list one by one
      When each project, goal, and larger outcome is evaluated one by one
      Then at least one current action item for each is ensured to be in the system
      And project plans and support material are browsed

    Scenario: Get Current — review relevant checklists
      Then any relevant checklists are reviewed for new actions and additions

    Scenario: Get Creative — review the Someday/Maybe list
      Then ripe items are activated onto the Projects list
      And dead items are deleted

    Scenario: Get Creative — be creative and courageous
      When the user asks "Any new, wonderful, harebrained, creative, thought-provoking, risk-taking ideas to add into your system?"
      Then any new ideas are captured into the system

    Scenario: Invariant — every active project leaves with a next action
      Then after the Weekly Review, every active project has at least one current next action in the system

    Scenario: The review is done
      Then the user can honestly say "I absolutely know right now everything I'm not doing but could be doing if I decided to"
      And all lists are current and the head is empty
      And the user has thought enough so they don't have to think — just act

  @WF-22 @cadence-monthly-plus @src-B2-W-12 @src-B1-W-24
  Rule: WF-22 — The reflection cadence ladder (higher-horizon reviews)

    Background:
      Given the system is populated and operational levels are under control
      And the rule holds: the longer the horizon, the longer the interval between reviews

    Scenario: Apply the cadence ladder
      Then each horizon is reviewed at its interval:
        | horizon                                  | interval   |
        | runway — current actions                 | daily      |
        | 10,000 ft — current projects             | weekly     |
        | 20,000 ft — current responsibilities     | monthly    |
        | 30,000 ft — 1-2 year goals               | quarterly  |
        | 40,000 ft — 3-5 year vision              | annually   |
        | 50,000 ft — career, purpose, lifestyle   | annually + |

    Scenario: Process what the review provokes
      When a higher-horizon review surfaces material
      Then each note is processed into trash, Someday/Maybe, or a project with a next action

    Scenario: Travel bottom-up
      Given the bottom is out of control
      When the user tries to manage from the top down
      Then it is recognized as the least effective approach
      And Ground and the Projects level are made current first

    Scenario: Sufficiency is self-defined
      Then the user reminds themselves of their commitments only as often as they need to
      And the review intervals are adjusted until the user trusts the system at every horizon
