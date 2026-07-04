# Canonical GTD workflows — stage: CLARIFY
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the canonical stage name is "clarify" (not "process"), and "Maybe" is
# explicitly defined as "no, but the item might require action later".

Feature: Clarify — deciding what each captured item is and means
  The per-item decision tree that empties "in": identify the item, decide
  actionability, define outcomes and next actions, and dispatch every item
  into exactly one destination. Nothing ever goes back into "in".

  @WF-06 @cadence-per-item @src-B1-W-06 @src-B2-W-07 @src-B2-W-14 @src-DA94-W02 @src-DA94-W09
  Rule: WF-06 — The per-item clarifying decision tree (the core algorithm)

    Background:
      Given the user is processing a single item taken from "in"
      And items are worked one at a time from a single bucket

    Scenario: Identify the item
      When the user is asked "What is it?"
      Then the user determines what the item actually is and what it means to them

    Scenario: Decide actionability
      When the user is asked "Is it actionable?"
      Then there are two possible answers: yes and no
      And "Maybe" is actually "no, but the item might require action later"

    Scenario: Not actionable — exactly one of three fates
      Given the answer to "Is it actionable?" is no
      Then the item is dispatched to exactly one of:
        | fate      | meaning                                                              |
        | trash     | meaningless now — toss, shred, recycle                               |
        | incubate  | nothing to do now, but maybe later — Someday/Maybe or a dated trigger |
        | reference | no action, but value as information — file it immediately            |
      And either policy "When in doubt, throw it out" or "When in doubt, keep it" is acceptable

    Scenario: Actionable — determine outcome and next action
      Given the answer to "Is it actionable?" is yes
      When the user is asked "What's my desired outcome? What am I committed to accomplishing or finishing about this?"
      And the user is asked "What's the next action? What's the next thing I need to do to move toward that goal?"
      Then if more than one action step is needed to close the loop, the outcome is captured on the Projects list
      And the project entry can be created inline without leaving the clarifying flow

    Scenario: The next action must be a physical, visible activity
      When a next action is defined
      Then it is the next physical, visible activity that moves current reality toward completion
      And "set meeting" is not a next action, while "e-mail X to propose times" is
      And if the needed action is "decide", the next action is the physical activity that enables the decision

    Scenario: The three-question specificity test
      When the user checks a defined next action against:
        | question                     |
        | "What has to happen first?"  |
        | "What does doing look like?" |
        | "Where does it happen?"      |
      And the user cannot answer all three specifically
      Then there is still clarifying work to do on that action

    Scenario: Route the decided action — do, delegate, or defer
      Given a next action has been decided
      Then the user does it now if it takes two minutes or less
      And otherwise delegates it if someone else is the right person to do it
      And otherwise defers it onto the appropriate context list or the calendar

    Scenario: Invariant — every item leaves "in" through exactly one door
      Then the item ends in exactly one of: trash, Someday/Maybe, a dated tickler or calendar trigger, reference, done, Waiting For, the calendar, or a next-action list
      And a Projects-list entry exists in addition if the item is a multi-step outcome
      And the item never returns to "in"

  @WF-07 @cadence-daily @src-B1-W-07 @src-DA94-W09
  Rule: WF-07 — Processing-session discipline (getting "in" to empty)

    Background:
      Given the user sits down to process an in-tray or inbox to zero
      And emptying "in" means deciding what each item is, not doing all the actions

    Scenario: Process the top item first
      When the user begins
      Then the top item is taken first, with no cherry-picking
      And emergency scanning is allowed only as a separate, named activity that is not processing

    Scenario: Process one item at a time
      When the user works the stack
      Then only one item is in hand at a time
      And no decision about an item is avoided for more than a minute or two

    Scenario: Never put anything back into "in"
      When an item has been picked up
      Then the user decides what to do about it and where it goes the first time
      And nothing is ever put back into "in"

    Scenario: Large e-mail backlogs go last-in-first-out
      Given a large e-mail backlog
      When the user processes it
      Then last-in-first-out order is preferred because of discussion threads
      And otherwise LIFO versus FIFO does not matter if the bottom is reached in a reasonable period

    Scenario: Session done
      Then "in" is empty
      And every item has gone through the clarifying decision tree
      And "in" reaches empty at least every day or two

  @WF-08 @cadence-per-item @src-B1-W-08 @src-B2-W-14 @src-DA94-W02
  Rule: WF-08 — The two-minute rule

    Background:
      Given a next action has just been decided during clarifying

    Scenario: Do it now if it fits in two minutes
      When the user estimates the action can be done in two minutes or less
      Then the user does it when they first pick the item up, even if it is not high priority
      And the rationale is that storing and tracking it would take longer than doing it
      And a timer or alarm may bound the two minutes

    Scenario: Completing the loop or chaining the next action
      When the two-minute action completes the loop
      Then the item is done and nothing about it is tracked
      When the two-minute action does not complete the loop
      Then the next action on it is clarified and done, delegated, or deferred by the same criteria

    Scenario: The cutoff is a guideline
      Then the cutoff may extend to five or ten minutes with a long open window
      And shorten toward thirty seconds when time is tight
      And the rule applies primarily when engaging new input, not as a way to spend the day hunting two-minute actions

    Scenario: Invariant — sub-two-minute actions are executed, never tracked
      Then no action of two minutes or less appears on any list

  @WF-09 @cadence-per-item @src-B1-W-09 @src-B2-W-14 @src-DA94-W05
  Rule: WF-09 — Delegation handoff and tracking

    Background:
      Given a decided next action takes more than two minutes
      And the user asks "Am I the best person to be doing it?" and the answer is no
      And delegation may go down, sideways, or up

    Scenario: Hand it off through systematic channels
      When the user delegates the action
      Then the handoff uses, in preference order: e-mail; a written note routed with the item; text or voice mail; an Agenda-list item for the next real-time conversation; direct real-time interruption as a last resort

    Scenario: Track the handoff on Waiting For
      When the handoff is made
      Then the item is recorded on the Waiting For list if the user cares whether it happens
      And the date of the handoff is recorded on the entry
      And any agreed due date is recorded on the entry
      And nothing about the item remains in "in" or in the head

    Scenario: Sending a communication can spawn its own tracking
      When a delegating communication is sent
      Then a Waiting For entry may be created from the communication in the same step
      When the communication is left unfinished
      Then a next action to finish it is created automatically, such as "Finish e-mail to X"

    Scenario: Track balls already in others' courts
      Given the ball is already with someone else
      Then it is recorded the same way, as "Waiting for X from Y", with the date

  @WF-10 @cadence-per-item @src-B1-W-10 @src-B2-W-08
  Rule: WF-10 — Outcome reframing and project identification

    Background:
      Given captured items include topics, problems, or vague intentions rather than completable commitments

    Scenario: Rename topics as completable outcomes
      When the user clarifies a topic such as "Mom"
      Then it is reframed as a concrete goal such as "Give mom a great sixtieth birthday party"
      And the next action is then derived from the outcome

    Scenario: Commit dilemmas to resolution, not to an answer
      Given an unresolved dilemma
      When the user names the project
      Then the commitment is to resolution, such as "Resolve situation with Carolyn Jones", before any answer is chosen

    Scenario: Recognize research and process projects
      When a commitment exists to make a decision
      Then a research project exists as soon as the commitment to decide exists, named "R&D ..." or "Look into ..."
      When a fuzzy intention is recurrent, like "exercise" or "pay bills"
      Then it is reframed as a process project such as "Set up an exercise routine" or "Set up my electronic bill-paying system"

    Scenario: Sweep generated actions for incomplete commitments
      Given a processing pass has generated single actions
      When the user scans them for actions that will not finish the commitment
      Then each such larger outcome is written on the Projects list

    Scenario: Invariant — the Projects list is placeholders, not priorities
      Then the Projects list exists to hold a placeholder for every multi-step open loop achievable within a year
      And the list is not ordered by size or priority
      And thirty to one hundred entries are expected
