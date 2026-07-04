# Canonical GTD workflows — area: PLANNING (vertical focus on a single project)
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the raise/lower-focus steering rule and the off-your-mind sufficiency test
# are book 2's crisper formulations.

Feature: Planning — the Natural Planning Model
  How the mind naturally plans: purpose, vision, brainstorm, organize, next
  actions. A project gets exactly as much planning as it takes to get it off
  the user's mind — no more.

  @WF-34 @cadence-per-item @src-B1-W-28 @src-B2-W-25 @src-B1-W-31 @src-DA94-W03
  Rule: WF-34 — Natural planning model (five phases, calibrated per project)

    Background:
      Given a project needs more clarity or shape than its Projects-list entry and next action provide

    Scenario: Phase 1 — defining purpose and principles
      When the user asks "Why is this being done? What would 'on purpose' really mean?"
      Then purpose defines success, creates decision-making criteria, aligns resources, motivates, clarifies focus, and expands options
      And principles are elicited with "I would give others totally free rein to do this as long as they…"

    Scenario: Phase 2 — outcome visioning
      When the user asks "What would it be like if it were totally successful? How would I know?"
      Then what wild success looks, sounds, and feels like is pictured
      And the principle holds: you won't see how to do it until you see yourself doing it

    Scenario: Phase 3 — brainstorming
      When ideas about the project are captured externally
      Then the rules apply: don't judge, challenge, evaluate, or criticize; go for quantity, not quality; put analysis and organization in the background
      And a project planning trigger list may support the brainstorm
      And a timed free-form download may bound the session

    Scenario: Phase 3b — triage the brainstormed notes
      When the notes are processed one at a time
      Then each is classified as a to-do with its own sub-outcome and next action, a resource, data, or an option
      And each is then activated, incubated, or eliminated

    Scenario: Phase 4 — organizing
      When enough ideas exist
      Then the significant pieces are identified
      And they are sorted by components, sequences, and priorities, detailed only to the required degree

    Scenario: Phase 5 — identifying next actions
      Then next actions are decided for each current independent moving part of the project
      And if more planning is required, the next action is the action that gets that planning to happen

    Scenario: Anti-pattern — the unnatural planning model
      Given planning starts with "who's got a good idea?" or with action-first crisis mode
      Then it is recognized as the reactive, unnatural model working backwards

    Scenario: Calibration — how much planning per project
      Then about 80 percent of projects need only the outcome and a next action, with the model run informally in the head
      And about 15 percent need at least some external brainstorming
      And about 5 percent need deliberate application of one or more of the five phases
      And the selection test for more planning is: the project still has the user's attention after next actions are defined, or ideas about it keep showing up ad hoc

    Scenario: Invariant — the sufficiency test
      Then if the project is off the user's mind, planning is sufficient
      And if it is still on the user's mind, more planning is needed

  @WF-35 @cadence-per-item @src-B1-W-29 @src-B2-W-25
  Rule: WF-35 — Unsticking a project (raise or lower the level of focus)

    Background:
      Given a project is stuck — either unclear or not moving

    Scenario: The steering rule
      Then if the project needs more clarity, the user raises the level of focus
      And if the project needs more to be happening, the user lowers the level of focus

    Scenario: Shifting up for clarity
      Given busy-but-confused action
      When the user shifts up the model
      Then they pull out or create the plan
      And if the plan is untrusted, they brainstorm more
      And if the brainstorm is fuzzy, they return to the outcome vision asking "What would the outcome look like?"
      And if the vision is unclear, they return to purpose: "why are you engaged in this at all?"

    Scenario: Moving down for motion
      Given enthusiasm about purpose but no picture
      When the user moves down the model
      Then they define the vision
      And if stuck after that, they brainstorm the hows
      And with ideas but no decisions, they organize and decide mission-critical deliverables
      And with a plan but no traction, they ask "What's the next action, and who's got it?" per component

    Scenario: Done when off the mind and moving
      Then the project is off the user's mind and in motion

  @WF-36 @cadence-weekly @src-B1-W-30 @src-B2-W-25
  Rule: WF-36 — Activating a project's moving parts

    Background:
      Given the user is verifying a project is sufficiently planned for implementation

    Scenario: The sufficiently-planned test
      Then a project is sufficiently planned when every next-action step has been decided on every front that can be moved on without some other component completing first

    Scenario: Interrogate each component
      When the user asks of each component "Is there something that anyone could be doing on this right now?"
      Then components blocked by dependencies get no action yet
      And independently movable components each get a next action

    Scenario: Planning itself can be the next action
      Given more planning is needed before the project can move
      Then the next action is a process action, such as "Draft more ideas" or "Set up planning meeting"

    Scenario: Someone else's action
      Given the next action on a component belongs to someone else
      Then whose action it is gets clarified
      And it is tracked on the Waiting For list

    Scenario: Invariant — one next action per movable front
      Then every independently movable front has exactly one current next action or a tracked Waiting For entry
      And the Weekly Review's project pass restores this invariant weekly
