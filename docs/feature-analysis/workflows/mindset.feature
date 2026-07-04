# Canonical GTD workflows — area: MINDSET (master heuristics and standards)
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the Control + Perspective master frame and the Matrix of Self-Management
# are canonical and new in book 2.

Feature: Mindset — the master heuristics that drive everything else
  The flows above are mechanics; these are the stances that select among
  them: get control and get perspective, start with what's true, keep
  self-agreements visible, and end every interaction with a next action.

  @WF-37 @cadence-per-item @src-B2-W-01
  Rule: WF-37 — Appropriate Engagement: the master Control + Perspective loop

    Background:
      Given the user feels "off" instead of "on" — out of control, lacking direction, or both
      And this applies from cooking dinner to running a company

    Scenario: There are only two things to do
      When the user is off-balance
      Then the remedy is to get control and to clarify objectives at the appropriate horizon, in either order

    Scenario: Apply the right half of the model
      Given something is out of control
      Then the five stages — capture, clarify, organize, reflect, engage — are applied to it
      Given something lacks direction
      Then the six Horizons of Focus are applied to it

    Scenario: Invariant — the weakest link
      Then productivity is only as good as the weakest link in the capture-clarify-organize-reflect-engage chain

    Scenario: Don't aim for permanent perfection
      Then the trick is not preventing unsettled states but shortening the time spent in them
      And the loop is: capture, clarify, and organize what you can, have the conversations you need to have, get moving, course-correct, and get going again — ad infinitum

    Scenario: The end state
      Then "work" means anything you want to get done that's not done yet
      And what you're doing at any point in time is what you think you should be doing

  @WF-38 @cadence-monthly-plus @src-B2-W-02
  Rule: WF-38 — Matrix of Self-Management self-diagnosis

    Background:
      Given the user wants to assess their own standing on the control and perspective axes, as a guide for improvement

    Scenario: Place yourself on the matrix
      When the user self-assesses on low/high control crossed with low/high perspective
      Then the quadrant names its syndrome:
        | control | perspective | syndrome     | positive aspect       |
        | low     | low         | Victim       | Responder             |
        | high    | low         | Micromanager | Implementer           |
        | low     | high        | Crazy Maker  | Visionary             |
        | high    | high        | Captain and Commander | Captain and Commander |

    Scenario: The matrix is relative, situational, and fluid
      Then the user can occupy different quadrants per area and per horizon
      And can cycle through all four in a day
      And the labels are warnings for a course correction, not identities

    Scenario: Acceptance, then strategy
      When a quadrant is identified
      Then the first step in improving what's going on is acceptance of what is going on
      And needing more control routes to the five stages
      And needing more perspective routes to the horizons
      And possibly both

  @WF-39 @cadence-per-item @src-B2-W-03
  Rule: WF-39 — "What's true right now?": start with what has your attention

    Background:
      Given the user does not know where to start, or is trying to set priorities while loose ends pull at them

    Scenario: Begin with where you are
      When the user is asked "Okay, so what's true right now?"
      Then they begin with where they are, not with idealized priorities or strategy
      And most people start with something like "Fix printer", not "Fulfill my destiny on the planet"

    Scenario: Capture what surfaces, large or small
      When blips surface
      Then each is captured, because the process of dealing with them is identical for the small and the large

    Scenario: Invariant — the attention heuristic
      Then what usually most needs your attention is what most has your attention
      And if you don't pay attention to what has your attention, it will take more of your attention than it deserves
      And dealing with the current inventory clears the way for recognizing the bigger stuff

  @WF-40 @cadence-per-item @src-B1-W-32
  Rule: WF-40 — Managing self-agreements (don't make / complete / renegotiate)

    Background:
      Given every captured "should" is an agreement with oneself
      And a broken self-agreement erodes self-trust

    Scenario: Three and only three ways to silence the pressure
      When an internal commitment causes pressure or capacity is exceeded
      Then the user takes exactly one of three paths:
        | path                      | example                                          |
        | don't make the agreement  | say no, toss it, move it to Someday/Maybe         |
        | complete the agreement    | do it, for instance via the two-minute rule       |
        | renegotiate the agreement | consciously change it                             |
      And "A renegotiated agreement is not a broken one"

    Scenario: Invariant — renegotiation requires visibility
      Then it is impossible to renegotiate agreements with yourself that you can't remember you made
      And therefore 100 percent capture and regular review are prerequisites
      And renegotiation happens naturally each time the lists are reviewed

  @WF-41 @cadence-per-item @src-B1-W-33 @src-DA94-W06
  Rule: WF-41 — "What's the next action?" as a closure standard

    Background:
      Given a discussion point, meeting, or shared decision is concluding

    Scenario: Ask the closure question
      When each discussion point ends
      Then someone asks "So what's the next action on this?"
      And the answer and its owner are recorded into the system

    Scenario: Meeting lifecycle closure
      Given a meeting has occurred
      When its outcomes are recorded
      Then each topic gets its decisions noted
      And each next action gets a who, a when, and any due date
      And the meeting outputs are sent to the in-basket for processing

    Scenario: Invariant — nothing ends unowned
      Then no interaction ends with an ambiguous "we should…" left unowned
