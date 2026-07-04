Feature: Clarify
  Deciding what each captured item is, what done looks like, and what the
  next physical action is — one item at a time, to zero.

  @REC-CLA-01 @type-methodology @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: The user empties the inbox to zero, top item first
    Given the inbox contains unprocessed items
    When the user starts a processing session
    Then the tool presents one item at a time, top item first
    And the tool offers no affordance to skip ahead to easier items
    And the user can reach zero within their 24-48 hour cadence because each step is a decision, not a task

  @REC-CLA-02 @type-tool @strength-must @src-B1
  Scenario: An item never returns to the inbox undecided
    Given the user has picked up an inbox item
    When they finish with that item
    Then it has been dispatched to exactly one destination
    And "put it back in the inbox" is not among the offered choices

  @REC-CLA-03 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario Outline: The gating question routes each item
    Given the user is clarifying an item and has answered "what is it?"
    When they answer "is it actionable?" with "<answer>"
    Then the tool routes the item toward <destinations>

    Examples:
      | answer            | destinations                          |
      | yes               | do now / delegate / defer (the 3Ds)   |
      | no                | trash / incubate / reference          |
      | maybe             | incubate (maybe resolves to not-now)  |

  @REC-CLA-04 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: Actionable items get an outcome and a next physical action
    Given the user is clarifying the vague item "Mom"
    When they declare the outcome "Give Mom a great 60th party" and the next action "Draft invitee list"
    Then the tool records both, creating the project inline without leaving the processing flow
    And the next action is routed to its proper list

  @REC-CLA-05 @type-methodology @strength-should @src-B1 @src-B2
  Scenario: The clarity test sharpens a fuzzy action
    Given the user has written "deal with Dad" as a next action
    When they apply the clarity test (what has to happen first? what does doing look like? where does it happen?)
    Then the tool's prompts help them rewrite it as a concrete, physical, single action ("Call Roberta re: Dad")
    And the tool never accepts silently that vagueness is fine — but never blocks the user either

  @REC-CLA-06 @type-methodology @strength-should @src-B1 @src-B2
  Scenario: Hidden projects are extracted from fuzzy attention items
    Given the inbox holds a fuzzy concern like "Exercise more"
    When the user clarifies it
    Then the tool supports turning it into a project with a defined outcome and next action
    And an outcome may legitimately be "accept and close this" with no solution

  @REC-CLA-07 @type-methodology @strength-should @src-B2
  Scenario: Clarify is non-committal
    Given the user captured an idea they are unsure about
    When they clarify it
    Then dismissing, deferring, or dropping it are first-class outcomes
    And sensitive or emotional items flow through the same capture-outcome-next-action path as any other

  @REC-CLA-08 @type-methodology @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: The two-minute rule short-circuits tracking
    Given the user clarifies an item whose next action takes two minutes or less
    When they choose "do it now"
    Then the tool stands aside while they do it (optionally offering a timer)
    And the completed action is not stored on any list
    And if the parent outcome is not finished, the tool asks for the new next action and re-routes it

  @REC-CLA-09 @type-tool @strength-may @src-B1
  Scenario: The do-it-now threshold is configurable
    Given the user has a large processing window
    When they raise the do-it-now threshold from 2 to 10 minutes
    Then the clarify flow uses the new threshold for its "do it now" suggestion
    And the threshold can equally shrink to ~30 seconds for tight windows

  @REC-CLA-10 @type-methodology @strength-should @src-B1
  Scenario: Entries never degrade back into stuff
    Given an action list contains the entry "Johnny's birthday"
    When the user encounters it during clarify or review
    Then the tool supports re-clarifying it into a discrete physical next action
    And the system's guidance treats non-action entries on action lists as defects, not content

  @REC-CLA-11 @type-methodology @strength-may @src-B1
  Scenario: Broken agreements get an honest exit
    Given clarifying surfaces a commitment the user can no longer keep
    When they decide what to do with it
    Then the tool offers the three honest exits: renegotiate it, complete it, or consciously drop it
    And no agreement disappears from the system silently

  @REC-CLA-12 @type-tool @strength-should @src-B2
  Scenario: Daily clarifying is fast enough to sustain
    Given staying current costs roughly 30-90 minutes of processing per day
    When the user clarifies a typical item
    Then the per-item loop costs minimal keystrokes and forces no extra form-filling
    So that daily inbox-clearing fits the available budget
