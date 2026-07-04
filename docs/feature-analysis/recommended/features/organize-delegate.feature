Feature: Organize - Delegate and Waiting For
  Handing work to the right person and tracking what others owe.

  @REC-DEL-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: An action is delegated to the right entity
    Given the user clarifies an action they are not the right person to do
    When they delegate it (down, sideways, or up)
    Then the action is recorded as handed to that person or entity

  @REC-DEL-02 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: A delegated item becomes a Waiting For entry
    Given the user delegates an action or awaits a deliverable they care about
    When the handoff happens
    Then a Waiting For entry records who has it, the date requested, and any due date
    And the Waiting For list contains only deliverables others owe the user, never the user's own steps

  @REC-DEL-03 @type-tool @strength-should @src-B2
  Scenario: A whole project is delegated
    Given the user has a decomposed project that someone else will now own
    When they delegate the entire project
    Then it collapses to a single Waiting For (who / when / status) on the user's side
    And the task breakdown is preserved for the project's possible return
    And an optional "outcomes I'm waiting on from others" list holds such delegated outcomes

  @REC-DEL-04 @type-tool @strength-may @src-DA94
  Scenario: Delegation passes through two states
    Given the user decides someone else should do an action but has not yet handed it off
    When they mark it "plan to delegate to <person>"
    Then the item is distinguishable from one actually "delegated to <person> on <date>"
    And the actual handoff records its date

  @REC-DEL-05 @type-methodology @strength-may @src-B1
  Scenario: The user prefers trackable delegation channels
    Given the user is about to delegate an action
    When they choose a channel
    Then the tool's guidance favors channels that leave a record (email over note over voice over face-to-face)
    And the tool never blocks any channel the user prefers
