Feature: Organize - Next Actions
  Deferred actions parked in a trusted place, surfaced when relevant.

  @REC-NXT-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: A deferred action is parked in a trusted place
    Given the user clarifies an actionable item taking more than two minutes that only they can do
    When they defer it
    Then it lands on the Next Actions list (optionally context-tagged)
    And the user can stop thinking about it, trusting it will surface when relevant

  @REC-NXT-02 @type-tool @strength-may @src-B1 @src-DA94
  Scenario: An action carries what is needed to do it
    Given the next action is "Call Fred re: garage"
    When the user files it
    Then the action can carry the phone number so no lookup interrupts doing
    And an Errands action can carry a sublist of items (with optional anyday/weekend tagging)

  @REC-NXT-03 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Action lists carry no priority scaffolding
    Given the user views any action list
    Then the tool offers no ABC/123/high-medium-low priority coding on entries
    And no daily-priority re-sorting ritual is required or suggested
    Because prioritizing happens intuitively against the whole inventory at engage time

  @REC-NXT-04 @type-tool @strength-may @src-B1
  Scenario: An optional "if I have time" list
    Given the user wants a light short-list for the day
    When they pick a few items from Next Actions
    Then the tool can hold them as an informal "if I have time I'd like to..." list
    And that list is kept visibly distinct from calendar have-tos

  @REC-NXT-05 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Longer reading goes to a Read/Review queue
    Given the user clarifies an article that takes more than two minutes to read
    When they organize it
    Then it joins a Read/Review queue for low-attention or spare-time windows
    And the queue stays distinct from stored Reference material
    And the queue is self-regulating — the user prunes it by urge when it bloats
