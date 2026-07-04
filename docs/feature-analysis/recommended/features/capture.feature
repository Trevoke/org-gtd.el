Feature: Capture
  Getting every open loop out of the user's head and into one trusted place,
  with zero friction and zero judgment.

  @REC-CAP-01 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: Any open loop is captured without judgment
    Given the user has a thought competing for their attention, personal or professional, big or small
    When they invoke capture and type the thought
    Then the item lands in the single trusted inbox exactly as entered
    And the tool requires no category, priority, or value judgment at capture time

  @REC-CAP-02 @type-tool @strength-must @src-B1 @src-B2
  Scenario: All inputs funnel into a leakproof set of inboxes
    Given the user has inputs arriving from multiple channels
    When each input is captured
    Then it reaches the one canonical inbox target
    And no input is left stranded outside the system
    And an email subsystem may serve as its own contained inbox without breaking the funnel

  @REC-CAP-03 @type-tool @strength-must @src-B1 @src-B2 @src-DA94
  Scenario: Capture is available in every context
    Given the user is in the middle of any other activity
    When a thought occurs
    Then a capture entry point is reachable immediately, without leaving their current work
    And any file, location, or activity can be tagged into the inbox for later closure

  @REC-CAP-04 @type-tool @strength-must @src-B1 @src-B2
  Scenario: Each captured thought is a discrete placeholder
    Given the user captures three unrelated thoughts in one sitting
    When the capture session ends
    Then the inbox contains three separate items, one heading each
    And an item may stand in textually for a physical or awkward thing ("Purge boat shed")

  @REC-CAP-05 @type-tool @strength-must @src-B1 @src-DA94
  Scenario: Items are date-stamped automatically at capture
    Given the user captures an item
    When the item is created
    Then it carries an automatic creation timestamp without user action
    And that timestamp is available later for waiting-for follow-up timing

  @REC-CAP-06 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario: A guided mind sweep empties the user's head
    Given the user feels overwhelmed or wants to get current
    When they invoke the mind-sweep action
    Then the tool walks them through trigger prompts (quick list, full Incompletion Trigger List, the six Horizons)
    And every thought prompted is captured as its own inbox item
    And the sweep can be invoked at any time, not only during review

  @REC-CAP-07 @type-tool @strength-should @src-DA94
  Scenario: Capture and clarify are decoupled
    Given the user has just finished entering items into a capture session
    When the session ends
    Then the tool offers "process now" and "add to inbox" as distinct choices
    And choosing "add to inbox" never forces immediate processing

  @REC-CAP-08 @type-tool @strength-may @src-DA94
  Scenario: Activity boundaries prompt a sweep
    Given the user has just finished a phone call or work session
    When the activity ends
    Then the tool may prompt "anything to process?"
    And anything entered goes straight to the inbox

  @REC-CAP-09 @type-methodology @strength-may @src-B2
  Scenario: The user runs a current-reality and distractions inventory
    Given the user wants to know "what's true right now?"
    When they run the themed inventory sweep
    Then the tool scaffolds capture of current realities and distractions as inbox items
    And the practice is repeatable on demand, distinct from the general trigger-list sweep
