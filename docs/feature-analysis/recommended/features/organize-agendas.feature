Feature: Organize - Agendas (per-person / per-meeting)
  Topics queued for specific people and meetings.

  @REC-AGE-01 @type-tool @strength-should @src-B1 @src-B2 @src-DA94
  Scenario: Topics queue on a per-person agenda
    Given the user thinks of something to discuss with Sam
    When they add it to the "Sam" agenda
    Then it joins that person's talk-to list, addable ad hoc at any time
    And when the user is with Sam, the full agenda is reviewable in one place
    And agenda lists stay distinct from Waiting For
    And a list may be time-limited (e.g. a contractor for one project's duration)

  @REC-AGE-02 @type-tool @strength-may @src-B1 @src-DA94
  Scenario: A person view aggregates the whole relationship
    Given the user is about to meet a colleague
    When they open that person's view
    Then they see: discussion agendas, waiting-fors owed to me and owed by me, which projects/areas the person is a resource for, ongoing interest points, and tickler-fed special dates
    And the view can be printed or presented as a pre-meeting brief
    And notes taken during the meeting route back to the inbox for processing

  @REC-AGE-03 @type-tool @strength-may @src-DA94
  Scenario: A meeting lives through its lifecycle
    Given the user creates an upcoming meeting (typed, project-tied, with participants)
    When they prepare it
    Then a structured agenda holds: info to give, info to get, options to explore, choices to make, set next meeting?
    And after the meeting occurs, per-topic decisions and next actions (who / when / due) are recorded
    And the meeting's outputs are sent to the inbox for clarification
