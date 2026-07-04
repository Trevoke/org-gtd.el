# Canonical GTD workflows — area: PERSPECTIVE (the six Horizons of Focus)
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the six horizon conversations — each with its own question, scope, formats,
# and cadence — are canonical, promoted from an aside in book 1 to a co-equal
# half of the method.

Feature: Perspective — the six Horizons of Focus
  Control alone decays without perspective. Each horizon is a conversation
  with its own question, addressed in separate dedicated processes, usually
  bottom-up. Altitude merely represents the breadth of view, not importance.

  @WF-26 @cadence-monthly-plus @src-B2-W-17 @src-B1-W-27
  Rule: WF-26 — Perspective sequencing: one horizon at a time, usually bottom-up

    Background:
      Given relative control has been achieved and the question is where to put focus

    Scenario: One horizon at a time
      When horizon work is done
      Then each horizon is addressed in a separate, dedicated process
      And cross-horizon thoughts that surface are captured without changing the current focus

    Scenario: Default direction is bottom-up
      When the user begins horizon work
      Then the default order is runway first, then projects, then upward
      And the reasons hold: the mundane end is less daunting, control is easier to gain on the lower rungs, and subtle commitments are more attractive once implementation is trusted

    Scenario: Override — start where the attention is loudest
      Given a particular horizon most has the user's attention
      When sequencing is chosen
      Then work starts at that horizon, paying close attention to what most has the attention
      And top-down is right in special cases, such as aligning values before goals

    Scenario: Invariant — altitude is not importance
      Then a higher horizon merely represents a broader view
      And higher horizons serve as reference points for the priorities of lower ones
      And each level should align with the one above, with conflicts resolved upward

  @WF-27 @cadence-daily @src-B2-W-18
  Rule: WF-27 — Runway conversation: next actions

    Background:
      Given the runway horizon answers the question "What do you need to do?"

    Scenario: Scope and formats
      Then the scope is all the physical, visible actions the user can take, typically one hundred or more outstanding
      And the formats are the calendar for day- and time-specific items plus context-sorted action lists

    Scenario: How and when to engage
      Then the calendar is consulted whenever a where-or-when question arises
      And the action lists are consulted whenever there is discretionary time and the user wants to consider all options
      And this happens multiple times daily

    Scenario: Invariant — completeness is the lever
      Then only a complete runway inventory lets the user trust both the plan of attack and the actions they are not taking
      And the lower the horizon, the more sophisticated the system must be

    Scenario: Alignment litmus test
      When alignment at the runway is checked
      Then the test is whether there is consensus about the very next actions that need to be taken, by whom, and by when

  @WF-28 @cadence-weekly @src-B2-W-19 @src-B1-W-20
  Rule: WF-28 — Projects conversation (10,000 ft) and the hidden-project sweep

    Background:
      Given the projects horizon answers the question "What do I need to complete?"
      And the scope is outcomes finishable within a year that involve more than one action

    Scenario: Scope and formats
      Then the Projects list is a single index of thirty to one hundred entries, one per line
      And personal projects belong on the same list as professional ones
      And project plans and support material live separately from the list
      And a project is never done directly — only its actions are

    Scenario: Hidden-project sweep — three areas
      When the user suspects the inventory is incomplete
      Then current activities are scanned: calendar, action lists, and physical artifacts whose larger outcome is not yet a project
      And higher-horizon interests and commitments are reviewed for "look into" projects
      And current problems, issues, and opportunities are converted — "When is a problem a project? Always."

    Scenario: Trigger verbs to find projects
      When the user scans for unrecognized projects
      Then trigger verbs prompt them: Finalize, Implement, Research, Publish, Distribute, Maximize, Learn, Set up, Organize, Create, Design, Install, Repair, Submit, Handle, Resolve

    Scenario: Three distinct times to engage
      Then the user engages this horizon once a week in a regular one-to-two-hour executive session with themselves
      And whenever key projects seem to be lagging behind in keeping next actions current and in motion
      And whenever they feel they have lost their grip on priorities in the short term

    Scenario: Invariant — every project has a current next action
      Then every project on the list has a current next action in the system

  @WF-29 @cadence-monthly-plus @src-B2-W-20
  Rule: WF-29 — Areas of Focus conversation (20,000 ft)

    Background:
      Given the areas horizon answers the question "What do I need to maintain?"
      And its contents are not meant to be finished — they generate projects and actions

    Scenario: Scope and formats
      Then the inventory is roughly ten to fifteen areas of responsibility and interest
      And the job contributes about four to seven areas and life about seven to ten
      And formats include a high-level job description and a personal lifestyle checklist

    Scenario: Derivation move — bottom-up
      When the user examines an existing project or action
      And asks "Why am I doing that? What area of interest or responsibility does it reflect?"
      Then the area inventory is completed from the answers

    Scenario: Generation move — top-down
      Given an area the user wishes they engaged more
      When the user asks "what project, if implemented or completed, would automatically get you doing more of what you want to be doing?"
      Then the answer becomes a new project, such as "Set up a regular exercise routine"

    Scenario: When to engage
      Then the high-level checklist is revisited every month or so
      And on any significant job or role change
      And whenever life areas feel out of balance

  @WF-30 @cadence-monthly-plus @src-B2-W-21
  Rule: WF-30 — Goals and Objectives conversation (30,000 ft)

    Background:
      Given the goals horizon answers the question "What do I want to achieve?"
      And the scope is completable outcomes over the next year or two

    Scenario: Sorting rule — project or goal by review frequency
      When classifying a commitment
      Then if its status really needs checking weekly, it is a project
      And if reassessing monthly or quarterly is honestly enough, it is a goal

    Scenario: The annual ritual
      When the year turns
      Then about thirty minutes are spent inventorying everything accomplished and noteworthy that year
      And about thirty minutes asking what should be on that list at the end of the following year
      And the answers are captured as the new goals list

    Scenario: When reassessment is demanded
      Then goals are rethought at least once a year, with monthly or quarterly recalibration
      And reassessment is forced when old goals have been overrun and not reset
      And when commitment to an ambitious long-term vision is having trouble connecting to reality
      And ambitious goals are not set while in survival mode

  @WF-31 @cadence-monthly-plus @src-B2-W-22
  Rule: WF-31 — Vision conversation (40,000 ft)

    Background:
      Given the vision horizon answers the question "What would long-term success look, sound, and feel like?"
      And the time frame is three to five years or more

    Scenario: Elicitation
      When vision is elicited
      Then questions used include "So what do you see yourself doing five years from now?"
      And "What is the biggest and best thing you can imagine for yourself?" with a best-guess timeline
      And formats include ideal-scene lists, scripted scenarios, as-if future articles, and treasure maps

    Scenario: Invariant — the 51 percent credibility rule
      Then the held image must be at least 51 percent credible to achieve stickiness in the psyche
      And it is ambitious but believable

    Scenario: Linkage — reverse-engineer the vision
      When a vision is held
      Then it is reverse-engineered back to short-term goals, which create projects, which trigger next actions
      And the cascade questions are "What's the outcome? What's the next action?"
      And alternatively the vision may simply be held until it reaches critical mass

    Scenario: When to engage
      Then vision is revisited regularly, often annually or every two to three years
      And on any major transitional event: kids leaving the nest, death, divorce, a new partner, an inheritance, a surprising job offer, an unexpected illness
      And whenever near-term goal alignment is stuck, since clarity on the longer future unlocks agreement about the nearer one

  @WF-32 @cadence-monthly-plus @src-B2-W-23
  Rule: WF-32 — Purpose and Principles conversation (50,000 ft)

    Background:
      Given the top horizon answers the questions "Why am I (are we)?" and "How am I (are we)?"

    Scenario: Purpose — when to ask why
      Then "Why?" is engaged at the very start of an endeavor
      And when the user's involvement is unclear
      And when resource-allocation conflicts arise over limited budget or time
      And when all else associated with an endeavor fails, the best question to ask is "Why am I (are we) doing this?"
      And purpose statements are not forced prematurely — they typically take years to articulate

    Scenario: Principles — elicitation
      When principles are elicited
      Then questions used include "What would have to be true about a situation for you not to really care where you worked or what you were doing?"
      And "You wouldn't care where you live as long as…?"
      And for a group, "We are at our best when what's true?"

    Scenario: Principles — when to engage
      Then principles are surfaced at the front end of relationships and ventures — mergers, partnerships, new hires
      And when violations surface, since people often become aware of their values only when someone else violates them

    Scenario: Invariant — the reference point for tough choices
      Then tough choices are tested against "Is this decision really in keeping with my purpose? Does it line up with what I consider really important?"

  @WF-33 @cadence-monthly-plus @src-B2-W-24
  Rule: WF-33 — Category corral: control then perspective on one runaway category

    Background:
      Given a single category of life has sprawled everywhere, such as reading material

    Scenario: Control first
      When the user corrals the category
      Then one labeled location is dedicated to it
      And every single item in the user's universe that fits the description is collected there, neglecting nothing

    Scenario: Then perspective
      Given the category is corralled
      When the user interrogates its contents
      Then questions like "What, of all the things you've captured that you think you ought to read, should you really be reading?" are asked
      And subscriptions and inputs are cancelled or added based on current aspirations

    Scenario: Invariant — the ordering rule
      Then asking the perspective question before gaining control is wasted effort
      And skipping the perspective step lets the category sprawl again
