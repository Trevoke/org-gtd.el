Feature: Horizons of Focus
  Perspective above the runway: areas, goals, vision, purpose.

  @REC-HOR-01 @type-tool @strength-must @src-B1 @src-B2
  Scenario: Areas of Focus are listed and reviewed
    Given the user defines their ongoing responsibilities (health, finances, family, job hats)
    When they maintain the Areas of Focus checklist
    Then it holds roughly 7-15 areas (fewer than twenty; one list for job plus one for life suffices)
    And areas are never "finished" — they are reviewed on a ~monthly cadence

  @REC-HOR-02 @type-methodology @strength-should @src-B1 @src-B2
  Scenario: Areas reveal gaps and imbalance
    Given the user reviews projects against the Areas of Focus
    When an area has no project reflecting it
    Then the gap is visible and the user can frame a missing commitment
    And a vague area theme ("staff development") can be converted into a concrete project with next actions

  @REC-HOR-03 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Goals live at 30k feet
    Given the user commits to an outcome 1-2 years out
    When they record it as a Goal
    Then it is stored at the Goals horizon, reviewed monthly to quarterly
    And any project likely to take more than a year parks here instead of the Projects list

  @REC-HOR-04 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Vision is a stored picture of success
    Given the user articulates what success looks like 3-5 years out
    When they store the vision
    Then free-form text, scenarios, or treasure maps (linked images/files) are all supported
    And the vision is revisited annually or on major transitions, optionally re-surfaced by tickler
    And it supports reverse-engineering: vision to goals to projects to actions

  @REC-HOR-05 @type-tool @strength-should @src-B1 @src-B2
  Scenario: Purpose and principles anchor the system
    Given the user writes purpose statement(s) and core values / credo / affirmations
    When stored at the 50k horizon
    Then they serve as the top criterion for priorities
    And are reaffirmed periodically (annually or longer)

  @REC-HOR-06 @type-tool @strength-may @src-B1 @src-B2
  Scenario: Personal standards inform tough choices
    Given the user faces a hard decision
    When they consult their stored standards/values document
    Then it serves as decision-support reference ("Is this in keeping with my purpose?")
    And it is never actionable content

  @REC-HOR-07 @type-tool @strength-may @src-B2 @src-DA94
  Scenario: An optional overview spans the horizons
    Given the user wants to see the relationships between levels
    When they open the "overview of my life" view
    Then purpose, principles, vision, goals, areas, projects, and next actions appear linked in one outline
    And the capability is optional — diligent per-horizon review substitutes fully where it is unused
    And horizon contents remain mutable, recast over time

  @REC-HOR-08 @type-methodology @strength-should @src-B1 @src-B2
  Scenario: Priorities flow top-down while control builds bottom-up
    Given the user has clarified multiple horizons
    When they judge priorities
    Then the judgment flows top-down (purpose, values, vision, goals, areas, projects, actions)
    And the tool offers no ABC/123 priority codes as a substitute
    And each horizon remains equally important to clarify
