# Canonical GTD workflows — stage: SETUP
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins.

Feature: Setup — installing the GTD system
  The one-time flow that stands up the system: dedicated time, a dedicated
  workspace, processing tools, and a working reference filing system, all in
  place before any capturing or clarifying begins.

  @WF-01 @cadence-once @src-B1-W-01
  Rule: WF-01 — Initial full-scale implementation (time, space, tools)

    Background:
      Given the user has decided to install the GTD system from scratch

    Scenario: Block out a contained period of dedicated time
      When the user schedules the implementation
      Then the recommended allocation is two whole days, back to back, with interruptions blocked
      And capturing alone is budgeted at one to six hours and clarifying at about eight more
      And partial implementation is permitted but one contained period is strongly preferred

    Scenario: Set up a dedicated, unshared workspace
      When the user prepares the physical setting
      Then there is a dedicated workspace with at minimum a writing surface and an in-tray
      And any satellite location (home, in transit) mirrors the same system
      And no workspace is shared with another person

    Scenario: Acquire the basic processing tools
      When the user gathers tooling
      Then the workspace holds at least three trays, plain paper, pen, folders, labeler, a calendar, and wastebasket or recycle bins
      And whatever capture tools are already in use are kept in the funnel

    Scenario: The filing system must exist before processing starts
      Given the general-reference filing system is not yet in place
      When the user attempts to begin processing "in"
      Then the user is directed to establish the reference filing system first
      And the filing system meets the less-than-sixty-second filing standard

    Scenario: Clear the decks
      Given a commitment exists that would interrupt the implementation session
      When the user prepares to start
      Then the commitment is either handled now or parked with a reminder in "in"
      And no competing commitment is left pulling on attention

    Scenario: Setup is complete
      Then dedicated time is blocked
      And the workspace is functional and tools are at hand
      And the filing system is ready
      And the user is ready to gather all open loops into one place
