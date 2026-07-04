# Canonical GTD workflows — stage: CAPTURE
# Sources: book1-workflows.md (B1-W), book2-workflows.md (B2-W), da-software-1994.md (DA94-W)
# Where the books differ, book 2 (Making It All Work, 2008) wins:
# the canonical stage name is "capture" (not "collect").

Feature: Capture — getting everything out of the head and into "in"
  Everything potentially meaningful goes into a small number of trusted
  external buckets, one placeholder per item. Capturing and clarifying are
  different mindsets and are always done separately.

  @WF-02 @cadence-once @src-B1-W-03 @src-B2-W-04
  Rule: WF-02 — Initial physical gathering ("corralling your stuff")

    Background:
      Given the user is starting implementation, after setup is complete
      And the goal is that everything that does not permanently belong where it is, the way it is, goes into "in"

    Scenario: Sweep the physical environment into "in"
      When the user searches the physical environment
      Then anything that doesn't permanently belong where it is, the way it is, is put in the in-tray
      And the locations are worked in order: desktop, desk drawers, countertops, inside cabinets, floors, walls and shelves, equipment, then other locations

    Scenario: Apply the stays-put test
      When the user evaluates an object in place
      Then it remains in place without action only if it is supplies, reference material, decoration, or equipment
      And it remains only if it is exactly where and how it should be
      And otherwise it goes into "in"

    Scenario: Let the owner make the attention call
      When the user considers an ambiguous object
      And the user is asked "Do you have any attention on this?"
      Then any item with attention on it goes into "in"

    Scenario: Handle oversized items, trash, and the purge urge
      When an item is too big for the in-tray
      Then a representative note is written on a sheet of paper, dated, and put in "in"
      When an item is obvious trash
      Then it is tossed immediately
      When the user is unsure what an item is
      Then it goes into "in" to be decided later, because capturing and clarifying are done separately
      When an area needs cleanup
      Then the user resists organizing it now and instead puts a note such as "Purge four-drawer cabinet" into "in"
      And existing lists and organizers are treated as items still to be processed and go into "in"

    Scenario: Gathering is complete
      Then every physical nook holds only supplies, reference, decoration, or equipment that are where and how they should be
      And everything else, or a note representing it, is in "in"
      And nothing has been kept or discarded for minimalism's sake — only embedded actions matter

  @WF-03 @cadence-once @cadence-weekly @src-B1-W-04 @src-B2-W-04 @src-DA94-W01
  Rule: WF-03 — The mind sweep (mental gathering with trigger lists)

    Background:
      Given physical gathering is complete
      And the user's head still holds unrepresented commitments

    Scenario: Empty the head onto separate placeholders
      When the user is asked "What has your attention that isn't represented by something already in your in-tray?"
      Then each thought, idea, or project is written on its own separate placeholder, one item per placeholder
      And each placeholder is tossed into "in"

    Scenario: Go for quantity, defer all judgment
      When ideas surface during the sweep
      Then the user goes for quantity, not quality
      And no analysis or decision-making is done during capturing
      And the capturing rules apply: no bad ideas, overcapture, stream of consciousness, no commitment

    Scenario: Run the incompletion trigger list
      Given the spontaneous flow has slowed
      When the user reviews the incompletion trigger list item by item, professional then personal
      Then each item it jogs loose gets its own placeholder into "in"
      And a quick trigger list may be used before the full trigger list

    Scenario: Close the sweep with an explicit routing choice
      When the sweep ends
      Then the user chooses for the captured batch between "process now" and "add to in-basket"

    Scenario: The sweep is complete
      Then nothing else shows up as a reminder in the user's mind
      And the full first sweep is expected to take from twenty minutes to several hours

  @WF-04 @cadence-monthly-plus @src-B2-W-05
  Rule: WF-04 — Higher-horizon capture sweep (horizons as trigger checklist)

    Background:
      Given an obvious mind sweep has been done
      And more qualitatively weighty things remain hidden behind the visible ones

    Scenario Outline: Scan each horizon for what has attention
      When the user scans the <horizon> level asking what has their attention there
      Then each hit is captured into "in" like any other item
      And it is not necessary to know anything in its fullness in order to capture it

      Examples:
        | horizon                                  |
        | projects (unrecognized problems)         |
        | areas of focus and responsibility        |
        | goals and objectives                     |
        | vision                                   |
        | purpose and principles                   |

    Scenario: When in doubt, write it down
      Given an item at a higher horizon is fuzzy
      When the user hesitates to capture it
      Then the rule applied is "When in doubt, write it down. Put it in your in-basket"
      And fuzzy higher-horizon items may later become "looking into" projects

  @WF-05 @cadence-per-item @src-B1-W-05 @src-B2-W-06 @src-B1-W-26 @src-B2-W-04
  Rule: WF-05 — Ongoing capture habit and interrupt shielding

    Background:
      Given the system is installed and life keeps producing inputs

    Scenario: Capture the moment a commitment appears
      When a "should", "need to", or "ought to" attaches to something
      Then it is recognized as an open loop
      And it is written down or recorded into a capture tool the moment it occurs
      And capture tools are as ubiquitous as a toothbrush

    Scenario: Keep the funnel minimal and empty it regularly
      Then the user keeps as few capture locations as they can get by with, and as many as they need
      And every capture bucket is emptied at least every day or two
      And emptying does not mean doing — it means deciding what each item is and organizing it into the system
      And nothing is ever left in or returned to "in"

    Scenario: Shield focus from interruptions
      Given the user is focused on something
      When an interruption arrives with a request
      Then the request is written down and thrown into the in-basket instead of dropping everything
      But it is done now if it is genuinely the most important thing to do or takes under two minutes
      And the principle holds: "There are no interruptions—there are only mismanaged inputs"

    Scenario: Bookmark open threads in calls and meetings
      When the user takes notes during a call or meeting
      Then on completion the notes are either tossed (no action) or thrown into the in-tray for later clarifying
      And annotated meeting agendas go into the in-basket after the meeting

    Scenario: Everything is in or out of the head, never in between
      Then everything potentially meaningful is in a trusted external bucket, not the head
      And the standard holds: "Keep everything in your head or out of your head. If it's in between, you won't trust either one"
