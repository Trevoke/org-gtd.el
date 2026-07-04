---
source: DA_software.pdf (19 pages, scanned hand-drawn designs on "All-purpose Grid" paper)
author: David Allen
source-date: 1994 (page 2 dated 4/12/94)
extraction-date: 2026-06-10
extracted-by: Claude (page-by-page transcription from scans)
status: primary source, first incorporation into org-gtd feature analysis
---

# David Allen's 1994 GTD Software Designs

Hand-drawn screen designs and flowcharts for a GTD application, predating the
2001 book by seven years. The vocabulary is already fully formed: in-basket,
process, projects, next actions, waiting-for, someday/maybe, tickler, weekly
review ("de-briefing"), focus areas, 2-minute rule.

Transcription conventions: text in [brackets] was written in brackets by Allen
or is an editorial note; [illegible] marks unreadable text; ~~strikethrough~~
marks text Allen crossed out.

---

## Page 1 — FEATURES

### Transcription

- Default de-briefing process weekly (can customize) keeps life current + head clear
- Customize list sorting [due date, priority, project, age, size, pers vs. prof., etc.]
- Cross reference projects to related actions, wtg.-for's, reference, people, dates, mtgs., etc.
- Decision-making and organizing expert system assistance built in
- Retrospective calendar archive / log
- Global search
- Gateway to all other software (while processing)
- Allows free-flowing thinking while tracking ~~[word crossed out]~~ toward closure
- Rules-based customizing (e.g. every AA flight scheduled, schedule 72 hr. upgrade)
- Prints any views in any hard-copy format [e.g. by mtg., person, dates, project]
- Generates complete hard-copy systems with up-to-the-second lists & data
- Tag any file / location / activities → in-basket to ensure later closure [or make any note]
- Alarm

### Interpretation

The master feature list for the whole application: differentiating capabilities
rather than screens.

### Classification

| Item | Class | Notes |
|---|---|---|
| Weekly default de-briefing, customizable | FEATURE | Software-driven weekly review |
| Customize list sorting | FEATURE | Sort keys: due date, priority, project, age, size, personal vs. professional |
| Cross-reference projects to actions/waiting-fors/reference/people/dates/meetings | FEATURE | Relational linking |
| Expert-system assistance for deciding/organizing | FEATURE | "Coaching" — recurs on pp. 3, 6, 18, 19 |
| Retrospective calendar archive/log | FEATURE | |
| Global search | FEATURE | |
| Gateway to all other software while processing | FEATURE | Launch external apps from within the workflow |
| Free-flowing thinking while tracking toward closure | FEATURE | Brain-dump capture that the system later forces to closure |
| Rules-based customizing | FEATURE | User-defined automation rules |
| "every AA flight scheduled, schedule 72 hr. upgrade" | EXAMPLE | Illustration of rules-based customizing — the feature is rules automation, NOT airline handling |
| Print any view in any hard-copy format | FEATURE | "by mtg., person, dates, project" are example formats |
| Generate complete hard-copy systems, up-to-the-second | FEATURE | Full paper-system export |
| Tag any file/location/activity → in-basket | FEATURE | Universal capture hook on arbitrary objects |
| Alarm | FEATURE | |

---

## Page 2 — INITIAL/CURRENT VIEW (dated 4/12/94)

### Transcription

Top bar icons: **IN** (in-basket), **IN PROCESS**.
Buttons: PROJECTS | NEXT ACTIONS | WAITING FOR... | CALENDAR | COMMUNICATION |
MEETINGS | SOMEDAY MAYBE'S | FOCUS AREAS | CHECKLISTS | REFERENCE | COACHING | FIND

Stacked/cascading windows:
- PROJECTS (list window)
- NEXT ACTIONS, with sections: CALLS, OTHER, ERRANDS
- WAITING FOR'S...
- Calendar stack: MONTH / WEEK / "ONLY ON **SUNDAY APR 10**" with columns
  TIME-SPECIFIC (blocked time slots) and DAY-SPECIFIC ("DUE BY TODAY",
  "START BY TODAY"), plus a DAY-SPECIFIC NOTES box.

Week strip across bottom of calendar:
SUN BOS | MON BOS MAP | TUE BOS MAP | WED →PHL | THU YPO | FRI →LAX | SAT OJAI | SUN OJAI

Red warning flags at bottom of the screen:
- 🚩 "You have ___ # items to process in your in-basket"
- 🚩 "You have ___ projects with no next action defined"
- 🚩 "Previous calendar item not completed!"

### Interpretation

The home/dashboard screen: simultaneous cascading windows of every core GTD
list, a layered calendar, and a system-integrity warning area.

### Classification

| Item | Class | Notes |
|---|---|---|
| Dashboard of cascading list windows (projects/next actions/waiting-for/calendar) | FEATURE | |
| Next actions grouped by context (calls / other / errands) | FEATURE | |
| Calendar layered month → week → day | FEATURE | |
| Day view split: time-specific vs. day-specific; due-by-today vs. start-by-today | FEATURE | |
| Day-specific notes box | FEATURE | |
| Week strip showing each day's location/theme | FEATURE | |
| Warning flag: N in-basket items to process | FEATURE | System-integrity nag |
| Warning flag: N projects with no next action defined | FEATURE | Stuck-project detection, 1994 |
| Warning flag: previous calendar item not completed | FEATURE | Drives the page-18 coaching dialog |
| "Sunday Apr 10", BOS/MAP/PHL/YPO/LAX/OJAI itinerary | EXAMPLE | Allen's own travel week illustrating the week strip |

---

## Page 3 — INITIAL SCREEN

### Transcription

**INPUT BOX — BRAIN DUMP — PROCESS** (in-basket icon, top left)

- PROJECTS: View active..., View someday's..., New...
- NEXT ACTIONS: List..., New...
- WAITING FOR'S: List..., New...
- CALENDARS: View today... (mo; yr.), Add..., Tickler...
- MEETINGS: View, Add..., Edit...
- COMMUNICATIONS: Create..., Send..., Edit...
- VIEWS (SETS): Current commitments, Areas of focus, Goals/objectives
- FIND: by person, by key word, by date, by project, by area, by date range
- FOCUS AREAS: View..., Add..., Edit...
- REFERENCE LISTS: View..., Add..., Edit...

### Interpretation

The application's menu map — every module and its verbs. "Views (sets)" are
composite cross-list views at different horizons.

### Classification

| Item | Class | Notes |
|---|---|---|
| Menu structure of 10 modules, each with view/add/edit verbs | FEATURE | Information architecture |
| Composite views: current commitments / areas of focus / goals & objectives | FEATURE | Horizon-level rollups |
| Multi-key FIND (person, keyword, date, project, area, date range) | FEATURE | Elaborates p.1 global search |
| Tickler as part of Calendars menu | FEATURE | |

---

## Page 4 — IN (in-basket / capture screen)

### Transcription

"**WHAT'S ON YOUR MIND...?**" — large lined entry area (one item per line, arrow at top).

- QUICK TRIGGER LIST — green annotations: agreements? interactions?
  incompletions? events? new to-do's? distractions? new ideas?
- FULL TRIGGER LIST — [from MAP list]
- Buttons: **PROCESS NOW** / **ADD TO IN-BASKET**

### Interpretation

The capture screen: free-form mind-sweep entry assisted by on-demand trigger
lists, ending with a choice to clarify immediately or defer to the in-basket.

### Classification

| Item | Class | Notes |
|---|---|---|
| Free-form multi-line capture ("What's on your mind?") | FEATURE | |
| Quick + full trigger lists to prompt the sweep | FEATURE | "Full" sourced from the MAP (Managing Action & Projects seminar) list |
| agreements/interactions/incompletions/events/new to-do's/distractions/new ideas | EXAMPLE | Sample trigger categories on the quick list |
| Choice: process now vs. add to in-basket | FEATURE | Capture and clarify are decoupled |

---

## Page 5 — IN → PROCESS

### Transcription

(1st) ITEM ____________

~~Committed to do something about this?~~ → rewritten: **ACTION ON THIS?**
(green "OR" with three buttons: **ACTIVATE / INCUBATE / ELIMINATE**)

- ☐ NO → ☐ Discard? · ☐ Someday/Maybe?... → go to "SOMEDAY" · ☐ Reference/support ↕
- ☐ YES →
  - ☐ Potentially more than single step? → (**go to "PROJECTS"**) or...
    Title project/outcome [field] → WHAT'S THE NEXT ACTION? [field] → **go to "NEXT ACTIONS"**
  - ☐ Simple action →
    - ☐ Less than 2 min? (ALARM) **DO IT**
    - Can it be delegated?
      - ☐ NO → go to "NEXT ACTIONS"
      - ☐ YES → go to "COMMUNICATION"

### Interpretation

The canonical GTD clarify flowchart, designed directly as software navigation —
seven years before the book's workflow diagram. Each terminal routes to a
module. Note the alarm attached to the 2-minute rule, and the
activate/incubate/eliminate triage vocabulary.

### Classification

| Item | Class | Notes |
|---|---|---|
| Entire decision tree | WORKFLOW | See DA94-W02 |
| One-item-at-a-time presentation ("(1st) ITEM") | FEATURE | |
| 2-minute timer with alarm during "do it" | FEATURE | |
| Activate / Incubate / Eliminate triage buttons | FEATURE | Alternative phrasing of the actionable? question |
| Inline project creation (title outcome → next action) without leaving the flow | FEATURE | "or..." branch avoids context switch to Projects module |

---

## Page 6 — PROJECT (INPUT)

### Transcription

Top row: FREE-FORM DOWNLOAD? ☐ · HOW LONG? ☐ · TITLE/TOPIC ____ · FOCUS AREA [list]

- **ADD NEW IDEAS / BRAIN DUMP** box — (unprocessed notes)
- WANT PROJECT-THOUGHT COACHING? ☐ → **PROJECT TEMPLATE** ( ·—? ·—? ·—? etc.)
- **PROCESS NOTES** → [one @ time] →
  - TO DO? → sub-output title ____ / next action ____
  - RESOURCE? · DATA? · OPTION?
- **ACTIVATE / INCUBATE / ELIMINATE** ←----?----

Below the divider (project record layout):
PURPOSE · SUCCESSFUL OUTCOME SCENARIO · COMPONENTS/SEQUENCES/PRIORITIES
(checklist) · CURRENT NEXT ACTIONS · CURRENT WTG.-FOR'S · MAYBE'S/OPTIONS ·
REFER. (in-basket icon) → RESOURCE PEOPLE · OTHER RESOURCES · DATA · NOTES

### Interpretation

Project creation and project-support screen. Two layers: (a) a brainstorm
workflow with optional coaching template and one-at-a-time note triage into
to-do/resource/data/option; (b) the persistent project record — the Natural
Planning Model (purpose, outcome vision, components, next actions) as a data
structure.

### Classification

| Item | Class | Notes |
|---|---|---|
| Brainstorm → process notes one-at-a-time → classify → activate/incubate/eliminate | WORKFLOW | See DA94-W03 |
| Optional "project-thought coaching" with question template | FEATURE | |
| Timed free-form download ("How long?") | FEATURE | |
| Note triage categories: to-do (→ sub-output + next action), resource, data, option | FEATURE | |
| Project record: purpose, successful outcome scenario, components/sequences/priorities, current next actions, current waiting-fors, maybes/options, resource people, other resources, data, notes | FEATURE | Natural Planning Model as schema |
| Focus area assigned at project creation | FEATURE | |

---

## Page 7 — PROJECTS

### Transcription

- LIST (DUE-DATED FIRST) — green: ENTER DATE — → [EACH]
- green: EXPECTED BY / DELEGATED ____
- PLANS/OUTLINE/ORGANIZATION (in-basket icon)
- PURPOSE/GOAL/VISION — STANDARDS
- RELATED TO WHAT LARGER AREA OR OBJECTIVE? OR OUTPUT? ☐
- SUCCESSFUL OUTCOME – DESCRIPTION
- RESOURCE PEOPLE (box)
- ACTIONS DONE | NEXT ACTION(S) (two-column table)
- CUSTOMERS / RECIPIENTS / BENEFICIARIES

### Interpretation

The projects list view plus the per-project detail. Adds: due-dated projects
sort first; per-project done/next two-column history; linkage upward to larger
areas/objectives; stakeholders.

### Classification

| Item | Class | Notes |
|---|---|---|
| Project list sorted with due-dated projects first | FEATURE | |
| Entry-date stamping of projects | FEATURE | |
| Per-project "expected by / delegated" | FEATURE | Projects themselves can be delegated |
| Per-project actions-done vs. next-action(s) ledger | FEATURE | Project history + forward motion side by side |
| Link project to larger area/objective/output | FEATURE | Vertical (horizon) linking |
| Purpose/goal/vision, standards, successful-outcome description fields | FEATURE | |
| Customers/recipients/beneficiaries field | FEATURE | Stakeholder tracking |

---

## Page 8 — NEXT ACTIONS

### Transcription

- red: AUTO DATES ENTRIES — [LIST] table: DATE | item | DONE?
  ✓ → [ASKS] → PROJECT COMPLETE ☐ · WHAT'S THE NEXT ACTION? ____
- green flag: **CAN BE DELEGATED** → red: [GO TO COMMUNICATION VIEW]
- Verb menu (blue box): CALL... RE:.... · DRAFT... · BRAINSTORM... · REVIEW...
  · EDIT... · READ... · GET/BUY... → ERRAND LIST? (sub-list: MARKET, HARDWARE,
  STATIONERY, CLOTHES, GIFTS, MISC. ERRANDS, WEEKEND ERRANDS, NURSERY;
  checkboxes ANYDAY ☐ / WEEKEND ☐) · ORGANIZE... · TAKE...
- Scheduling (red: [ONE! ONLY.]):
  - ☐ CAN **ONLY** DO **ON** [DATE]
  - ☐ NEED TO DO **BY** [DATE] ⇓
  - ☐ NEED TO **START BY** [DATE]
  - ☐ DO AS SOON AS APPROPRIATE
- FOCUS AREA — green: [DEFAULT FROM PROJECT] [PULL-DOWN IF SINGLE ACTION]

### Interpretation

The next-actions module. Richest single page for data semantics: completion
prompts that keep projects unstuck, action-verb-driven entry, errand
sub-contexts, a strict exactly-one date-semantics rule, and focus-area
inheritance.

### Classification

| Item | Class | Notes |
|---|---|---|
| Auto-date-stamp on entry | FEATURE | |
| On marking done: ask "project complete?" and "what's the next action?" | FEATURE / WORKFLOW | The anti-stuck-project mechanism; see DA94-W04 |
| "Can be delegated" flag routes to Communication view | FEATURE | |
| Action entry begins with a verb menu (call/draft/brainstorm/review/edit/read/get-buy/organize/take) | FEATURE | Enforces actions-start-with-verbs |
| Get/Buy verbs route to an errand list with sub-locations | FEATURE | |
| Market/hardware/stationery/clothes/gifts/nursery, etc. | EXAMPLE | Sample errand sub-categories |
| Anyday vs. weekend errand tagging | FEATURE | |
| Exactly ONE of: only-on date / do-by date / start-by date / ASAP | FEATURE | Mutually exclusive date semantics per action |
| Focus area defaults from parent project; pull-down only for single actions | FEATURE | Property inheritance |

---

## Page 9 — PERSONS

### Transcription

Four columns:
- DISCUSSION AGENDAS — red: DROP TO "NOTES"
- WTG. FOR'S OUTSTANDING — →ME | →THEM
- RESOURCE FOR WHAT PROJECTS/AREAS? — red: TIE TO PROJECTS, AREAS
- ON-GOING INTEREST POINTS

Blue (top right): **PRINT PRIOR TO MTG.** ☒
Green box: ~~CALENDAR~~ SPECIAL DATES — red: FROM TICKLER
Bottom: ~~LAST MTG.~~ ~~NEXT MTG.~~ **NOTES** (wide box) — → **IN-BASKET FOR
FURTHER PROCESSING** (torn-page box, right)

### Interpretation

A per-person CRM-like view: agendas to discuss, bidirectional waiting-fors,
which projects/areas the person is a resource for, standing interests, special
dates pulled from the tickler, meeting notes that can be swept into the
in-basket. Printable before a meeting.

### Classification

| Item | Class | Notes |
|---|---|---|
| Per-person discussion agenda list | FEATURE | |
| Waiting-fors split by direction: owed to me vs. owed to them | FEATURE | Bidirectional commitment tracking |
| Person ↔ projects/areas "resource for" links | FEATURE | |
| Ongoing interest points per person | FEATURE | Relationship memory |
| Special dates auto-fed from tickler | FEATURE | |
| Print person brief prior to meeting | FEATURE | |
| Agenda items drop into meeting notes; notes route to in-basket | FEATURE / WORKFLOW | Notes are raw input, not storage |

---

## Page 10 — SOMEDAY/MAYBE...

### Transcription

ITEM ____ · FOCUS AREA ____

- INCLUDE IN IDEAL VISION? ☐
- BRAINSTORM? ____ MINUTES — red: → GO TO "TOPIC DEVELOPMENT"
- REMIND/REVIEW THIS HOW?
  - ☐ SOMEDAY LIST (WEEKLY REVIEW)
  - ☐ FUTURE DATE ____
  - ☐ EVERY ____ (TICKLER)

### Interpretation

Incubation intake form. The key design idea: every someday/maybe item must
choose its own resurfacing mechanism — generic weekly-reviewed list, a specific
future date, or a recurring tickler.

### Classification

| Item | Class | Notes |
|---|---|---|
| Per-item choice of review mechanism (weekly list / future date / recurring) | FEATURE | Incubation with guaranteed resurfacing |
| "Include in ideal vision?" flag | FEATURE | Links incubated items to higher horizons |
| Optional timed brainstorm → "topic development" | FEATURE | |
| Focus area on someday items | FEATURE | |

---

## Page 11 — TICKLER

### Transcription

~~RECORDS~~
- ACTIVITY/EVENT/NOTE ________ — PERSONS INVOLVED ____ → red: TIE TO "PERSON" VIEW
- DATE ____ → red: TIE TO CALENDAR [DAY-SPECIFIC NOTES]
- RECUR EVERY ____
- NOTIFY ____ DAYS / WEEKS / MONTHS AHEAD

### Interpretation

Tickler record schema: a dated (optionally recurring) note that materializes in
the calendar's day-specific notes, with configurable advance notification and
links to person views.

### Classification

| Item | Class | Notes |
|---|---|---|
| Tickler entries surface as day-specific calendar notes | FEATURE | |
| Recurrence ("recur every") | FEATURE | |
| Advance notification N days/weeks/months ahead | FEATURE | Lead-time reminders |
| Persons-involved link to Person view | FEATURE | |

---

## Page 12 — MEETINGS

### Transcription

UPCOMING ☐ · OCCURRED ☐
- TITLE ____ (type list: STAFF, PROJECT, COMMITTEE, OTHER, SALES)
- PROJECT TIE? [LIST]
- DATE ____ · EXPECTED PARTICIPANTS [LIST]
- POSSIBLE/PROPOSED AGENDAS: INFO TO GIVE · INFO TO GET · OPTIONS TO EXPLORE ·
  ~~DECISIONS~~ CHOICES TO MAKE · SET NEXT MTG.?
- [IF "OCCURRED"] (EACH) → TOPIC ____ · DECISIONS ____ · NEXT ACTIONS
  (WHO? ____ WHEN? ____ DUE DATE? ____) · NOTES · PROJECT TIE ☐
- green: **SEND TO IN-BASKET**
- [PRINT TO FILL IN NOTES]

### Interpretation

Meeting lifecycle object: pre-meeting (typed, project-tied, participants,
structured agenda) and post-meeting (per-topic decisions and next actions with
owner/when/due). Outcomes can be sent to the in-basket; the form can be printed
blank for handwritten notes.

### Classification

| Item | Class | Notes |
|---|---|---|
| Upcoming → occurred meeting lifecycle | WORKFLOW | See DA94-W06 |
| Structured agenda template (info to give/get, options to explore, choices to make, set next mtg) | FEATURE | |
| Post-meeting capture: per-topic decisions + next actions with who/when/due | FEATURE | |
| Meeting ↔ project tie | FEATURE | |
| Send meeting outputs to in-basket | FEATURE | |
| Print form to fill in notes by hand | FEATURE | |
| Staff/project/committee/sales meeting types | EXAMPLE | Sample type values |

---

## Page 13 — COMMUNICATION

### Transcription

**COMMUNICATION**
TO: [BOB, SUE, etc. — list]
- ☐ EMAIL [CONNECT TO SYSTEM]
- ☐ PHONE [DIAL] → [WHEN HANGS UP] ANYTHING TO PROCESS?
- ☐ FAX [AUTO SEND OR PRINT]
- ☐ NOTE/MEMO PRINT [TYPE BELOW]
- ☐ WORD PROCESSOR [RUN APPLICATION]
- ☐ RUN AN AGENDA — green: ONE-ON-ONE
- ☐ MTG. AGENDA — red: → GO TO "MEETING"

PROJECT TIE? (DEFAULT) or [DROPDOWN FROM LIST]
NOTES (bulleted box)
→ ☐ WAITING FOR? (OUTPUT RESPONSE box)
→ COMPLETE? → YES → ☐ STORE · ☐ DISCARD
            → NO → [GOES TO NEXT ACTION "FINISH EMAIL TO..."]

### Interpretation

The delegation/outbound-communication hub. Channel-agnostic (email, phone,
fax, memo, word processor, agenda), integrated with external systems, with two
crucial closure mechanisms: optionally spawn a waiting-for on send, and if the
communication is left incomplete, auto-generate a next action to finish it.

### Classification

| Item | Class | Notes |
|---|---|---|
| Multi-channel communication dispatch with external-app integration (email connect, phone auto-dial, fax auto-send, launch WP) | FEATURE | Concrete form of p.1 "gateway to all other software" |
| Post-phone-call prompt "anything to process?" | FEATURE | Capture hook at communication boundaries |
| "Run an agenda" — pull person's agenda items into a one-on-one | FEATURE | Connects to PERSONS view |
| Project tie defaulted, overridable via dropdown | FEATURE | |
| Optional waiting-for created from the communication | FEATURE | Delegation → tracking in one step |
| Incomplete communication auto-becomes a next action | FEATURE | Nothing falls through mid-task |
| "Finish email to...", "Bob, Sue" | EXAMPLE | Illustrations of the generated action and recipient list |

---

## Page 14 — AREAS OF FOCUS

### Transcription

GOALS + OBJECTIVES | [AREA] ________
Columns: ACTIVE PROJECTS | NEXT ACTIONS | SOMEDAY/MAYBE'S — LATER REVIEW |
~~REFERENCE~~ ROUTINES + REMINDERS

Below: **REFERENCE** — LIST 1 | LIST 2 | RESOURCE PEOPLE |
☐ e.g. WINE LISTS · ☐ ~~BOOKS~~ RECOMMENDED RESOURCES → BOOKS ☐ TAPES ☐
VIDEOS ☐ SEMINARS ☐ PEOPLE ⬡

### Interpretation

A per-area rollup: everything (projects, actions, someday/maybes,
routines/reminders, reference material, resource people) filtered by one area
of focus, headed by that area's goals and objectives — the horizontal lists
re-sliced vertically.

### Classification

| Item | Class | Notes |
|---|---|---|
| Per-area composite view: active projects, next actions, someday/maybes, routines + reminders | FEATURE | |
| Goals + objectives attached to each area | FEATURE | |
| Per-area reference lists and resource people | FEATURE | |
| Wine lists; recommended books/tapes/videos/seminars/people | EXAMPLE | Sample reference content |

---

## Page 15 — (untitled: action/delegation record layout)

### Transcription

Field row (blue): ENTRY DATE | OUTCOME/ACTIVITY | PROJECT TITLE | FOCUS AREA |
EXPECTED BY (WHOM) | DUE DATE | MUST BE STARTED BY (WHEN) | DONE (DATE)

Green detail under "Expected by": PLAN ~~HOLDING~~ TO DELEGATE TO ☐ ·
DELEGATED TO ☐ → DATE ☐

### Interpretation

The universal record schema for a tracked item — likely the underlying data
model shared by next actions and waiting-fors. Delegation has two distinct
states: planned-to-delegate vs. actually delegated (with date).

### Classification

| Item | Class | Notes |
|---|---|---|
| Unified item schema: entry date, outcome/activity, project, focus area, owner, due date, must-start-by, done date | FEATURE | |
| Two-stage delegation state: plan-to-delegate → delegated-to + date | FEATURE | |

---

## Page 16 — REFERENCE LISTS

### Transcription

[WILL HAVE SOME FORM-DESIGN CAPABILITIES] [OR MACRO TO RUN WP APP]
CITIES/AREAS YELLOW PGS. (ETC.)
"Books" (doodle)

### Interpretation

Reference module: user-definable list forms, or escape hatch to a word
processor via macro.

### Classification

| Item | Class | Notes |
|---|---|---|
| User form-design capability for reference lists | FEATURE | |
| Macro escape to external word processor | FEATURE | |
| Cities/areas yellow pages, books | EXAMPLE | Sample reference lists |

---

## Page 17 — WEEKLY DE-BRIEFING

### Transcription

- DATA DUMP
- PROCESS IN-BASKET — ON LINE; DESK/BRIEFCASE CHECK
- REVIEW DAILY PAGES SINCE LAST DE-BRIEF — ADD NOTES · ARCHIVE
- TODAY'S CALENDAR — OK?
- **PROJECT** REVIEW — DONE? · NEW TO ADD? · NEXT ACTION ON EACH?
- **WTG.-FOR** REVIEW — REC'D? · NEW? · NEXT ACTIONS ON ANY?
- ROUTINES/RESPONSIBILITIES/CHECKLISTS — OK? · NEXT ACTIONS? · ADDITIONS?
- **UPCOMING CALENDAR** REVIEW — ADDITIONS/CHANGES? · PREPARATIONS REQUIRED?
  ACTIONS · DAY THEMES/LOCATIONS UPDATED?
- **NEXT ACTIONS** REVIEW — DONE? · NEW?
- **FOCUS AREA** REVIEWS

### Interpretation

The weekly review as an ordered, software-guided checklist — the design behind
page 1's "default de-briefing process weekly." Every step is phrased as
questions the software can ask per item.

### Classification

| Item | Class | Notes |
|---|---|---|
| Entire de-briefing sequence | WORKFLOW | See DA94-W07 |
| Guided checklist with per-list audit questions | FEATURE | "Next action on each?" = stuck-project sweep |
| Daily-page review with annotate + archive | FEATURE | Feeds p.1 retrospective calendar log |

---

## Page 18 — COACHING MESSAGES

### Transcription

"You have an action recorded on your calendar that was not recorded as done.
Calendars are for items that **expire** only. We will move the item to your
on-going 'Next Action' list, unless you

  [CANCEL]    [RE-SCHEDULE] — WHEN? ____"

### Interpretation

A sample coaching dialog enforcing the hard-landscape calendar principle: an
uncompleted calendar item is automatically migrated to next actions unless the
user cancels or reschedules it. The software teaches methodology while
correcting state.

### Classification

| Item | Class | Notes |
|---|---|---|
| Methodology-enforcing coaching dialogs | FEATURE | |
| Auto-migration of uncompleted calendar items to next actions (with cancel/reschedule escape) | FEATURE / WORKFLOW | See DA94-W08 |

---

## Page 19 — COACHING MODELS — IN-BASKET PROCESSING

### Transcription

**IN-BASKET PROCESSING**
- 1 ITEM @ A TIME · TOP ITEM 1ST
- WHAT IS IT?
- OUTCOME REQUIRED? → **ENTER PROJECT**
- NEXT ACTION? → ~~ENTER NEXT ACTION~~
  - **DUMP** IT
  - **DO** IT (< 2 MIN) ← FINISH COMMUNICATION ROUTE
  - **DELEGATE** IT → GO: COMMUNICATION
  - **DEFER** IT → ENTER: NEXT ACTION
- THEN STORE ~~[word]~~ INPUT PARTICLE AS REFERENCE, OR TOSS.

Right margin (green):
- → LIKE POST-IT ON SCREEN FOR PROCESSING EMAIL
- → ☐ ON INITIAL SCREEN FOR PROCESSING PAPER

### Interpretation

The coaching script for in-basket processing — the earliest known written form
of the "Four D's" (dump/do/delegate/defer) — plus a UI insight: the coach
should render as a floating post-it overlay when processing email, and as a
panel on the home screen when processing physical paper.

### Classification

| Item | Class | Notes |
|---|---|---|
| One-at-a-time, top-first processing discipline | WORKFLOW | Constraint baked into the UI |
| What is it? → outcome? → next action? → dump/do/delegate/defer → store-or-toss | WORKFLOW | See DA94-W02/W09 |
| Context-sensitive coach presentation (post-it overlay for email; home-screen panel for paper) | FEATURE | Coaching adapts to the medium being processed |

---

# SYNTHESIS

## Master feature list (deduplicated)

| ID | Feature | Page(s) |
|---|---|---|
| DA94-01 | Weekly de-briefing/review: software-guided, ordered checklist with per-item audit questions; on by default, customizable | 1, 17 |
| DA94-02 | Customizable list sorting (due date, priority, project, age, size, personal vs. professional) | 1 |
| DA94-03 | Cross-referencing/relational links: projects ↔ actions, waiting-fors, reference, people, dates, meetings | 1, 7, 9, 11, 12, 13 |
| DA94-04 | Built-in expert-system "coaching": decision/organizing assistance, methodology-enforcing dialogs, context-sensitive presentation (post-it overlay for email, panel for paper) | 1, 2, 3, 6, 18, 19 |
| DA94-05 | Retrospective calendar archive/log; daily pages reviewed, annotated, archived | 1, 17 |
| DA94-06 | Global search / FIND by person, keyword, date, project, area, date range | 1, 3 |
| DA94-07 | Gateway to other software while processing: email connect, phone auto-dial, fax auto-send/print, launch word processor, macros | 1, 13, 16 |
| DA94-08 | Free-form brain-dump capture everywhere, with the system tracking captured material to closure | 1, 4, 6 |
| DA94-09 | Rules-based customizing (user-defined automation rules) | 1 |
| DA94-10 | Print any view in any format; generate a complete up-to-the-second hard-copy system; print person briefs and blank meeting forms | 1, 9, 12 |
| DA94-11 | Universal in-basket routing: tag any file/location/activity, meeting outputs, or person notes into the in-basket for later closure | 1, 9, 12 |
| DA94-12 | Alarms, including a 2-minute timer during "do it" | 1, 5 |
| DA94-13 | Dashboard home view: cascading windows of all core lists plus calendar | 2 |
| DA94-14 | System-integrity warning flags: "You have N items to process in your in-basket", "You have N projects with no next action defined", "Previous calendar item not completed!" | 2 |
| DA94-15 | Layered calendar (month/week/day); day view split into time-specific vs. day-specific, due-by-today vs. start-by-today; day-specific notes; week strip with per-day locations/themes | 2, 8, 17 |
| DA94-16 | Next actions grouped by context (calls/other/errands); errand sub-locations with anyday/weekend tagging | 2, 8 |
| DA94-17 | Capture screen with quick and full trigger lists prompting the mind-sweep | 4 |
| DA94-18 | Decoupled capture: "process now" vs. "add to in-basket" | 4 |
| DA94-19 | One-item-at-a-time, top-first clarification, presented as guided navigation | 5, 19 |
| DA94-20 | Activate / Incubate / Eliminate triage verbs | 5, 6 |
| DA94-21 | Inline project creation during clarify (title outcome → next action) without leaving the processing flow | 5 |
| DA94-22 | Project brainstorm module: timed free-form download, optional coaching template, one-at-a-time note triage into to-do (sub-output + next action) / resource / data / option | 6 |
| DA94-23 | Project record = Natural Planning Model schema: purpose, successful outcome scenario, components/sequences/priorities, current next actions, current waiting-fors, maybes/options, resources (people, data, notes) | 6, 7 |
| DA94-24 | Project metadata: due-dated-first sorting, entry dates, vertical link to larger area/objective, standards, customers/recipients/beneficiaries, actions-done vs. next-actions ledger | 7 |
| DA94-25 | Completion prompts: marking an action done asks "project complete?" / "what's the next action?" | 8 |
| DA94-26 | Verb-menu action entry (call, draft, brainstorm, review, edit, read, get/buy, organize, take...) with verb-driven routing (get/buy → errands) | 8 |
| DA94-27 | Exactly-one date semantics per action: can-only-do-ON / need-to-do-BY / need-to-start-BY / as-soon-as-appropriate | 8 |
| DA94-28 | Focus-area inheritance: actions default to their project's area; pull-down only for standalone actions | 8 |
| DA94-29 | Person view: discussion agendas, waiting-fors split me/them, resource-for project/area links, ongoing interest points, tickler-fed special dates, notes → in-basket, print prior to meeting | 9 |
| DA94-30 | Someday/maybe items choose their resurfacing mechanism: weekly-review list, specific future date, or recurring tickler; optional "include in ideal vision?" flag and timed brainstorm | 10 |
| DA94-31 | Tickler records: date + recurrence + advance notification (days/weeks/months ahead), surfacing as day-specific calendar notes, linked to persons | 11 |
| DA94-32 | Meeting object: typed, project-tied, participants, structured agenda (info to give/get, options to explore, choices to make, set next meeting); post-meeting per-topic decisions and next actions with who/when/due | 12 |
| DA94-33 | Communication/delegation hub: pick person + channel, project tie defaulted, optional waiting-for spawned on send, incomplete communication auto-becomes a next action ("Finish email to...") | 13 |
| DA94-34 | Capture hooks at communication boundaries: when a phone call ends, prompt "anything to process?" | 13 |
| DA94-35 | "Run an agenda": pull a person's queued agenda items into a one-on-one or meeting | 13, 9 |
| DA94-36 | Areas-of-focus rollup view: per-area goals/objectives, active projects, next actions, someday/maybes, routines + reminders, reference, resource people | 3, 14 |
| DA94-37 | Composite horizon views ("sets"): current commitments, areas of focus, goals/objectives | 3 |
| DA94-38 | Unified item schema: entry date, outcome/activity, project, focus area, expected-by (whom), due date, must-start-by, done date; auto-date-stamping | 8, 15 |
| DA94-39 | Two-stage delegation state: plan-to-delegate-to → delegated-to + date | 15 |
| DA94-40 | User form-design for reference lists (with word-processor macro escape hatch) | 16 |
| DA94-41 | Uncompleted calendar items auto-migrate to next actions unless cancelled/rescheduled ("calendars are for items that expire only") | 18, 2 |

## Workflows

| ID | Workflow | Page(s) |
|---|---|---|
| DA94-W01 | Mind-sweep capture: "What's on your mind?" → optionally consult quick/full trigger lists → enter items → PROCESS NOW or ADD TO IN-BASKET | 4 |
| DA94-W02 | Clarify (IN → PROCESS): take 1st item → action on this? — NO → discard / someday-maybe (→ Someday module) / reference-support; YES → more than one step? → Projects (or inline: title outcome → next action → Next Actions); simple action → <2 min (alarm) → DO IT; can it be delegated? YES → Communication, NO → Next Actions | 5, 19 |
| DA94-W03 | Project development: free-form brain dump (timed) → optional coaching template → process notes one at a time → classify each as to-do (sub-output + next action) / resource / data / option → activate / incubate / eliminate | 6 |
| DA94-W04 | Action completion: mark done → system asks "project complete?" → if not, "what's the next action?" — guaranteeing no project is left without a next action | 8 |
| DA94-W05 | Communication/delegation: choose recipient + channel → external system executes (dial, send) → optionally record waiting-for → complete? YES → store or discard; NO → auto-create next action to finish it | 13 |
| DA94-W06 | Meeting lifecycle: create upcoming meeting (type, project tie, participants, proposed agenda) → mark occurred → per topic record decisions + next actions (who/when/due) + notes → send outputs to in-basket | 12 |
| DA94-W07 | Weekly de-briefing: data dump → process in-basket (online + desk/briefcase) → review daily pages since last de-brief (annotate, archive) → today's calendar OK? → project review (done? new? next action on each?) → waiting-for review (received? new? next actions?) → routines/responsibilities/checklists → upcoming calendar (changes? preparations? day themes/locations?) → next actions review (done? new?) → focus area reviews | 17 |
| DA94-W08 | Calendar integrity: calendar item not marked done by end of day → flag on dashboard → coaching dialog → auto-move to next actions unless user cancels or reschedules | 2, 18 |
| DA94-W09 | In-basket processing coach: one item at a time, top first → what is it? → outcome required? → enter project → next action? → dump / do (<2 min) / delegate (→ communication) / defer (→ next action) → then store input as reference or toss | 19 |

## Items flagged as EXAMPLES (not features)

| Page | Example text | Actual feature it illustrates |
|---|---|---|
| 1 | "every AA flight scheduled, schedule 72 hr. upgrade" | Rules-based customizing (DA94-09) |
| 1 | "by mtg., person, dates, project" | Print any view in any format (DA94-10) |
| 2 | "Sunday Apr 10"; BOS / BOS MAP / →PHL / YPO / →LAX / OJAI week strip | Day view + week strip with locations/themes (DA94-15) |
| 4 | agreements? / interactions? / incompletions? / events? / new to-do's? / distractions? / new ideas? | Trigger-list content (DA94-17) |
| 8 | Market, hardware, stationery, clothes, gifts, misc. errands, weekend errands, nursery | Errand sub-location lists (DA94-16) |
| 12 | Staff / project / committee / other / sales | Meeting type field (DA94-32) |
| 13 | "Bob, Sue"; "Finish email to..." | Recipient list; auto-generated follow-up action (DA94-33) |
| 14 | Wine lists; recommended books/tapes/videos/seminars/people | Per-area reference lists (DA94-36) |
| 16 | Cities/areas yellow pages; books | Reference lists (DA94-40) |

## Notable UI/UX ideas

- **Dashboard warning flags** (p. 2): "You have ___ items to process in your
  in-basket" / "You have ___ projects with no next action defined" / "Previous
  calendar item not completed!" — system-integrity surfaced at the home screen,
  including stuck-project detection in 1994.
- **Coaching as a first-class UI element** (pp. 2, 18, 19): a COACHING button on
  the toolbar; methodology-enforcing dialogs; the coach rendered as a "post-it
  on screen" overlay when processing email vs. a panel when processing paper.
- **Auto-migration with consent** (p. 18): incomplete calendar items moved to
  next actions by default — the system acts, the user can veto (cancel /
  reschedule) — rather than nagging passively.
- **Exactly-one date semantics** (p. 8): an action carries at most one of
  ON / BY / START-BY / ASAP, marked "[ONE! ONLY.]" — a deliberate constraint
  against date soup.
- **Closure loops everywhere**: communications that aren't finished become next
  actions; completed actions interrogate their project; phone hang-ups prompt
  capture; meeting outputs and person notes route to the in-basket. Every exit
  path feeds back into the system.
- **Verb-first action entry** (p. 8): actions begin from a verb menu, encoding
  "next actions are physical, visible behaviors" into the input control itself.
- **Print as a first-class citizen** (pp. 1, 9, 12): the digital system
  generates complete, current paper systems on demand — 1994 mobility.

## Relevance hooks for org-gtd.el (extraction notes, not part of the source)

- DA94-14 ("projects with no next action") corresponds to org-gtd's stuck-project
  review; the 1994 design surfaces it proactively on the dashboard rather than
  only in a review view.
- DA94-25/W04 (done → "what's the next action?") matches org-gtd's project task
  progression; the prompt-on-completion pattern is the primary anti-stuck
  mechanism in the source.
- DA94-27 maps cleanly to org-mode timestamp vs. SCHEDULED vs. DEADLINE, with
  the addition of an explicit "must be started by" distinct from SCHEDULED.
- DA94-28 (focus-area inheritance from project to action) parallels property
  inheritance of areas of focus.
- DA94-30 (someday items choose their resurfacing mechanism) is richer than a
  single incubate date: list-based, dated, or recurring review per item.
- DA94-W02/W09 confirm the clarify flow org-gtd implements; the
  activate/incubate/eliminate vocabulary (DA94-20) is an alternative framing.
