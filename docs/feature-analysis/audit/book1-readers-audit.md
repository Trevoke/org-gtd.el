# Audit: Book 1 (Getting Things Done) reader extractions vs. source

> Verifies every spec in `docs/source-material/extraction/book1-reader-{A,B,C,D}.md` against
> `docs/source-material/Getting Things Done (David Allen) (z-library.sk, 1lib.sk, z-lib.sk).txt` (8867 lines).
> Four independent auditors, one per reader file; each read the reader's full slice plus all cited lines.
> Audited 2026-06-10. Note: the source file is the **revised edition** of GTD (capture/clarify/organize/reflect/engage vocabulary), not the 2001 printing.

## Summary

| Reader | Specs | SUPPORTED | MISREAD | OVERREACH | EXAMPLE-AS-FEATURE | UNCITED | Missed items |
|---|---|---|---|---|---|---|---|
| A (lines 1–2300) | 45 | 43 | 0 | 2 | 0 | 0 | 3 significant + 2 minor |
| B (lines 2300–4500) | 43 | 41 | 0 | 2 | 0 | 0 | 5 substantive + 1 minor |
| C (lines 4500–6700) | 50 | 48 | 0 | 2 | 0 | 0 | 5 (2 clear, 3 minor) |
| D (lines 6700–8867) | 28 | 26 | 0 | 1 | 0 | 1 | 4 significant + 1 minor |
| **Total** | **166** | **158** | **0** | **7** | **0** | **1** | **~17** |

**Overall verdict:** high-fidelity extraction. Zero example-as-feature violations — Allen's anecdotes (dinner-planning, Mioko, conference-manager, vacation stories) were consistently kept as illustrations. Citations were precise throughout. The 7 overreaches share one failure shape: promoting a human judgment/mental practice into stored tool data or an invented tool behavior. The one UNCITED spec (B1-D-25) rests entirely on index entries pointing outside the reader's slice (content is covered by reader C's slice).

### Specs requiring correction in any downstream synthesis

| Spec | Problem | Fix |
|---|---|---|
| B1-A-34 | Energy as a *stored attribute* on actions — book only describes an in-the-moment human criterion ("How much energy do you have?", l.1613) | Soften: tool MAY support energy tagging; the model itself is a human selection lens |
| B1-A-36 | "Record/track items across six levels" — listing is supported only for Ground (l.1661), H1 (l.1663), H2 (l.1675); H3–H5 tracking exceeds the text | Limit storage claim to Ground/H1/H2; H3–H5 are review perspectives |
| B1-B-24 | "Actions MUST be organized by context" — l.4173–4175 explicitly says a single Next Actions list "might suffice" at ~25 actions | Recommended-with-escape-hatch, not mandate |
| B1-B-31 | "Every project must have support material" — source (l.4079–4081, 4123–4125) says only that support material, *where it exists*, lives off the Projects list | Invert: separation requirement, not existence requirement |
| B1-C-05 | Extends "may lack next action when dependency-blocked" from subprojects-in-support-material (l.4518) to top-level Projects entries; l.5157 gives every Projects item a kick-start action at review | Restrict to subprojects held in support material |
| B1-C-38 | "Detect staleness and prompt re-engagement" — l.5032/5106 state the consequence of stale lists and prescribe the Weekly Review; no detection feature in text | Reframe as: Weekly Review is the staleness remedy; detection is an implementation idea, not a source spec |
| B1-D-03 | Implementation intentions (l.6812–6814) are *mental* if-then plans; "tool prompts the user to act" exceeds the text | Reframe as passive: system serves as the visible cue |
| B1-D-25 | Four-criteria model cited only via index entries (l.7508, 7582); never presented in slice prose | Re-source to reader C's B1-C-39 (l.5322–5330), which covers it properly |

### Caveats on otherwise-SUPPORTED specs

- B1-D-14: "alpha-sorted, discrete folders" detail rests on index entries only; core spec stands (l.6906, 7070, 7108).
- B1-D-16: two-minute rule endorsed by name only in slice (l.6930); definition lives in readers A/B territory (well covered there).
- B1-B-19: waiting-for tracking is conditioned on "if you care at all whether something happens" (l.3995) — "every … must" is a hair strong.
- B1-B-01: "single place" is conceptual; l.3592–3594 lets e-mail stay in its own subsystem.
- B1-A-32: "check what else remains on the calendar" is a human habit at l.1522; the spec's "prompted" framing slightly tool-ifies it (reader self-flagged, confidence medium).

---

## Reader A (lines 1–2300) — chapters 1–3

### Verdicts

| Spec | Verdict | Type | Notes |
|---|---|---|---|
| B1-A-01 | SUPPORTED | DIGITAL-TOOL | 704 (open-loop def), 1051, 1067, 1121–1141 all check. |
| B1-A-02 | SUPPORTED | DIGITAL-TOOL | Exact quote at 1146; funneling at 1148–1156. "Single canonical inbox" flagged as note, not asserted as source. |
| B1-A-03 | SUPPORTED | DIGITAL-TOOL | 1141 ("keep them close by"), 1146 ("available in every context"). Habit framing; reasonable tool reading. |
| B1-A-04 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool-supported) | 1067, 1161 ("doesn't mean you have to finish… You don't leave it or put it back into 'in'!"). |
| B1-A-05 | SUPPORTED | METHODOLOGY-PRINCIPLE | "What Is It?" at 1190–1192; item-by-item thinking at 1177. |
| B1-A-06 | SUPPORTED | DIGITAL-TOOL | 1204–1206; reinforced at 1341. |
| B1-A-07 | SUPPORTED | DIGITAL-TOOL | 1210 ("wastebasket and <Del> key"), 1410–1412. |
| B1-A-08 | SUPPORTED | DIGITAL-TOOL | 1210, 1415–1489. |
| B1-A-09 | SUPPORTED | DIGITAL-TOOL | 1210, 1492–1504. |
| B1-A-10 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool prompts) | 1228 ("next physical, visible activity"), 1341. |
| B1-A-11 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool offers path) | 251, 1246 — verbatim. |
| B1-A-12 | SUPPORTED | DIGITAL-TOOL | 1248; Waiting-For tracking at 1343, 2210. |
| B1-A-13 | SUPPORTED | DIGITAL-TOOL | 1250, 1385–1389. |
| B1-A-14 | SUPPORTED | DIGITAL-TOOL | Definition verbatim at 1280; "stake in the ground" at 1226. |
| B1-A-15 | SUPPORTED | DIGITAL-TOOL | 1320 verbatim ("master list… review them regularly… appropriate next actions"). |
| B1-A-16 | SUPPORTED | DIGITAL-TOOL | 1329 ("Projects list will be merely an index"); overlap-with-reference note matches 1331. |
| B1-A-17 | SUPPORTED | DIGITAL-TOOL | 1350–1360. |
| B1-A-18 | SUPPORTED | DIGITAL-TOOL | 1354, 1362 (Mioko example correctly left as example). |
| B1-A-19 | SUPPORTED | DIGITAL-TOOL | 1356, 1364 (incl. lead-time/due-date reminders). |
| B1-A-20 | SUPPORTED | DIGITAL-TOOL (design constraint) | 1376 ("and nothing else!"), 1380 ("sacred territory"). Faithful tool reading of 1376–1380. |
| B1-A-21 | SUPPORTED | DIGITAL-TOOL | 1385–1401, 1632. |
| B1-A-22 | SUPPORTED | DIGITAL-TOOL | 1401 (Calls / Computer), 1530 (At Home; At Computer; In Meeting with George), 1603. |
| B1-A-23 | SUPPORTED | DIGITAL-TOOL | 1264, 1343, 1520, 2210. |
| B1-A-24 | SUPPORTED | DIGITAL-TOOL | 1421–1423; Weekly-Review scan at 1461. |
| B1-A-25 | SUPPORTED | DIGITAL-TOOL | 1463–1483; "review only when you have an urge" matches by-urge note. |
| B1-A-26 | SUPPORTED | DIGITAL-TOOL | 1266 ("bring-forward"), 1485–1487 (mail-to-self; calendar can serve same function). |
| B1-A-27 | SUPPORTED | DIGITAL-TOOL | 1500 (two forms verbatim), 1502. |
| B1-A-28 | SUPPORTED | DIGITAL-TOOL | 1262–1264; eight categories enumerate exactly as listed. |
| B1-A-29 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool-scaffolded) | 1550 (list incl. Agendas), 1562 ("clean, clear, current, and complete"). |
| B1-A-30 | SUPPORTED | METHODOLOGY-PRINCIPLE | 1550, 1556–1560. |
| B1-A-31 | SUPPORTED | METHODOLOGY-PRINCIPLE | 1522 ("hard landscape"), 1530, 1532. |
| B1-A-32 | SUPPORTED | METHODOLOGY-PRINCIPLE | 1522 is a human habit, not a tool prompt; reader self-flagged (medium). |
| B1-A-33 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool can filter) | 1599: four criteria "in this order" — verbatim. |
| B1-A-34 | **OVERREACH** | DIGITAL-TOOL (mislabeled) | 1613: energy is an in-the-moment human selection criterion; book never says actions carry a stored energy attribute. |
| B1-A-35 | SUPPORTED | DIGITAL-TOOL | 1382 — verbatim, correctly marked optional. |
| B1-A-36 | **OVERREACH** | mixed | 1645–1683 supports six-level model as prioritizing *perspectives*; listing supported only for Ground (1661), H1 (1663), H2 (1675). "Record/track across six levels" extends beyond text. |
| B1-A-37 | SUPPORTED | DIGITAL-TOOL | 1675: "not things to finish… Listing and reviewing these responsibilities." |
| B1-A-38 | SUPPORTED | METHODOLOGY-PRINCIPLE | 1751–1759 (five phases), 2216–2248; 80/15/5 split at 2228–2232. |
| B1-A-39 | SUPPORTED | DIGITAL-TOOL | 736 ("single written sentence"), 2046, 1711. |
| B1-A-40 | SUPPORTED | DIGITAL-TOOL | 2074 ("when you have the idea, you grab it"), 2082, 2087 (distributed cognition). |
| B1-A-41 | SUPPORTED | DIGITAL-TOOL | 2154, 2165–2175 (components/sequences/priorities, "Detail to the required degree"). |
| B1-A-42 | SUPPORTED | METHODOLOGY-PRINCIPLE (maps to dependencies) | 2202, 2204 ("linchpin"). |
| B1-A-43 | SUPPORTED | METHODOLOGY-PRINCIPLE + waiting-for | 2210: "clarify whose it is (a primary use of the Waiting For action list)." |
| B1-A-44 | SUPPORTED | METHODOLOGY-PRINCIPLE | 2198, 2206 ("process action"). |
| B1-A-45 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool: stuck-project check) | 884–894, 1341, 2189, 2208, 2224 ("If the project is still on your mind, there's more thinking required."). |

### Missed items (reader A's slice)

1. **Per-person / per-meeting Agenda lists** — first-class reviewed list at l.1550, instances at 1632, 1661. Only mentioned in passing inside B1-A-29; no spec for agenda items keyed to a person/meeting.
2. **Stage separation as a design requirement** — l.1035–1037 (doing all five steps at once is the major failure mode), l.1187 ("You can't organize what's incoming—you can only capture it and process it"). The tool must let the user run the stages as distinct modes — a real architectural constraint, uncaptured.
3. **Sub-categorizing the Projects list by areas of focus** — l.1324 ("There may be reasons to sort your projects into different subcategories, based upon different areas of your focus").
4. *(Minor)* Active project support material reviewed more frequently than ordinary reference — l.1331.
5. *(Minor)* Hierarchical outline with dependent/independent parts and milestones for complex projects — l.2156 (mostly subsumed by B1-A-41/42).

---

## Reader B (lines 2300–4500) — chapters 4–7 (setup, capture, clarify, organize)

### Verdicts

Type key: D-T = digital-tool capability, M-P = methodology-principle.

| Spec | Verdict | Type | Notes |
|---|---|---|---|
| B1-B-01 | SUPPORTED | D-T | 2658, 2676, 2711 "into one place—into 'in'". Caveat: 3592–3594 lets e-mail stay in its own subsystem — "single" is conceptual. |
| B1-B-02 | SUPPORTED | D-T | 2755 ("write a note… to represent it"); examples at 2686/2755 match. |
| B1-B-03 | SUPPORTED | D-T | 2472, 2873: discrete placeholders vs "one amorphous list". |
| B1-B-04 | SUPPORTED | D-T | 2757: date everything; digital date-stamp endorsed. 4001 supports dating for handoffs. |
| B1-B-05 | SUPPORTED | M-P/D-T | 2869–2877: mind sweep, "go for quantity". |
| B1-B-06 | SUPPORTED | D-T | Trigger list spans 2884–3580 (Professional 2886, Personal 3242); "review… item by item" at 2882. |
| B1-B-07 | SUPPORTED | M-P (tool-enforceable) | Rules at 3648–3652, easier-item warning 3690. Multitasking exception (3701–3705) correctly parked in V3. |
| B1-B-08 | SUPPORTED | M-P (tool-enforceable) | 3652, 3709–3711: "Never put it back in 'in'". |
| B1-B-09 | SUPPORTED | M-P (tool-prompted) | 3715, 3834, 3897–3905. |
| B1-B-10 | SUPPORTED | D-T | 3733–3741: trash / incubate / reference. |
| B1-B-11 | SUPPORTED | D-T | 3737, 3746–3760. |
| B1-B-12 | SUPPORTED | D-T | 3741, 3806–3818. |
| B1-B-13 | SUPPORTED | D-T | 3791. |
| B1-B-14 | SUPPORTED | D-T | 3793 (calendar or tickler), 3803 ("remind on October 17"). |
| B1-B-15 | SUPPORTED | D-T | 3921–3927: "You have three options" — do/delegate/defer. |
| B1-B-16 | SUPPORTED | M-P | 3923, 3932–3946. Threshold is a guideline (3946). |
| B1-B-17 | SUPPORTED | M-P | 3964. |
| B1-B-18 | SUPPORTED | D-T | 3925, 3973–3995; includes upstream delegation (3977). |
| B1-B-19 | SUPPORTED | D-T | 3995–4001, 4310. Caveat: conditioned on "if you care at all whether something happens". |
| B1-B-20 | SUPPORTED | D-T | 4087, 4302 (deliverables, not your action steps), 4310–4312. |
| B1-B-21 | SUPPORTED | D-T | 3927, 4004–4008. |
| B1-B-22 | SUPPORTED | M-P (calendar discipline) | 2500, 4155–4165: "hard landscape", "sacred territory". |
| B1-B-23 | SUPPORTED | D-T | 4155: exact examples. |
| B1-B-24 | **OVERREACH** | D-T | 4169–4177: context organization is "the best way" but 4173–4175 says a single list "might suffice" at ~25 actions. Mandate-inflation. |
| B1-B-25 | SUPPORTED | D-T | 4184–4198: the common headings. |
| B1-B-26 | SUPPORTED | D-T | 4204: phone number alongside each item — suggestion, medium confidence correct. |
| B1-B-27 | SUPPORTED | D-T | 4248–4266: per-person/per-meeting lists, 3–15 typical (4254), ad hoc addability (4266). |
| B1-B-28 | SUPPORTED | D-T | 4268–4284: >2-min reading; "self-regulating mechanism" (4284). |
| B1-B-29 | SUPPORTED | D-T | 4232: sublists within Errands items. |
| B1-B-30 | SUPPORTED | D-T | 4022–4024, 4399–4403 (comprehensive index, no plans/priority). |
| B1-B-31 | **OVERREACH** | D-T | Source says support material is its own category kept off the Projects list (4079–4081, 4123–4125, 4403) — NOT that every project must have it. |
| B1-B-32 | SUPPORTED | D-T | 4077–4099: seven categories, "pristinely distinct" (4113). |
| B1-B-33 | SUPPORTED | D-T | 4405, 4024. |
| B1-B-34 | SUPPORTED | D-T | 4405 ("at least once a week"), 4418, 4433–4434, 4486. |
| B1-B-35 | SUPPORTED | M-P | 4326–4335: original items as their own reminders. |
| B1-B-36 | SUPPORTED | M-P/D-T | 4344–4345: "visibly discrete categories based upon the next action required". |
| B1-B-37 | SUPPORTED | D-T | 4352–4370: @ACTION / @WAITING FOR folders, prefix sorting, e-mail "in" to zero. |
| B1-B-38 | SUPPORTED | M-P (tool-supported) | 4381–4383: dispersed triggers OK if all categories reviewed equally. |
| B1-B-39 | SUPPORTED | D-T | 2581 ("less than one minute"), 2625, 3810. |
| B1-B-40 | SUPPORTED | D-T | 2589 ("One Alpha System"); 2601 allows discrete directories for huge volumes. |
| B1-B-41 | SUPPORTED | D-T | 2629 (purge at least once a year), 2631, 3770. |
| B1-B-42 | SUPPORTED | M-P | 3660–3673: "Process does not mean 'spend time on'". |
| B1-B-43 | SUPPORTED | D-T | 2779, 3592–3594. |

Vague items V1–V7 spot-checked: accurately cited and appropriately hedged (V6 Pending-as-transitional-stack and V3 multitasking exception are correct readings).

### Missed items (reader B's slice)

1. **Zero-friction creation of new reference categories/folders on the fly** — l.2603 ("Make It Easy to Create a New Folder", "instantly create a new directory in your data-storage software"), l.3814. Distinct from filing speed: new-category creation cost.
2. **Reference retrieval design: search is insufficient; a categorized overview/index is required** — l.2597–2599 ("'search… can find it sufficiently' is… quite suboptimal"; "a visual map sorted in ways that make sense"; "write-only syndrome"), reinforced 3770.
3. **Configurable do-it-now threshold** — l.3946: the two-minute cutoff can stretch to five/ten minutes or shrink to thirty seconds depending on the processing window. A concrete tunable parameter.
4. **No priority scaffolding on action lists** — l.4139: don't build external priority structuring into lists; prioritization is intuitive against the whole list. A general "don't build" design constraint.
5. **Easy re-routing of an item between categories over its lifetime** — l.4306: an item "may bounce back and forth many times" (Waiting For → Read/Review → At Computer → Waiting For…). Low-friction recategorization.
6. *(Minor)* Delegation-channel preference order favoring trackable records (e-mail > note > text/voice mail > agenda > face-to-face), l.3979–3993; per-person Waiting-For visibility when meeting someone, l.4308.

---

## Reader C (lines 4500–6700) — chapters 7 (cont.), 8, 9, 10

### Verdicts

| Spec | Verdict | Type | Notes |
|---|---|---|---|
| B1-C-01 | SUPPORTED | DIGITAL-TOOL | 4505–4507: "Projects—Presentations" sub-list; separate in-progress views. |
| B1-C-02 | SUPPORTED | DIGITAL-TOOL | 4505: chronological listing by calendar order; date-property note is flagged inference. |
| B1-C-03 | SUPPORTED | DIGITAL-TOOL | 4509: sort projects by areas of focus. |
| B1-C-04 | SUPPORTED | DIGITAL-TOOL | 4514–4524: both subproject representations; user's choice. |
| B1-C-05 | **OVERREACH** | DIGITAL-TOOL | 4518 licenses *subprojects in support material* to lack next actions when blocked; extending to top-level Projects entries goes beyond the text (4520, 5157). |
| B1-C-06 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool-enforceable) | 4518: next action current on any independently movable portion. |
| B1-C-07 | SUPPORTED | DIGITAL-TOOL | 4532–4534, 4546. |
| B1-C-08 | SUPPORTED | DIGITAL-TOOL | 4548/4550: review plans every Weekly Review. |
| B1-C-09 | SUPPORTED | DIGITAL-TOOL | 4557–4567: "Attached Notes" on a Projects-list item. |
| B1-C-10 | SUPPORTED | METHODOLOGY-PRINCIPLE | 4583. |
| B1-C-11 | SUPPORTED | METHODOLOGY-PRINCIPLE | 4585 ("stale virus"); echoed 5178. |
| B1-C-12 | SUPPORTED | DIGITAL-TOOL | 4597–4601, 4620, 4624: reference = library, "a snap to file". |
| B1-C-13 | SUPPORTED | DIGITAL-TOOL | 4628–4630: >50 folders → own section/database. |
| B1-C-14 | SUPPORTED | METHODOLOGY-PRINCIPLE (tool-design guidance) | 4632–4636: contacts are purely reference; CRM caveat correctly pushed to V2. |
| B1-C-15 | SUPPORTED | DIGITAL-TOOL | 4664–4673: Someday/Maybe as "back burner". |
| B1-C-16 | SUPPORTED | DIGITAL-TOOL | 4683–4703: creative-imaginings inventory. |
| B1-C-17 | SUPPORTED | DIGITAL-TOOL | 4707: demote projects idle "for the next few months or more". |
| B1-C-18 | SUPPORTED | DIGITAL-TOOL | 4709: subcategorize someday/maybe; corporate "parking lot". |
| B1-C-19 | SUPPORTED | DIGITAL-TOOL | 4714–4736: special-interest lists, cross between reference and Someday/Maybe. |
| B1-C-20 | SUPPORTED | DIGITAL-TOOL | 4762–4772, 4782, 4808–4822: activation triggers; on arrival "insert the item as an active project". |
| B1-C-21 | SUPPORTED | DIGITAL-TOOL | 4772: day-specific (vs time-specific) calendar slot. |
| B1-C-22 | SUPPORTED | DIGITAL-TOOL | 4827–4849 (mail to future self), 4861 (check daily; look ahead before leaving town). |
| B1-C-23 | SUPPORTED | DIGITAL-TOOL | 4865, 4978–5004: checklists ad hoc + permanent, "create and eliminate them as required". |
| B1-C-24 | SUPPORTED | METHODOLOGY-PRINCIPLE | 4877–4907. |
| B1-C-25 | SUPPORTED | DIGITAL-TOOL | 4913–4955: blueprint checklists reviewed regularly. |
| B1-C-26 | SUPPORTED | DIGITAL-TOOL (engage ordering) | 5064–5066 ("Look at Your Calendar First"), 5082. |
| B1-C-27 | SUPPORTED | DIGITAL-TOOL | 5082, 5342, 5352: context lists "fold in or out". |
| B1-C-28 | SUPPORTED | DIGITAL-TOOL | 5131: "get clear, get current, and get creative". |
| B1-C-29 | SUPPORTED | DIGITAL-TOOL | 5136–5142: Get Clear steps. |
| B1-C-30 | SUPPORTED | DIGITAL-TOOL | 5149. |
| B1-C-31 | SUPPORTED | DIGITAL-TOOL | 5151: calendar past 2–3 weeks → transfer + archive. |
| B1-C-32 | SUPPORTED | DIGITAL-TOOL | 5153: upcoming events → projects & preparations. |
| B1-C-33 | SUPPORTED | DIGITAL-TOOL | 5155: follow-ups → Agenda list, check off received. |
| B1-C-34 | SUPPORTED | DIGITAL-TOOL | 5157: every project gets "at least one current kick-start action". |
| B1-C-35 | SUPPORTED | DIGITAL-TOOL | 5159: review relevant checklists. |
| B1-C-36 | SUPPORTED | DIGITAL-TOOL | 5178: activate / delete stale / add emerging — all three verbs present. |
| B1-C-37 | SUPPORTED | DIGITAL-TOOL | 5180: "Be Creative and Courageous". |
| B1-C-38 | **OVERREACH** | source is METHODOLOGY-PRINCIPLE | 5032/5106 state the consequence of stale lists and prescribe the Weekly Review as remedy; "detect staleness and prompt" is an invented tool capability. |
| B1-C-39 | SUPPORTED | METHODOLOGY-PRINCIPLE → tool mapping | 5322–5330: four criteria in order: Context, Time, Energy, Priority. |
| B1-C-40 | SUPPORTED | DIGITAL-TOOL | 5342, 5363, 5365; source itself generalizes ("tailor your own contextual categories"). |
| B1-C-41 | SUPPORTED | DIGITAL-TOOL | 5373–5377; time-estimate attribute flagged as inference (medium). |
| B1-C-42 | SUPPORTED | DIGITAL-TOOL | 5397: inventory of low-horsepower tasks. |
| B1-C-43 | SUPPORTED | DIGITAL-TOOL | 5052, 5090, 5155, 5342, 5602: Agenda lists per person/meeting, distinct from Waiting For. |
| B1-C-44 | SUPPORTED | DIGITAL-TOOL | 4755, 5487: Read/Review. |
| B1-C-45 | SUPPORTED | DIGITAL-TOOL | 4552, 5032, 5052. |
| B1-C-46 | SUPPORTED | DIGITAL-TOOL | 5632 ("Areas of Focus" list, Professional/Personal sublists, trigger every 1–3 months), 5636–5638. |
| B1-C-47 | SUPPORTED | METHODOLOGY-PRINCIPLE (partly tool-hostable) | 5519–5529 (six-horizon ladder), 5648, 5674–5680, intervals 5255–5257/5632. |
| B1-C-48 | SUPPORTED | METHODOLOGY-PRINCIPLE | 5606. |
| B1-C-49 | SUPPORTED | DIGITAL-TOOL (principle core) | 5786, 5803–5817. |
| B1-C-50 | SUPPORTED | DIGITAL-TOOL | 5823–5825, 5953 (low-end = Notes section of the project's task item). |

Vague items spot-checked: threefold model, CRM coupling, thinking-tools material correctly quarantined.

### Missed items (reader C's slice)

1. **Per-project e-mail/reference folders with Active/Archive split** — l.4569 ("Johnson Partnership—Active" / "—Archive"); coordination warning at 4571–4577 (project info must stay scannable from one Projects list).
2. **Recurring annual-event re-activation triggers** — l.4774–4778 (when to re-add next year's "Annual sales conference"). C-20 covers one-shot triggers only.
3. *(Minor)* **Scheduling the Weekly Review itself** — l.5191–5197: block two hours, early afternoon of the last workday.
4. **Ground-level completeness heuristic** — l.5602: "if you don't have at least fifty next actions and waiting-fors, including all the agendas… I would be skeptical."
5. *(Minor)* **Anti-degradation check on action lists** — l.6376–6396: entries "morph back into 'stuff'" ("Johnny's birthday," "Receptionist"); solid as a clarify-time requirement that entries stay discrete physical next actions.

---

## Reader D (lines 6700–8867) — chapters 14–15, glossary; index begins l.7216

Structural claims verified: chapter boundaries (ch.11 l.6001, ch.12 l.6201, ch.13 l.6512, ch.14 l.6692, ch.15 l.6852) and index start (l.7216) are accurate.

### Verdicts

| Spec | Verdict | Type | Notes |
|---|---|---|---|
| B1-D-01 | SUPPORTED | DIGITAL-TOOL (principle-backed) | 6742, 6750–6752, 6896. |
| B1-D-02 | SUPPORTED | DIGITAL-TOOL | 6762–6764: Baumeister — next action + trusted parked reminder suffices. |
| B1-D-03 | **OVERREACH** | METHODOLOGY-PRINCIPLE (mislabeled toward tool) | 6812–6814: implementation intentions are *mental* if-then plans; "tool prompts the user to act" exceeds the text. |
| B1-D-04 | SUPPORTED | DIGITAL-TOOL | 6784, 6836. |
| B1-D-05 | SUPPORTED | METHODOLOGY-PRINCIPLE (clarify-adjacent) | 6962: concerns/worries → outcome-projects, incl. "putting to bed with no solution". |
| B1-D-06 | SUPPORTED | DIGITAL-TOOL | 6944, 6958–6962, glossary 7136/7182. |
| B1-D-07 | SUPPORTED | DIGITAL-TOOL | 6946, 6968–6970, glossary 7138. |
| B1-D-08 | SUPPORTED | DIGITAL-TOOL | Glossary 7132–7144 (Ground + H1–H5), 7016. |
| B1-D-09 | SUPPORTED | DIGITAL-TOOL | 6976–6978; source frames as user-mastery hallmark — mild reframing, acceptable. |
| B1-D-10 | SUPPORTED | DIGITAL-TOOL | 6980, 7010. |
| B1-D-11 | SUPPORTED | DIGITAL-TOOL | 6990: mid-week Weekly Review + fresh mind sweep on crisis. |
| B1-D-12 | SUPPORTED | DIGITAL-TOOL | 6902, glossary 7202. |
| B1-D-13 | SUPPORTED | DIGITAL-TOOL | 6904, 7154/7166. |
| B1-D-14 | SUPPORTED (caveat) | DIGITAL-TOOL | Core at 6906, 7070, 7108; "alpha-sorted, discrete folders" detail rests on index entries only. |
| B1-D-15 | SUPPORTED | DIGITAL-TOOL | 6908: calendar as pure hard landscape. |
| B1-D-16 | SUPPORTED (thin) | DIGITAL-TOOL | 6930 endorses two-minute rule by name; definition outside slice. |
| B1-D-17 | SUPPORTED | DIGITAL-TOOL | 6814, 6926, 6990. |
| B1-D-18 | SUPPORTED | METHODOLOGY-PRINCIPLE (procedure) | 6926: empty head, clean lists, add new, reclaim leaks. |
| B1-D-19 | SUPPORTED | DIGITAL-TOOL | Glossary 7148, 7190. |
| B1-D-20 | SUPPORTED | DIGITAL-TOOL | Glossary 7196: tickler/perpetual/bring-forward/suspense file. |
| B1-D-21 | SUPPORTED | DIGITAL-TOOL | Glossary 7118, 7154. |
| B1-D-22 | SUPPORTED | METHODOLOGY-PRINCIPLE (clarify guidance) | Glossary 7158. |
| B1-D-23 | SUPPORTED | DIGITAL-TOOL | Glossary 7124; 7146 is actually "horizontal thinking" (minor miscite, unneeded). |
| B1-D-24 | SUPPORTED | DIGITAL-TOOL | Glossary 7184 + 7108. |
| B1-D-25 | **UNCITED** | DIGITAL-TOOL | Both citations (7508, 7582) are bare index entries; model never presented in slice. Re-source to B1-C-39. |
| B1-D-26 | SUPPORTED | METHODOLOGY-PRINCIPLE | Glossary 7194 defines threefold work substantively. |
| B1-D-27 | SUPPORTED | METHODOLOGY-PRINCIPLE (onboarding, partly physical) | 7064–7072. |
| B1-D-28 | SUPPORTED | METHODOLOGY-PRINCIPLE (procedure) | 7076. |

Vague items V1–V5 honestly characterized; V4/V5 correctly self-identify as index-only.

### Missed items (reader D's slice)

1. **Review-driven recategorization between Projects and Someday/Maybe** — l.6938: "What 'projects' need to become 'someday/maybes,' and vice versa." Bidirectional promotion/demotion during review; D-19 covers incubation one-way only.
2. **Weekly Review concrete reflective mechanics** — l.7032: scanning past and future calendar items as capture triggers; regular Someday/Maybe reassessment.
3. **Natural planning model / vertical thinking glossary entries** — l.7156, 7200 (thin one-liners; full model in ch.3, covered by reader A's B1-A-38).
4. **Anywhere/any-context availability of the system** — l.6980 ("dynamic, working dashboard… at home, at the office, or in transit"), glossary 7150.
5. *(Minor)* **Email-to-zero processing** — l.6930.
