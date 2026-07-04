# Audit: Book 2 ("Making It All Work") Reader Extractions

Audited against `docs/source-material/making-it-all-work-CONVERTED.txt` (11,936 lines).
Every cited range was read with margin; the full source was read end-to-end for the
completeness check.

## Summary

| | Items audited | SUPPORTED | MISREAD | OVERREACH | EXAMPLE-AS-FEATURE | UNCITED |
|---|---|---|---|---|---|---|
| Reader A (1–3000) | 44 (25 specs, 14 deltas, 5 vague) | 44 | 0 | 0 | 0 | 0 |
| Reader B (3000–6000) | 58 (39 specs, 12 deltas, 7 vague) | 56 | 0 | 2 | 0 | 0 |
| Reader C (6000–9000) | 44 (30 specs, 8 deltas, 6 vague) | 42 | 0 | 1 | 1 | 0 |
| Reader D (9000–11936) | 47 (34 specs, 8 deltas, 5 vague) | 46 | 0 | 1 | 0 | 0 |
| **Total** | **193** | **188** | **0** | **4** | **1** | **0** |

Overall verdict: extraction quality is high. Spec-level claims are almost uniformly
accurate and correctly generalized (readers consistently extracted the principle, not
the anecdote). All errors are concentrated in the **"Changes vs Book 1"** sections,
where readers occasionally claimed Book-2 novelty for things Book 1 already contained,
and in one case a spec was cited from the Gracie's Gardens case study instead of the
chapter that actually defines the feature.

The Book-2 refinements that matter were captured correctly and consistently by all
four readers: the Control × Perspective matrix (A-D1/D10, B-D4/D5, D-D5), the
Capture/Clarify/Organize/Reflect/Engage renaming with explicit textual anchors
(A-D2/D3/D4 at 2782–2898, B-D1 at 3894–3896, B-D2 at 5892–5894, C-D5, and D-D1's
correct observation that Appendix iv reverts to Collect/Process/Organize/Review/Do),
and the six Horizons with per-horizon review cadences (B-D3, C-D2, C-28, D-22, D-D2
at 10557–10567). No invented refinements were found except the borderline cases noted
below.

The Gracie's Gardens chapters (10 and 18) were handled well overall: Reader C
explicitly flagged its one case-study-derived item (C-V4) and Reader D anchored its
overview-document spec (D-03) on the *general* recommendation at 9891–9893/9915–9920
rather than on Ron's worked document. The single failure is B2-C-29 (below).

### The three worst errors

1. **B2-B-D6 (OVERREACH)** — claims the "finishable within a year" project bound is a
   Book-2 sharpening. Book 1 already contained the one-year qualifier; nothing at
   5186–5188 marks it as new, and Reader C's own D7 explicitly acknowledges "GTD
   defined projects as multi-step outcomes within a year." The two readers contradict
   each other; C is right, B invented a refinement.
2. **B2-C-29 (EXAMPLE-AS-FEATURE)** — the reference-filing spec is cited entirely from
   the Gracie's Gardens case study (7027: "organizes reference files, sets up folders";
   7083: "good information to keep" — both narrate what Ron does). The real feature is
   the general Reference/Knowledge organizing category, defined in ch.7 at ~4641–4668
   and ~5621–5706 (Reader B's territory, captured there as B2-B-21). The capability is
   real but the citation basis is the applied example, not the definition.
3. **B2-C-D3 (OVERREACH)** — claims Book 2 "codifies" the 3Ds terminology. The source
   (6388–6390) says "the popular '3Ds' in the GTD model—Do, Delegate, or Defer—which
   are described **in more detail in Getting Things Done**," i.e. it presents the 3Ds
   as established Book-1/GTD-community terminology, not a Book-2 sharpening.

---

## Reader A — lines 1–3000

Cleanest of the four. Every spec, delta, and vague item verified at its cited lines.
Reader A correctly demoted the Star Trek voice-assistant scenario (472–503), the
six-week cadence inside it (494–497), and group capture (2955–2959) to Vague rather
than asserting them — exactly right.

| ID | Verdict | Notes |
|---|---|---|
| B2-A-01 | SUPPORTED | 375–384 ("write it all down… trusted 'bucket'"), 1083, 2945–2953. Methodology principle. |
| B2-A-02 | SUPPORTED | 2945–2953 (paper, typed, whiteboard, audio recorder, list or separate sheets). |
| B2-A-03 | SUPPORTED | 510–527. Largely a hardware/habit prescription (METHODOLOGY, partly outside a software tool's control) — reader's note acknowledges this. |
| B2-A-04 | SUPPORTED | 1083–1086, 1155–1158, 1790–1791 ("Finish your thinking" = decide next physical thing). |
| B2-A-05 | SUPPORTED | 1085–1086 ("outcomes and concrete next steps"), 1172–1173, 1052–1056. |
| B2-A-06 | SUPPORTED | 1181–1184 (">1 step = project, by my definition"), 1300–1301. |
| B2-A-07 | SUPPORTED | 1088–1090 (verbatim), 1156–1157. |
| B2-A-08 | SUPPORTED | 1754–1756 ("projects, actions, waiting-for's, and someday/maybe's"). |
| B2-A-09 | SUPPORTED | 1754–1757 ("job responsibilities, goals, visions, and values"). |
| B2-A-10 | SUPPORTED | 1680–1726 (calendar trust; 1721–1723 "translated down to the physical action level"), 1859–1874. |
| B2-A-11 | SUPPORTED | 766–767 ("you must also have a regular review process"), 1091–1093, 1673–1676. |
| B2-A-12 | SUPPORTED | 253–255, 1091–1093 (six horizons enumerated verbatim), 2195–2199. |
| B2-A-13 | SUPPORTED | 1076–1078 (verbatim "trusted choices about what to do (and not do)"), 1359–1368. |
| B2-A-14 | SUPPORTED | 1311–1315 ("time, energy, and location" explicitly named), 1799–1807. |
| B2-A-15 | SUPPORTED (weak citation) | 1799–1807 (hardware-store passage) supports "decide what you need and where" rather than context lists per se; the capability is established outright later (5318–5379, 6572–6584). Reader's "extract the capability, not the anecdote" note is correct. |
| B2-A-16 | SUPPORTED | 1754–1755. |
| B2-A-17 | SUPPORTED (note errata) | 1754–1756, 2210–2213 ("dance lessons"). The note's "coffee-table reading" actually comes from 2595–2596 where it illustrates Crazy-Maker overcommitment, not incubation — note-level slip, spec unaffected. |
| B2-A-18 | SUPPORTED | 1052–1057 ("dad and hospice?" → outcome → next action). Correctly generalized; the example is used as illustration, not promoted as feature. |
| B2-A-19 | SUPPORTED | 2201–2207 ("grow staff" surfaces from areas-of-responsibility review). General principle stated; example used correctly. |
| B2-A-20 | SUPPORTED | 2176–2185 ("three to twenty-four months" explicit). |
| B2-A-21 | SUPPORTED | 1092–1093, 2208–2209. |
| B2-A-22 | SUPPORTED | 1197–1216 (five phases verbatim). |
| B2-A-23 | SUPPORTED | 2223–2227 (definition verbatim). |
| B2-A-24 | SUPPORTED | 1859–1895 ("placeholders for your thinking"; trust = completeness + habit of looking, 1890–1895). |
| B2-A-25 | SUPPORTED | 2961–2999 (mind sweep walkthrough: desk, shelves, drawers, walls, "anything else?"). |
| B2-A-D1 | SUPPORTED | 1320–1333 ("People can't relate to 'out of horizontal'…"), 2017–2056, 2321–2326. |
| B2-A-D2 | SUPPORTED | 1144–1148 (Collect/Process/Organize/Review/Do), chapter titles 70–80 (Capturing…Engaging), 2782–2784, 2871–2873. |
| B2-A-D3 | SUPPORTED | 2895–2898 ("could as easily be called 'collect,' 'clear,' or 'corral'… identifying what's true, now"). |
| B2-A-D4 | SUPPORTED | 76 (ch.8 title "Reflecting"), 2782–2784 ("clearing"). |
| B2-A-D5 | SUPPORTED | 1259–1269 (whiteboard list says "Areas of responsibility") vs 1092–1093 / 2088–2090 ("areas of focus and responsibility"). |
| B2-A-D6 | SUPPORTED | 1229–1318, 2037–2040 (verbatim "amplified to 'clarify our objectives at the appropriate horizon'"). |
| B2-A-D7 | SUPPORTED | 2035–2037 (verbatim), 762–770. |
| B2-A-D8 | SUPPORTED | 1335–1342 ("The horizons actually represent the Planning model applied to a total scenario"). |
| B2-A-D9 | SUPPORTED | 306–316 ("far beyond a mere 'personal organizing system'"), 356–368, 2955–2959. |
| B2-A-D10 | SUPPORTED | 2360–2756 (full matrix chapter; quadrant names verified). Genuinely new vs Book 1. |
| B2-A-D11 | SUPPORTED | 902–934 ("potential meaning overload"; "You can't really manage time"). |
| B2-A-D12 | SUPPORTED | 1539–1599 (Power=Concentration chain), 2799–2806 (ringing-phone reframe). |
| B2-A-D13 | SUPPORTED | 1767–1807 ("Finish Your Thinking" section). |
| B2-A-D14 | SUPPORTED | 2273–2304 ("THE HOAX OF 'LIFE/WORK BALANCE'"; "inherent fallacy"). |
| B2-A-V1–V5 | SUPPORTED | All five exist at cited lines and are appropriately classified as vague/aspirational rather than asserted as specs. |

---

## Reader B — lines 3000–6000

All 39 specs verified. The two errors are in the deltas section, plus three
delta claims that are accurate about the Book-2 text but shaky as *changes vs Book 1*.

| ID | Verdict | Notes |
|---|---|---|
| B2-B-01 | SUPPORTED | 3000–3006 ("separate pieces of paper… focusing clearly on a single item"). |
| B2-B-02 | SUPPORTED | 3008–3015 (Incompletion Trigger list, Appendix i pointer). |
| B2-B-03 | SUPPORTED | 3039–3046 ("use the Horizons of Focus also as a checklist"). |
| B2-B-04 | SUPPORTED | 3073–3082 ("problems… you have yet to recognize as projects"; examples cited as examples, correctly). |
| B2-B-05 | SUPPORTED | 3084–3120; 5165–5177 ("fewer than twenty items"; "one for your job and one for your life"). |
| B2-B-06 | SUPPORTED | 3122–3156 (licensing exam, cruise); 5148–5161 (list in a document suffices). |
| B2-B-07 | SUPPORTED | 3158–3175; 5136–5145 ("treasure maps" or collages; lists + pictorial maps). |
| B2-B-08 | SUPPORTED | 3177–3205; 5097–5133 ("We are at our best when…", credo, index cards → list). |
| B2-B-09 | SUPPORTED | 3236–3242; 4358–4369 ("the project exists as soon as you have a commitment to make the decision"; ~10% start as R&D). |
| B2-B-10 | SUPPORTED | 3249–3255 ("When in doubt, write it down… You can always dismiss it later"). |
| B2-B-11 | SUPPORTED | 3257–3267 (tractor metaphor). |
| B2-B-12 | SUPPORTED | 3305–3344 (brainstorming = mind-sweep on a theme); 3640–3651 ("Collection tools are likely different from those you would use to organize"). Minor: brainstorm output isn't necessarily routed to the inbox in the source; the unconstrained-capture core is accurate. |
| B2-B-13 | SUPPORTED | 3571–3607 (bookmarking; phone-call notes → in-basket → split into actions/reference/discard). |
| B2-B-14 | SUPPORTED | 3533–3569 (Melissa drive-by; "trusting that you'll get to them sometime in the next few hours"). |
| B2-B-15 | SUPPORTED | 4187–4214 ("Is it actionable?"; "'Maybe' is actually 'no, but the item might require action later'"); catalyst framing at 4252–4255. |
| B2-B-16 | SUPPORTED | 4270–4303 (the two questions; "Mom" → party → draft invitee list), 4428–4476. |
| B2-B-17 | SUPPORTED | 4187–4191; 4478–4668 (Meaningless / Hold for later / Reference). |
| B2-B-18 | SUPPORTED | 4547–4591 ("Nothing in this category has a specific next action attached to it—that's a defining characteristic"; list longer than active projects), 5457–5530. |
| B2-B-19 | SUPPORTED | 5457–5569 ("two subcategories… You need two different structures to handle those distinct functions"). |
| B2-B-20 | SUPPORTED | 3989–3990, 4706–4710, 5533–5569 ("determine precisely when you want to be reminded… park the trigger"). |
| B2-B-21 | SUPPORTED | 4641–4668; 5621–5706. |
| B2-B-22 | SUPPORTED | 4216–4242 ("a numbness develops in the psyche"), 5589–5601 ("psychic numbness" — exact phrase at 5594–5595), 5638–5645. |
| B2-B-23 | SUPPORTED | 5181–5202 (30–100; "index"; "identify gaps in action or momentum"). |
| B2-B-24 | SUPPORTED | 5571–5619 (support material ≠ action reminder; active project files more accessible). |
| B2-B-25 | SUPPORTED | 5318–5379 (all eight context lists named as headings). |
| B2-B-26 | SUPPORTED | 5393–5455 ("At sailboat", "Brain-dead", office A/B, broadband; merge lists allowed). |
| B2-B-27 | SUPPORTED | 5381–5391 ("track who's got it, confirm when they got it, and check its status"), 5205–5216. |
| B2-B-28 | SUPPORTED | 5205–5216 ("Outcomes You're Waiting On from Others"); medium confidence is appropriate (Allen frames it as situational, esp. for executives). |
| B2-B-29 | SUPPORTED | 5262–5314 ("only those three items"; all-day-event slot at 5289–5291, 5301–5303). |
| B2-B-30 | SUPPORTED | 4830–4905 ("where things are suits what they mean to you" — 4838–4839), 5022–5051. |
| B2-B-31 | SUPPORTED | 5036–5039 (outcomes/actions/incubating/support/reference/trash) + 5052–5730 subcategory walkthrough. This is the canonical taxonomy as claimed. |
| B2-B-32 | SUPPORTED | 5062–5091 (subdivision examples + overcategorization warning). |
| B2-B-33 | SUPPORTED | 5661–5697 (A–Z index, <60 seconds, fun/easy, purge yearly). Paper-filing best practice transferred to digital — reader's medium confidence and framing are appropriate (METHODOLOGY → tool embodiment). |
| B2-B-34 | SUPPORTED | 5903–5919 ("(a) to update its contents and (b) to provide trusted perspective" — verbatim). |
| B2-B-35 | SUPPORTED | 5197–5202 ("reviewed at least weekly"), 5482–5486, 5946–5967. |
| B2-B-36 | SUPPORTED | 5983–5994 ("the longer the horizon, the longer the time interval… calls… checked a lot more often than your company principles or your yearly goals"). |
| B2-B-37 | SUPPORTED | 4888–4895 (meaning changes with time), 5743–5751, 5957–5967 (active project→someday; Computer action→calendar "have-to" — both examples verbatim). |
| B2-B-38 | SUPPORTED | 5238–5249 ("total-life action list as the foundation of hour-by-hour decision-making, instead of a simple daily To-do list… latest and loudest"). |
| B2-B-39 | SUPPORTED | 3562–3565 (two-minute rule, cross-ref to ch.9). |
| B2-B-D1 | SUPPORTED | 3894–3896 ("In the original GTD workflow model, I referred to this stage as 'processing,' but… 'clarify' is a more encompassing word"); "mind management more than time management" 3890–3892. |
| B2-B-D2 | SUPPORTED | 5892–5894 ("Hence I'm using 'reflect' in place of the original GTD wording of 'review'"), 5969–5982. |
| B2-B-D3 | SUPPORTED | 3039–3205, 5052–5202, 5983–5989. |
| B2-B-D4 | SUPPORTED | 4470–4472, 5969–5994 ("the two dynamics of control and perspective sit closely together" at reflection — verbatim at 5973–5974). |
| B2-B-D5 | SUPPORTED | 4046–4106 (Visionary↔capture, Implementer↔clarify; "Both modalities will short-circuit" 4081–4082), 4244–4255, 5964–5967. |
| B2-B-D6 | **OVERREACH** | 5186–5188 accurately quoted, but the claim that the one-year bound is a Book-2 sharpening is wrong: Book 1 already defined projects as multi-step outcomes completable within a year, and the source text does not present it as new. Reader C's D7 (7706–7709) correctly identifies the actual Book-2 delta: the year bound is *justified by review cadence* and tied to the 30k handoff. |
| B2-B-D7 | **OVERREACH** | 5457–5569 accurately characterized, but Book 1 already had both mechanisms (Someday/Maybe list AND the tickler/43-folders file). The genuine Book-2 element is only the unified framing of "Incubating" as one category with two structurally distinct subcategories — narrower than "a finer distinction than Book 1's single Someday/Maybe bucket." |
| B2-B-D8 | SUPPORTED (caveat) | 5571–5619 accurate; but Book 1 also kept project support material separate from reminders. The delta is one of emphasis/taxonomy placement, not a new distinction. |
| B2-B-D9 | SUPPORTED (caveat) | 5318–5455 accurate; the context-list set is essentially Book 1's set. The defensible delta is the customization framing (sailboat/brain-dead/office A-B), not an "enlarged taxonomy." |
| B2-B-D10 | SUPPORTED | 3269–3445 (journaling, brainstorming, cleaning up, group capture all folded into Capture). Genuine Book-2 broadening. |
| B2-B-D11 | SUPPORTED (caveat) | 3957–4022 accurate, but Book 1 contained a near-identical "stuff" definition; "given a precise operating definition" overstates the novelty. |
| B2-B-D12 | SUPPORTED | 4811–4905 ("a key mistake… the belief that 'getting organized' is one event" 4814–4815; meaning-precedes-organization 4944–4960). |
| B2-B-V1–V7 | SUPPORTED | All seven verified at cited lines; classification as vague/out-of-scope is appropriate in each case (V3's "process projects" at 4371–4396 is a genuinely good catch for org-gtd-habit.el). |

---

## Reader C — lines 6000–9000

This reader owned the Gracie's Gardens case study (ch.10) and handled it almost
perfectly — its specs draw from chapters 8–9 and 11–17, and C-V4 explicitly flags the
one case-study-derived practice. One spec (C-29) slipped: cited from the case-study
narrative. Several citations carry a systematic ~1000-line typo (6519/6521/6524 for
7519/7521/7524) that does not affect verdicts.

| ID | Verdict | Notes |
|---|---|---|
| B2-C-01 | SUPPORTED | 6035–6036 ("at least once a week… best review cycle for projects"), 7785 ("one- to two-hour executive session"), 7796. |
| B2-C-02 | SUPPORTED | 6037–6038, 6588–6595 ("see your eight calls in one quick glance" vs hunting a 150-item list). |
| B2-C-03 | SUPPORTED | 6219–6225 ("deal with Dad's situation" vs "Call Roberta re: Dad's situation"), 6272–6280 (three-question test). Example correctly used as illustration of the general rule — not example-as-feature. |
| B2-C-04 | SUPPORTED | 6264–6266, 6285 ("park the answer in some trusted place"). |
| B2-C-05 | SUPPORTED | 6384–6390 (do <2min / delegate / track remainder; "3Ds"). Applied while processing in-baskets (6406–6409). |
| B2-C-06 | SUPPORTED | 6385, 6392–6394, 6411–6412. Labeling note: methodology says 2-minute items are *done, not stored*; "category" is the org-gtd tool mapping (org-gtd-quick-action.el), which is fine but is TOOL framing layered on a METHODOLOGY rule. |
| B2-C-07 | SUPPORTED | 6386, 6396–6401 ("could involve passing something to a peer or even to a boss"), 6443–6447. |
| B2-C-08 | SUPPORTED (citation erratum) | 6436–6439 verbatim ("tracked on your own lists as agenda topics, organized by the person"). Third citation "6524" should be 7524 ("topics to talk with people about"). |
| B2-C-09 | SUPPORTED (citation erratum) | 6572–6584 ("organized by context… the first criterion that limits your options"). Citations 6519/6522 appear to be typos for 7519–7525 (context list enumeration). |
| B2-C-10 | SUPPORTED (citation erratum) | 7520–7521, 7827–7831 (scan back two weeks and forward; "other people have permission to add items to your calendar"). "6521" → 7521. |
| B2-C-11 | SUPPORTED | 6469–6471 ("three limiting factors"), 6553–6555 ("context, time, and energy"), 6599–6675. |
| B2-C-12 | SUPPORTED | 6586–6588 ("more than 150"), 6631–6633, 6652–6655, 7508–7510 ("more than a hundred"). |
| B2-C-13 | SUPPORTED | 6740–6743, 6755–6765 ("90 percent of my usage… renegotiate with myself"). |
| B2-C-14 | SUPPORTED | 6579–6581 (Read/Review folder during takeoff/landing), 7223–7235 (dedicated Read/Review collection). |
| B2-C-15 | SUPPORTED | 7697–7703 ("outcomes that can be finished within a year that involve more than one action"; 30–100). |
| B2-C-16 | SUPPORTED | 7759–7767 ("one simple list… one per line"), 7822–7825 ("ensure that I have all the operative next actions for each one"). |
| B2-C-17 | SUPPORTED | 7769–7778 ("Install new set of tires"; "Handle summer schedules…"; resistance to personal projects named explicitly). |
| B2-C-18 | SUPPORTED | 7782–7792 ("three distinct times" — verbatim list). |
| B2-C-19 | SUPPORTED | 7798–7846 ("get you clear, current, and creative" + the three named subsections). |
| B2-C-20 | SUPPORTED | 7817–7825 (finished-but-unmarked actions; "circumstances… that have actually morphed into projects"). |
| B2-C-21 | SUPPORTED | 8039–8043 ("ten to fifteen categories"; reader's "~7–15" is a fair composite of 4–7 job hats (8052–8053) + 7–10 life areas (8077–8078)), 8132–8139 ("revisit a high-level checklist every month or so"; "not meant to be finished"). |
| B2-C-22 | SUPPORTED | 8084–8089 ("examine your projects and your actions and ask… 'What area of interest or responsibility does it reflect?'"). |
| B2-C-23 | SUPPORTED | 8191–8215 ("staff development… has to be captured and clarified before it can be acted on"; "self-notification" verbatim at 8211). |
| B2-C-24 | SUPPORTED | 8283–8294 ("next year or two"; "Any project that is likely to take longer than a year to finish should be parked in this category"; monthly/quarterly), 8322–8327. |
| B2-C-25 | SUPPORTED | 8473–8496, 8559–8590 (scenario scripts, treasure maps), 8594–8649 (annual / on major transitions — transition triggers enumerated at 8616–8620). |
| B2-C-26 | SUPPORTED | 8730–8738, 8860–8864 ("clarification of ultimate purpose as the primary criterion for setting priorities"), 8941–8945, 8993–9008. |
| B2-C-27 | SUPPORTED | 8691–8699. Generalized appropriately from Allen's first-person statement. |
| B2-C-28 | SUPPORTED | 7306–7307 ("address the Horizons of Focus in separate, dedicated processes"), 7320–7328 ("one horizon at a time" + capture associative ideas), 7455–7462. Cadences in note verified against the per-horizon chapters and Appendix vii. |
| B2-C-29 | **EXAMPLE-AS-FEATURE** | Both citations (7027, 7083) are the Gracie's Gardens ch.10 narrative — what Ron does, not a stated requirement. The actual feature (Reference category, organized by topic, quickly retrievable) is defined in ch.7 at ~4641–4668 and ~5621–5706 and is already captured as B2-B-21. The general principle is real; this entry should be merged into/deferred to B2-B-21 with corrected citations. |
| B2-C-30 | SUPPORTED (borderline) | 8336–8345 — Allen's personal year-end ritual, generalized; the surrounding text (8329–8334) does generalize ("the beginning of a new cycle… provides a great excuse"). Medium confidence + V5 self-flag make this acceptable; closest call to example-as-feature that survives. |
| B2-C-D1 | SUPPORTED | 6466–6471 (rejects A/B/C, 1-2-3, High/Med/Low), 6881–6896 ("A common criticism of my earlier writings…"; pointer to GTD ch.3 verbatim at 6884–6885). |
| B2-C-D2 | SUPPORTED | 6516–6526, 7455–7462, 7497–8999 (per-horizon scope question / formats / engage cadence all present; the six defining questions verified at 7499, 7693–7694, 8036–8037, 8281, 8473–8474, 8730–8733). |
| B2-C-D3 | **OVERREACH** | 6388–6390 explicitly presents the 3Ds as "the popular '3Ds' in the GTD model… described in more detail in Getting Things Done" — i.e., established terminology cross-referenced to Book 1, not a Book-2 codification. |
| B2-C-D4 | SUPPORTED | 6268–6280 (three-question clarity test, verbatim). |
| B2-C-D5 | SUPPORTED | 6962, 7171 ("capturing, clarifying, organizing, reflecting, and engaging"). Note 6179–6180 actually says "reviewing" — vocabulary wobbles even in the main text, consistent with D-D1's observation. |
| B2-C-D6 | SUPPORTED | 7798–7846. |
| B2-C-D7 | SUPPORTED | 7697–7711 (year bound justified by weekly-review cadence), 8286–8294 (>1yr → 30k). Correctly credits Book 1 with the definition — directly contradicting B2-B-D6. |
| B2-C-D8 | SUPPORTED | 6023–6041 ("how people can link the various items"; "we may actually see software that will provide this capability"; diligent review substitutes). |
| B2-C-V1–V6 | SUPPORTED | All verified; V4's explicit recognition that "What's true?" discovery (7002–7005, 7140–7143) comes from the case study is exactly the right instinct. V6 (family weekly review, 7974–7980) correctly held out as multi-user. |

---

## Reader D — lines 9000–11936

Dense appendix material extracted accurately. The reader correctly noted (scope note +
D-D1) that the appendices preserve the older Collect/Process/Organize/Review/Do
vocabulary — a genuinely valuable catch. Gracie's Gardens Revisited (ch.18) was
handled correctly: D-03 anchors on the general overview recommendation (9891–9893,
9915–9920), with Ron's document as illustration.

| ID | Verdict | Notes |
|---|---|---|
| B2-D-01 | SUPPORTED (minor) | 9026–9031 is the general guidance (credo/affirmations/self-image scripts, ch.17 formats). Secondary citation 9258–9266 is Ron's case-study principles list — corroboration only; primary citation carries the spec. |
| B2-D-02 | **OVERREACH** (mild) | 9033–9036 is *group/family* "rules of engagement" (collaboration, expectations); 9115–9121 is personal values as tough-choice criteria. The reader fused them into a "per-area 'rules of engagement' / standards document" — "per-area" appears nowhere; the source has group rules + a personal values reference point. Substance (stored standards used as decision criteria) is real; the per-area framing is invented. |
| B2-D-03 | SUPPORTED | General recommendation at 9891–9893 ("integrate them into an overview along with all the other frameworks, to gain additional perspective from their relationships") and 9915–9920 ("Having them all close by in some overview, as I do"). 9157–9293 (Gracie's overview document) correctly used as the worked illustration. |
| B2-D-04 | SUPPORTED | 9548–9562 ("AD HOC LIST FUNCTIONALITY… infinite number of lists… each of the Horizons of Focus… easily retrievable"), 9882–9888. |
| B2-D-05 | SUPPORTED | 9475–9509 ("keeping those buckets airtight—not letting anything stray outside of them" 9508–9509), 10462–10471 ("leakproof… as few of these collectors as you can and as many as you need"). |
| B2-D-06 | SUPPORTED | 9728–9738 ("reach to zero with all your input every twenty-four to forty-eight hours"), 9810–9821. METHODOLOGY discipline; reader labeled it as such. |
| B2-D-07 | SUPPORTED | 10473–10485 (Appendix iv decision tree, near-verbatim), 11038 (index). |
| B2-D-08 | SUPPORTED | 10487–10502 ("The four key action categories" — verbatim list). |
| B2-D-09 | SUPPORTED | 10503–10504, 10765–10767. |
| B2-D-10 | SUPPORTED | 10765–10767 ("action folders or bins (e.g. read/review, bills to pay)"); also 10483–10484 ("action folder"). Cited 10514 is the on-hold bullet — off by a few lines, harmless. |
| B2-D-11 | SUPPORTED | 9564–9571, 10511–10512. |
| B2-D-12 | SUPPORTED | 10514–10516 ("Someday/Maybe lists, calendar, tickler" — verbatim), 9904–9913. |
| B2-D-13 | SUPPORTED | 10517–10518 (verbatim). |
| B2-D-14 | SUPPORTED | 10508–10509, 10077–10256, 10325–10456. |
| B2-D-15 | SUPPORTED | 9618–9634 ("site walkarounds"; Incompletion Trigger list pointer), 10077–10256 (full two-column list present). |
| B2-D-16 | SUPPORTED | 9636–9645 ("Finalize personal management system setup… open item on your Projects list"). |
| B2-D-17 | SUPPORTED | 9674–9688 ("should be scheduled on your calendar if it's not already a recurring event"; two-hour block), 9742–9747. METHODOLOGY discipline → calendar capability; correctly framed. |
| B2-D-18 | SUPPORTED | 10573–10588 (verbatim checklist). |
| B2-D-19 | SUPPORTED | 10592–10623 ("ensuring at least one current action item on each" 10616–10617 — the review invariant, verbatim). |
| B2-D-20 | SUPPORTED | 10627–10637 (promote to Projects / "Delete items no longer of interest" — verbatim). |
| B2-D-21 | SUPPORTED | 10520–10523, 10557, 10769–10770. |
| B2-D-22 | SUPPORTED | 10528–10530, 10557–10567 (daily/weekly/monthly/quarterly/annually/annually+ — verbatim), 10732, 10744, 10756. |
| B2-D-23 | SUPPORTED | 10531–10535 ("(context), how much time you have, how much energy you have, and then your priorities" — verbatim ordering). |
| B2-D-24 | SUPPORTED | 10541–10550 (three work modes + "You must sufficiently process and organize to trust your evaluation of the priority of the ad hoc"). |
| B2-D-25 | SUPPORTED | 10259–10304 (Appendix ii, five phases verbatim). Note's claim that the Gracie's example shows the NPM filled in is imprecise (the ch.18 doc is the Horizons, not NPM) — cosmetic. |
| B2-D-26 | SUPPORTED | 10310–10321 (both tuning rules verbatim). |
| B2-D-27 | SUPPORTED | 10325–10456 (all listed sections present, including Creative-thinking prompts). |
| B2-D-28 | SUPPORTED | 10614–10619, 10484–10485, 9853–9855. |
| B2-D-29 | SUPPORTED | 9904–9913 ("I put my vision treasure maps in my tickler file for a random assessment every few months" — personal practice inside a general "consider building in some sort of regular review" recommendation), 10709–10715. |
| B2-D-30 | SUPPORTED | 9904–9907 ("review your annual goals every one to three months"), 10724–10735 ("12-24 months"; "Annually; quarterly reviews"). |
| B2-D-31 | SUPPORTED | 9915–9920 (recast job description / update goals / add vision components — verbatim); "three ways" actually at 9869–9878 (just above cited range) — verified. |
| B2-D-32 | SUPPORTED | 9751–9777 ("Reassess and Refresh Your System Consistently"; spawn project+next action when outdatedness grabs attention — verbatim at 9772–9777). |
| B2-D-33 | SUPPORTED | 9690–9703 ("few of these kinds of conversations will ever happen extemporaneously… planned and scheduled"). |
| B2-D-34 | SUPPORTED | 9796–9821 ("What is the weakest link in the chain?" 9803–9804; default to in-basket clearing with four reasons, 9811–9821). |
| B2-D-D1 | SUPPORTED | 10460–10567 — Appendix iv heading verbs verified as Collect/Process/Organize/Review/Do. The both-vocabularies-coexist observation is correct and important. |
| B2-D-D2 | SUPPORTED | 10557–10567, 10647–10775 (altitude map with explicit per-horizon frequencies — the genuine refinement, correctly identified). |
| B2-D-D3 | SUPPORTED | 10663–10669 ("Priorities are determined from the top down… Each horizon is equally important to clarify, however" — verbatim). |
| B2-D-D4 | SUPPORTED (caveat) | Footnote 4 (11914–11921) verified verbatim (Master and Commander = junior officer). Caveat: the rename is vs Allen's own earlier terminology/seminars — "Master and Commander" never appeared in Book 1's text, so "changed vs Book 1" is loose framing; the whole quadrant model is new (covered by D-D5/A-D10). |
| B2-D-D5 | SUPPORTED | Index terms verified (10994–11018, 11698–11711); full model in Reader A's slice (2360–2756). |
| B2-D-D6 | SUPPORTED | 9422–9448 ("vast majority of all performance improvement is systemic"; "GTD was the first process improvement for productive thinking" — quoted from a client, endorsed by Allen at 9433). |
| B2-D-D7 | SUPPORTED | 9647–9672 ("usually requires an hour a day"; "booked in meetings through an entire day, you will fall at least an hour behind"). (Ch.9 at 6776 says "thirty to ninety minutes a day" — an internal inconsistency in the book, not a reader error.) |
| B2-D-D8 | SUPPORTED (caveat) | All eight terms verified at cited lines. Caveat: "mind like water" is famously Book-1 vocabulary, not newly crystallized here; the item is framed as a glossary rather than strict deltas, so harmless. |
| B2-D-V1–V5 | SUPPORTED | All verified (10506–10507; 10537–10539; 9489–9497 + 9559–9562; 9764–9777; 11874–11899). Classifications appropriate; V5's GTD-Q correctly held at arm's length as marketing with a reusable underlying idea. |

---

## Missed items (completeness check)

Found by full read of chapters 5–9 and 11–17 plus ch.19/appendices. The readers'
coverage is genuinely thorough; these are the residual digital-tool-relevant items not
captured by any reader:

1. **Calendar-first daily orientation / discretionary-time computation** (Reader B
   slice, lines 5271–5296): the calendar "is what I pay attention to first… usually
   open and available for most of that time"; appointments + day-specific to-dos
   "should be the first things you take note of each morning to let you know how much
   discretionary time you have." Tool implication: the engage/agenda view should lead
   with the day's hard landscape and make remaining discretionary time evident.
   Adjacent to B2-B-29/B2-D-21 but the affordance itself was not extracted.

2. **Time-blocking out of the weekly review** (Reader C slice, lines 6619–6625 and
   7839–7843): "block out sufficient time slots for the important actions that require
   them. That is the kind of thoughtful planning that will tend to occur in a weekly
   review mode"; "you had now better block out two hours for yourself in the coming
   week." Tool implication: review flow should support creating calendar blocks for
   large-window actions. Not captured by C-18/C-19.

3. **Project-identification verb checklist** (Reader C slice, lines 7724–7749):
   Finalize / Implement / Research / Publish / … / Resolve, with the explicit
   instruction "just use the above as a checklist" to discover one's projects. This is
   a shippable trigger checklist exactly parallel to the two Appendix lists Reader D
   captured (D-14/D-15/D-27), but it lives in ch.13 and no reader extracted it.

4. **Anti-over-structuring design constraint for GTD software** (Reader A slice,
   lines 2532–2536; reinforced at 4924–4929 in Reader B's slice): "the raft of
   sophisticated software designers who have attempted to create the 'latest and
   greatest' GTD implementation applications have often overshot… requiring too much
   mental effort to make life fit into their supplied forms"; and "if you have created
   structural systems that are unduly complicated… you are also disorganized." A
   direct design warning aimed at tools like org-gtd; B2-B-30/B-32 brush against it
   but the tool-design constraint itself was never logged.

5. **Daily processing time budget stated in ch.9** (Reader C slice, lines 6776–6778):
   "processing the contents of your in-tray… usually takes from thirty to ninety
   minutes a day." Reader D captured the ch.19 "hour a day" restatement (D-D7), so
   this is cross-covered, but C missed the in-slice statement (and the 30–90 figure
   differs from ch.19's).

6. **Physical workstation guidance** (Reader D slice, lines 9573–9605: office/home/
   transit workstations, "cockpit" framing). Physical-environment material with no
   direct digital capability; D omitted it without flagging. Listed for completeness
   only — correctly out of scope.

## Cross-reader consistency notes

- **B2-B-D6 vs B2-C-D7 contradiction** on whether the one-year project bound is new:
  Reader C is correct; Reader B's entry should be amended or dropped.
- B2-C-29 duplicates B2-B-21 with weaker (case-study) citations; consolidate.
- Citation typos in Reader C (6519/6521/6524 → 7519/7521/7524) should be fixed before
  the extractions are used as a requirements source.
- Methodology-vs-tool conflation is rare and minor: B2-C-06 (2-minute items are done,
  not stored — the "category" is org-gtd's affordance), B2-A-03/B2-B-V7 (ubiquitous
  capture is largely hardware/habit), B2-B-33 (paper-filing practice transferred),
  B2-D-06/D-17 (process disciplines a tool can support but not enforce). In all five
  cases the readers' own notes already signal the distinction.
