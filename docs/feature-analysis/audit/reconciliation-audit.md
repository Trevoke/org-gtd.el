# Reconciliation Audit — 2026-06-04-gtd-tool-specs-from-source.md

Doc-to-doc traceability audit of the synthesis document against its 8 reader inputs
(`docs/source-material/extraction/book{1,2}-reader-{A,B,C,D}.md`). The readers themselves
are audited against the books separately; this audit treats the reader files as ground truth.

Audited: 2026-06-10. Method: every reader ID extracted (386 total: 294 specs, 42 deltas,
50 vague items), diffed against every ID cited in the synthesis (295 unique), then each
canonical spec's cited sources checked for meaning fidelity, plus invention, precedence,
coverage, and count checks.

## Summary

| Check | Result |
|---|---|
| Canonical specs checked | 72 spec entries (incl. ORG-00) + 14 Changes-Log rows + 27 V rows + 8 G rows |
| Phantom citations | **0** — all 295 cited reader IDs exist in the reader files |
| Meaning drift in citations | **2 minor** (CLA-03 cites B1-B-09; D-06 renders "3 action options" as "3Ds") |
| Substantive inventions | **0** — the two vague-item promotions (ORG-HAB-01, ENG-06) are explicitly self-flagged |
| Precedence (B1 vs B2) errors | **0 hard errors**; 1 questionable rendering (D-06, see drift) |
| Dangling internal references | **3** (ORG-TRASH-01 never defined; ORG-RR-01 → "V-12" should be V-06; D-13 → "V-13" should be V-08) |
| Orphaned reader specs (vanished, no note) | **~9 substantive** + ~10 borderline (see §4); 91 uncited IDs total, most legitimately deduplicated |
| Count discrepancies | **3 of 4 headline counts wrong or unverifiable** ("~120 raw" vs actual 294; ORG/REF breakdown wrong; 27-V aggregation untraceable). 14-delta count correct. |

Overall: provenance quality of the canonical specs is **high** — every citation resolves and
in spot-checks the cited reader text supports the canonical claim, including the
load-bearing numbers (2-min, 24–48 h, 30–100 projects, 7–15 areas, 100–150+ actions,
~60 s filing, 1–2 h review). The document's weaknesses are (a) its self-reported counts,
(b) a missing Trash canonical that the doc itself dangles a reference to, (c) the §5
needs-review table carrying no reader provenance at all, and (d) a tail of genuinely
vanished low-severity reader items.

---

## 1. Provenance check (per canonical)

All citations in CAP-01..06, CLA-01..07, ORG-00, ORG-PRJ-01..08, ORG-SA-01..02,
ORG-CAL-01..02, ORG-DEL-01..04, ORG-INC-01..04, ORG-KNOW-01..03, ORG-HAB-01, ORG-QA-01,
ORG-RR-01, REF-01..06, ENG-01..06, HOR-01..08, X-01..12 were resolved against the reader
files. Every cited ID exists (zero phantoms), including the range citation
"B1-C-28 through B1-C-37" in REF-02 (all ten IDs exist and match the Get Clear/Current/
Creative steps). Specific details verified to match their cited source, e.g.:

- CAP-05/X-06 "load-bearing for Waiting-For follow-up" ← B1-B-04 note, verbatim intent.
- ORG-PRJ-06 "~80% of projects need only outcome+next-action" ← B1-A-38 note; the
  off-your-mind heuristic ← B2-D-26.
- ORG-CAL-01 "supports others adding items to your calendar" ← B2-C-10 note.
- ORG-DEL-02 "date is the most crucial field" ← B1-B-19.
- ORG-KNOW-02 60-second rule + yearly purge ← B1-B-39/41, B2-B-33.
- REF-04's three review triggers ← B2-C-18, exact.
- HOR-01 "fewer than twenty items" ← B2-B-05; "~7–15" ← B2-C-21.
- X-05 "checked daily, with look-ahead before being away" ← B1-C-22.
- ORG-HAB-01 and ENG-06 promotions from B2-B-V3 / B1-D-26+B2-D-24 are honestly annotated.

### Meaning drift found (2, both minor)

1. **CLA-03 cites B1-B-09** for the "Is it actionable?" gating question, but B1-B-09 is
   the "What's the next action?" question (it properly supports CLA-04, where it is also
   cited). The correct Reader-B source for the actionable/non-actionable branch is
   **B1-B-10**, which the synthesis never cites anywhere. Wrong ID, right reader; content
   is still fully sourced via B1-A-05/06, B2-A-04, B2-B-15.

2. **Changes Log D-06** states Book 2's priority model as "6 Horizons × 3 limiting factors
   (context/time/energy) × **3Ds**". The source delta B2-C-D1 says "× 3 **action
   options**". In Book 2 the three engage-time action options are the threefold-work modes
   (predefined / ad-hoc / defining — which the synthesis itself canonicalizes as ENG-06
   via B2-D-24), not the 3Ds (Do/Delegate/Defer), which are a *clarify*-stage dispatch
   (B2-C-D3, B2-C-05). The "3Ds" rendering is an unsupported interpretation that
   contradicts the doc's own ENG-06. Low impact (D-06's tool conclusions still hold), but
   it is the one place the Changes Log misstates a reader.

### Internal cross-reference defects (3)

1. **ORG-TRASH-01 is referenced but never defined.** The CLA section blockquote (line 99)
   routes non-actionable items to "ORG-TRASH-01, ORG-INC-01, ORG-KNOW-01" — there is no
   Trash subsection or spec anywhere in the document. See §4 (this is also a coverage gap:
   B1-A-07, B1-B-11, B2-B-17's trash leg have no canonical home).
2. **ORG-RR-01 heading says "category mapping unsettled — see V-12"** — V-12 is "Granular
   Someday/Maybe subcategories". The Read/Review adjudication is **V-06**.
3. **Changes Log D-13 says "see V-13"** for the Knowledge-vs-Support-Material question —
   V-13 is "Subdividing the Projects list". The Reference-vs-Support adjudication is **V-08**.

### Structural traceability defects

- **§4 Changes Log table rows carry no reader citations.** All 14 rows were verified by
  content against reader deltas (D-01←B2-A-D3/B2-B-D10; D-02←B2-B-D1; D-03←B2-B-D2/B2-A-D4;
  D-04←B2-C-D5; D-05←B2-A-D1; D-06←B2-C-D1; D-07←B2-A-D7/B2-B-D12; D-08←B2-C-D2/B2-D-D2;
  D-09←B2-A-D8; D-10←B2-C-D3; D-11←B2-B-D6/B2-C-D7; D-12←B2-B-D7; D-13←B2-B-D8;
  D-14←B2-C-D4) — all check out except the D-06 drift above — but the table itself is not
  self-tracing; only the terminology-only paragraph below it cites IDs.
- **§5 needs-review table (V-01..V-27) carries no reader citations at all.** 25 of 27 rows
  could be mapped back to reader vague items / specs by content during this audit; the
  mapping cannot be verified from the document itself, and the claim "aggregated from all
  8 readers" is not checkable by a reader of the doc.

## 2. Invention check

No substantive invented requirements found. All quantitative claims, named examples
("Purge boat shed", "Mom's 60th", "Call Roberta re: Dad", "research life coaches",
"At sailboat", "Brain-dead"), and design constraints in the canonicals trace to cited
reader text. org-gtd flavoring (module names, org-edna, `Effort` property, inbox.org,
SCHEDULED timestamps) is pervasive but consistent with the reader files' own org-gtd
mapping notes and is the document's stated purpose. The two borderline promotions
(ORG-HAB-01 from vague B2-B-V3; ENG-06 from B1 vague + B2-D-24) are both explicitly
annotated as promotions with rationale — exemplary practice, not invention.

One soft tension worth noting (not an invention): **G-HOR-07** drops the unified
cross-horizon outline as "over-literal... Allen favors bottom-up", while the doc's own
HOR-07 cites B2-D-03 at confidence *clear* ("Maintain a single linked 'Overview of my
life' document... 'having them all close by in some overview'"), and HOR-08/D-06 declare
top-down priority canonical. G-series rows are user adjudications (allowed to overrule
specs), but the stated rationale conflicts with the doc's own sourced reading of B2-D-03.
The drop decision would be better grounded on B2-C-D8 (Allen: diligent review makes
cross-references largely unnecessary) — which the synthesis never cites (see §4).

## 3. Precedence check (Book 2 supersedes Book 1)

Verified across all reconciliation points; no inversions found:

- Stage vocabulary: Capture/Clarify/Organize/Reflect/Engage canonical (D-01..04) ✓, with
  the Book-2 appendix vocabulary conflict honestly disclosed via B2-D-D1 (§1.2 note) ✓.
- Control × Perspective replaces horizontal/vertical (D-05 ← B2-A-D1) ✓.
- ABC/123 priorities replaced by horizons + limiting factors (D-06 ← B2-C-D1) ✓ in
  substance; "3Ds" wording drift noted in §1.
- Project 1-year bound as review-cadence rule dividing Projects from Goals
  (D-11 ← B2-B-D6/B2-C-D7; carried into ORG-PRJ-01, HOR-03) ✓.
- Someday/Maybe split into review-list vs calendared later-starts
  (D-12 ← B2-B-D7; carried into ORG-INC-02, X-05) ✓.
- Support Material as own category (D-13 ← B2-B-D8): correctly logged as a B2 delta; the
  org-gtd resolution (unified Knowledge + links, V-08) is a documented user adjudication,
  not a silent B1-precedence error ✓ (but its cross-ref points at the wrong V row, §1).
- Terminology deltas (Captain & Commander ← B2-D-D4; areas of focus ← B2-A-D5;
  potential-meaning overload ← B2-A-D11; work-life-balance-as-hoax ← B2-A-D14) all match ✓.
- Review cadences per altitude: §1.3/REF-05 blend B2-C-28 ("monthly/quarterly" for goals)
  with B2-D-22/B2-D-D2 ("quarterly") as "monthly–quarterly" — a fair merge of two B2
  readers, not a precedence issue ✓.

## 4. Coverage check — vanished reader items

91 of 386 reader IDs are never cited. Most are legitimate dedup (content fully covered by
a cited sibling reader's near-identical spec) or vague items mapped by content into §5/§6
rows. After triage, the following have content **absent from the canonicals with no
merge/supersede/adjudication note**:

### Substantive orphans

1. **Trash/discard canonical missing** — B1-A-07 (route non-actionable to trash),
   B1-B-11 (discard with no further tracking), and the trash leg of B1-B-10/B2-B-17.
   The doc *intends* this spec to exist (it references ORG-TRASH-01) but never wrote it.
   This is the only missing member of the doc's own ORG-00 taxonomy.
2. **B1-C-38** — staleness/freshness detection: out-of-date lists break trust ("Calls list
   must be totally current"); implies staleness signaling. REF-04/REF-06 are adjacent but
   neither states the freshness invariant.
3. **B1-C-08 + B1-C-10** — Weekly Review must surface each project's *support material*
   and scan notes for inherent next actions. REF-02's Get Current phase lists projects,
   calendar, Waiting For, checklists — but not the support-material scan (B1-C-34's
   "browse plans/support material" clause is cited in REF-03 yet the canonical text drops
   the support-material wording).
4. **B1-B-38** — *all* reminder locations must be reviewed equally and regularly so
   nothing entrusted goes unseen (an explicit system invariant, stronger than REF-01).
5. **B1-A-35** — optional informal "if I have time I'd like to…" short list distinct from
   calendar have-tos (medium confidence in reader; vanished without note).
6. **B2-C-D8** — Allen's explicit design stance on cross-list linking (diligent review
   substitutes for manual cross-references; software may eventually provide it). Directly
   relevant to HOR-07 / G-HOR-07 / G-HOR-345 and X-10, never mentioned.
7. **B2-D-D7** — Book 2's explicit *daily* processing time budget (~1 h/day to stay
   current) with the reader's stated design implication ("make daily inbox-clearing
   fast"). CLA-01 has the 24–48 h target but the daily-budget delta vanished.
8. **B1-D-V5** — renegotiate/complete/forgive broken agreements as a clarify decision
   (vague in reader, but flagged there precisely to avoid silent dropping — and it was
   silently dropped).
9. **B2-C-V4** — "current-reality and distractions inventory" as a repeatable capture
   practice; no V row, no canonical, no note.

### Borderline / low-severity orphans (uncited, mostly-covered or philosophy)

- B1-B-15 (3Ds = exactly three dispositions — content in §1.4/CLA blockquote, ID uncited)
- B1-C-25 (overview checklists for unfinishable standards — largely HOR-01/X-07)
- B1-C-50, B1-D-02, B1-D-10, B2-A-09, B2-A-12, B2-A-23/B2-C-17 (personal items
  first-class — implied by CAP-01/ENG-01 "personal + professional" but the
  same-list-same-cadence claim of B2-C-17 is nowhere explicit), B2-A-24, B2-B-12, B2-C-13
- B1-D-V1 ("weird time" micro-window view), B1-D-V2 (relationship-reflection prompt
  lists), B2-D-V1 (horizons-alongside-action-lists ambiguity) — vague items with no §5 row
- Deltas with no tool impact, uncited and unmentioned: B2-A-D9 (GTD as universal model),
  B2-A-D13 ("finish your thinking" — content effectively inside CLA-04), B2-B-D5
  (matrix applied to capture/clarify), B2-A-D2 (stage-name transitional evidence),
  B2-D-D6 (systemic-improvement framing), B2-D-D8 (glossary vocabulary)

### Vague-item aggregation accounting

Readers contain 50 vague items; §5 has 27 rows. By content-matching, 25 of 27 rows map to
one or more reader vague items or demoted specs (V-16 ← spec B1-B-37; V-17 ← spec B1-B-35;
V-10/V-25 partially from canonicals' source specs). Reader vague items with **no** row and
no covering canonical/note: B1-D-V1, B1-D-V2, B1-D-V5, B2-C-V4, B2-D-V1 (and arguably
B1-C-V4, B1-C-V8, B1-A-V5, B2-A-V5 — though their own readers self-describe them as
redundant). So "aggregated from all 8 readers" is roughly true but leaky, and unverifiable
from the doc since §5 cites nothing.

## 5. Count verification

| Claim (§0 Counts) | Actual | Verdict |
|---|---|---|
| "Canonical specs: 71 (CAP 6, CLA 7, ORG 24, REF 8, ENG 6, HOR 8 + X 12)" | 72 defined entries: CAP 6 ✓, CLA 7 ✓, **ORG 27** (ORG-00 + PRJ 8 + SA 2 + CAL 2 + DEL 4 + INC 4 + KNOW 3 + HAB 1 + QA 1 + RR 1), **REF 6**, ENG 6 ✓, HOR 8 ✓, X 12 ✓ | **Wrong breakdown.** Total reconciles to 71 only if ORG-00 is excluded as a taxonomy preamble (then ORG = 26, still ≠ 24, and REF = 6 ≠ 8). Looks like a stale summary from an earlier draft where 2 specs lived under REF. |
| "~120 raw reader specs" (~95 collapsed into the 71) | **294 spec IDs** across the 8 readers (B1: 45+43+50+28 = 166; B2: 25+39+30+34 = 128); 386 IDs including 42 deltas and 50 vague items. 295 unique IDs are actually cited by the synthesis. | **Wrong by >2×.** No reading of the reader files produces ~120. The "~95 collapsed" arithmetic (120−95 = 25 ≠ 71) is internally incoherent as well. Ironically the doc's real consumption (295/386 cited) is *better* than it claims. |
| "14 reconciliation deltas (Changes Log §4)" | D-01..D-14 = 14 rows | **Correct.** |
| "Vague/Needs-Review: 27, all adjudicated" | §5 has exactly 27 rows, each with a decision and disposition; rollup sections re-list all 27 consistently | **27 rows confirmed**, but the aggregation from 50 reader vague items is untraceable (no citations) and ≥5 reader vague items have no row (see §4). |

G-series (§6): 8 rows (G-REF-02, G-REF-06, G-REF-06b, G-HOR-07, G-HOR-06, G-HOR-345,
G-DEL-03, G-CLA-06) — the §6 intro says "These 7", which is correct if G-REF-06b is
counted as a sub-item of G-REF-06; mildly inconsistent labeling, not a traceability error.

## 6. Recommended fixes (ordered)

1. Add the missing **ORG-TRASH-01** canonical (provenance: B1-A-07, B1-B-11, B1-B-10,
   B2-B-17, B2-D-07) — the doc already promises it.
2. Correct the §0 counts: 294 raw reader specs (386 IDs incl. deltas/vague); ORG 26+taxonomy /
   REF 6 (or recount after fix 1).
3. Fix the three cross-references (ORG-RR-01 → V-06; D-13 → V-08; CLA-03 cite B1-B-10).
4. Re-render D-06 as "× 3 action options (predefined/ad-hoc/defining, see ENG-06)".
5. Add a provenance column to §5 (and ideally per-row citations in §4).
6. Adjudicate or explicitly drop the §4 substantive orphans (Trash aside): B1-C-38,
   B1-C-08/10 support-material review step, B1-B-38, B1-A-35, B2-C-D8, B2-D-D7, B1-D-V5,
   B2-C-V4, B1-D-V1/V2, B2-D-V1.
