# GTD Feature Analysis

A verified, traceable analysis of org-gtd.el against David Allen's source material.
Built 2026-06-10 from three primary sources and a from-scratch audit of all prior extraction work.

## The five deliverables

| # | Question | Answer lives in |
|---|---|---|
| 1 | What features should a single-user digital GTD app have? | [`recommended/INDEX.md`](recommended/INDEX.md) — 122 features (REC-\*) + gherkin in [`recommended/features/`](recommended/features/) |
| 2 | What features does org-gtd.el actually implement? | [`implemented/INDEX.md`](implemented/INDEX.md) — 148 features (IMPL-\*) |
| 3 | Which recommended features are NOT implemented? | [`gaps/recommended-not-implemented.md`](gaps/recommended-not-implemented.md) — 73 gaps (30 partial, 21 via-config, 22 not-implemented) |
| 4 | Which implemented features are NOT recommended? | [`gaps/implemented-not-recommended.md`](gaps/implemented-not-recommended.md) — 44 extensions, 55 supporting, 49 recommended |
| 5 | What user workflows does the source material prescribe? | [`workflows/INDEX.md`](workflows/INDEX.md) — 41 workflows (WF-\*) as gherkin acceptance tests in [`workflows/*.feature`](workflows/) |

Fast lookup across everything: [`index/semantic-index.md`](index/semantic-index.md) — 54 GTD concepts → every related ID and file.

Planning the gaps: [`gaps/gap-implementation-strategies.md`](gaps/gap-implementation-strategies.md) — companion to deliverable #3 that sorts every gap by *how* to build it (content to author, implemented mechanics to reuse, core refactors, external Emacs packages, plus docs-only / config-recipe / schema / new-type / hook / orchestration lenses).

## Sources

1. **Getting Things Done** (revised edition text) — `docs/source-material/Getting Things Done….txt` (line numbers in citations refer to this file)
2. **Making It All Work** (2008) — `docs/source-material/making-it-all-work-CONVERTED.txt`. Allen's later refinement: **where the books differ, this one wins** (capture/clarify/organize/reflect/engage naming, horizons over ABC priorities, Matrix of Self-Management).
3. **DA_software.pdf** (1994) — Allen's hand-drawn designs for a GTD app, extracted page-by-page into [`sources/da-software-1994.md`](sources/da-software-1994.md) (41 features DA94-\*, 9 workflows DA94-W\*). New to this analysis; the books supersede it on methodology, but its software/UI ideas (dashboard integrity flags, auto-migration-with-consent, closure loops, mark-done prompts) stand on their own and seeded most of the REC-UI area.

## Verification trail (don't-trust-verify)

Prior work existed and was audited, not assumed:

- [`audit/book1-readers-audit.md`](audit/book1-readers-audit.md) — all 166 specs in the 4 GTD-book reader extractions checked against cited source lines: 158 supported, 7 overreaches, 1 uncited, 0 example-as-feature, ~17 missed source items.
- [`audit/book2-readers-audit.md`](audit/book2-readers-audit.md) — all 193 Making-It-All-Work specs checked: 188 supported, 4 overreaches, 1 example-as-feature, ~6 misses.
- [`audit/reconciliation-audit.md`](audit/reconciliation-audit.md) — the 2026-06-04 merged spec doc checked for traceability: 0 phantom citations, but a missing Trash canonical, ~9 orphaned reader specs, and wrong headline counts.
- [`audit/implementation-status-audit.md`](audit/implementation-status-audit.md) — the 2026-06-04 implementation verdicts re-checked against current code: 62/72 exact, one verdict change (ENG-06 → Partial).

Every correction these audits demanded is applied in `recommended/INDEX.md`, whose changelog records each delta (softened, added, dropped, re-sourced) against the 2026-06-04 doc, including an old→new ID map.

A recurring failure mode the audits screened for: **Allen's concrete examples masquerading as features** (the 1994 "every AA flight → schedule 72-hr upgrade" note is an example of rules-based automation, not an airline feature; Gracie's Gardens is a case study, not a spec). The corpus is clean of these; keep it that way when extending.

## Directory map

```
feature-analysis/
├── README.md                 ← you are here
├── sources/                  ← new primary-source extraction (1994 designs)
├── audit/                    ← verification of all pre-existing work
├── recommended/              ← deliverable 1: INDEX.md registry + features/*.feature (gherkin)
├── implemented/              ← deliverable 2: INDEX.md registry + inventory/ (raw per-module-group)
├── gaps/                     ← deliverables 3 & 4 + gap-implementation-strategies.md (how-to-build companion)
├── workflows/                ← deliverable 5: INDEX.md + *.feature (gherkin) + raw/ (per-book)
└── index/                    ← semantic cross-index (concept → IDs → files)
```

## ID conventions

- `REC-<AREA>-NN` — recommended feature (areas: CAP, CLA, PRJ, NXT, CAL, DEL, SOM, TIC, KNO, TRA, CHK, AGE, REF, ENG, HOR, UI, X)
- `IMPL-NN` — implemented feature (consolidates raw inventory IDs IMPL-WF/VW/GR/DOC-\*)
- `WF-NN` — canonical workflow (merges raw B1-W/B2-W/DA94-W ids)
- `B1-{A..D}-NN`, `B2-{A..D}-NN` — original reader extraction specs (in `docs/source-material/extraction/`)
- `DA94-NN`, `DA94-WNN` — 1994 software-design features/workflows
- Gherkin tags carry IDs (`@REC-CLA-08`, `@WF-21`), provenance (`@src-B2`), type and strength — so scenarios are filterable by any axis.

## Headline numbers

- **122 recommended** (99 tool / 23 methodology; 32 must / 51 should / 39 may)
- **148 implemented** (12 code-only/undocumented, 3 docs-only/stale, 2 latent)
- **49 of 122 recommended are fully implemented**; biggest verified gaps: guided weekly-review walkthrough (REC-REF-02), scheduled review with completion tracking (REC-REF-01), checklist/trigger-list infrastructure (REC-CHK-01), per-person agenda lists (REC-AGE-01), browsable reference index (REC-KNO-05)
- **44 extensions beyond canon**, of which 27 are the project-graph editor suite; 1 direct conflict (trash never deletes — REC-TRA-01's "no residue" clause), 3 mild tensions (multi-project AND-readiness, general DAG dependencies, concurrent clarify buffers vs. one-at-a-time discipline)
- **41 prescribed workflows**, each encoded as gherkin acceptance tests with Allen's decision questions verbatim
