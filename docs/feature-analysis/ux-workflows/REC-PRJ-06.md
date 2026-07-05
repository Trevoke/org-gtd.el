# UX Workflow — Natural Planning Model scaffold

`REC-PRJ-06` · *"I have a project that's still on my mind — help me think it through, not just list steps"* · cluster: `D — scaffolded project creation`

---

## 1. The need (what & why)

GTD's **Natural Planning Model** (purpose → vision → brainstorm → organize → next actions, `WF-34`) is the methodology's answer to a project that won't get off your mind with just a title and one action. Today org-gtd's project creation is purely mechanical (IMPL-040): you must already have `* Heading` + `** task` subheadings, or you hit a teaching-text wall (`org-gtd-projects--malformed`). The *thinking* the model prescribes — and the enduring plan artifact `WF-35` says to "pull out" when a project gets stuck — has nowhere to live. Per **V-04**: ship the scaffold as *helper text / optional structure now*, structured NPM schema in v5.

The user hits this at **organize time**, for the ~20% of projects (`WF-34` calibration: 80/15/5) that need more than an outcome and a next action.

## 2. Entry points & discovery

- **Invoke (proactive):** in the clarify WIP buffer, `C-c C-p` (`org-gtd-project-shape`) — a transient that scaffolds the item into a Natural-Planning project. Advertised in the clarify header-line alongside the existing keys.
- **Invoke (in-flow):** press `p` (new project) in `org-gtd-organize` on an item that has **no** `**` subheadings. Instead of only printing the malformed-teaching text, org-gtd offers: *"This isn't shaped as a project yet — scaffold it? [y] Natural Planning  [m] minimal  [n] show me the format."*
- **Discover:** the `C-c C-p` hint sits in the clarify-mode header-line (the surface users already read every clarify). The `p`-on-unstructured-item branch converts today's dead-end teaching moment into an offered action — you find it exactly when you need it.

## 3. Full-lifecycle walkthrough

**Create / start.** Clarifying "Reorganize the garage" (a fuzzy inbox item) in the WIP buffer. It's clearly a project but you don't yet know the steps. You press `C-c C-p`. The `org-gtd-project-shape` transient opens. You press `f` (full scaffold). org-gtd inserts, under the heading, a `:NATURAL_PLANNING:` drawer pre-seeded with the five phases as prompts, plus one placeholder `** Next action`:

```
* Reorganize the garage
:NATURAL_PLANNING:
Purpose:   Why? What would "on purpose" mean? →
Vision:    Wild success looks/sounds/feels like… →
Brainstorm: (dump freely — don't judge; go for quantity) →
Organize:  significant pieces, sequence, priorities →
:END:
** Next action
```

**See / preview.** The **WIP buffer is the live preview** — the org idiom (`_PRIMER` §2). Toggling a phase in the transient inserts/removes just that line, so you can run the model *partially* (calibration: raise focus for clarity, `WF-35`). No separate preview pane; the thing you're editing is the thing you'll file.

**Edit / reconfigure.** You free-edit the drawer prose. As brainstorm lines firm up, you **triage** (`WF-34` phase 3b) by cutting a line out to a real `** Buy shelving` action heading — planning stays in the drawer, actions become tasks. Re-invoking `C-c C-p` lets you add a phase you skipped or `x` to strip an unused one. This is also the **unsticking** surface (`WF-35`): re-clarify a stuck project (`C-u org-gtd-clarify-item`) and the drawer is right there to "pull out the plan and raise the level of focus."

**Save / name / recall.** File with `C-c C-o` → `p` (project) as normal. The `:NATURAL_PLANNING:` drawer **persists on the project heading** in `org-gtd-tasks.org` — it's reference, invisible to the task machinery (which keys off `**` subheadings only). Recall = visit the project, or re-clarify it to reopen the plan. Nothing new to learn; it's org text under your project.

**Delete / undo / back out.** `C-c C-k` discards the whole WIP (scaffold included) — no trace. Post-file, the drawer is plain text: delete it, or trash the project via the normal cancel-cascade. The scaffold never locks you in.

**Repeat / recur.** N/A per-project. The *pattern* recurs across projects via the transient; nothing to persist between them.

## 4. Interaction sketch

```
 WIP buffer (org-gtd-clarify-mode)          org-gtd-project-shape  [C-c C-p]
┌───────────────────────────────────┐     ┌────────────────────────────────┐
│ Clarify: reorganize garage        │     │  Shape as project              │
│ C-c C-o file · C-c C-p plan · C-c │     │ ── Phases (Natural Planning) ──│
│ C-k cancel                        │     │  p  Purpose & principles    [ ] │
│                                   │     │  v  Vision / outcome        [ ] │
│ * Reorganize the garage           │ ──► │  b  Brainstorm scratch      [✓] │
│ :NATURAL_PLANNING:                │     │  o  Organize outline        [ ] │
│ Brainstorm: (dump freely…) →      │     │  n  Next actions            [✓] │
│   old paint, shelving, donate box │     │  f  Full scaffold (all five)    │
│ :END:                             │     │ ── Naming (PRJ-10) ───────────  │
│ ** Buy shelving                   │     │  t  Verb starters for title…    │
│ ** Next action                    │     │ ── Finish ──────────────────    │
│                                   │     │  RET file as project            │
└───────────────────────────────────┘     │  x  strip empty phases          │
   live: toggling a phase edits buffer     │  q  quit (keep buffer)          │
                                           └────────────────────────────────┘
```

| key | action |
|---|---|
| `C-c C-p` | open `org-gtd-project-shape` from the WIP buffer |
| `p` `v` `b` `o` `n` | insert/toggle that NPM phase in the drawer (raise/lower focus, `WF-35`) |
| `f` | insert all five phases (full scaffold) |
| `t` | verb-starter list for the title (**PRJ-10 shared affordance**) |
| `x` | strip phases left empty (declutter before filing) |
| `RET` | file as project (hands off to `p` / `org-gtd-project-new`) |
| `q` | quit transient, keep buffer as-is |
| `C-c C-k` | cancel clarify entirely (discard scaffold) |

**Live preview:** toggling `p` inserts `Purpose: …→`; toggling it again removes exactly that line. The buffer *is* the preview — identical to how clarify already works.

## 5. Fit with org-gtd

- **Extends:** the **clarify + WIP buffer** `[R]` (`org-gtd-clarify-mode`, `org-gtd-wip`), the **organize transient**'s `p` disposition `[R]` (`org-gtd-project-new` → `org-gtd-project-new--apply`), and the **project-template** idiom `[R]` (`org-gtd-projects-template`, IMPL-018). The scaffold is a second, richer template inserted *into the WIP buffer* rather than at refile.
- **Shared surface / cluster (D):** the single **project-shaping affordance in the WIP buffer**. `REC-PRJ-06` (this doc) supplies the NPM phases; `REC-PRJ-10` supplies the `t` verb-starter title list — *same transient, same `C-c C-p` entry, same "optional scaffold you can ignore" feel*. They must be **one menu with two sections**, not two commands. The confirmed UX constraint: both feel like optional structure the 80%-case user never sees, surfaced only when a project needs more shape. (This confirms the build-route's `PRJ-10 ↔ checklist-engine` split from a UX view: PRJ-10's *engine* is the checklist type, but its *home* is here.)
- **Reuse vs. new:** reused as-is — WIP buffer, header-line hint idiom, org drawers, the `p` filing path, guidance-comment voice. Genuinely new — the `org-gtd-project-shape` transient and the in-flow "scaffold it?" offer replacing the bare malformed-text dead-end.
- **Release tag / rework:** everything leaned on is `[R]`. The one **rework of an `[R]` surface** is the `org-gtd-projects--malformed` teaching text: today it *tells* the user the required shape; we make it *offer to build it*. **Justification (UX + GTD-fidelity):** it converts a dead-end error into an assisted action and imports the Natural Planning Model — core GTD methodology currently entirely absent from the tool.

### Type / extension-UX opportunities

Real one here. The scaffold is hardcoded prose today (near-term V-04). The v5 structured NPM schema wants the Projects **type** to carry the phases as **property descriptors** `[R] mechanism / [U] live-edit` (`:semantic-name` PURPOSE / VISION, `:type text`) so `customize-type` can reshape the scaffold without touching code — and so a `:scaffold`/`:project-template` type slot supplies the phase list. That same slot is what lets `REC-PRJ-10`'s verb list plug into the *same* transient. **Recommendation:** introduce a per-type `:scaffold-fn` (or reuse the `[U]` `:organize-project-fn` slot) so project shaping is registry-driven, not a hand-authored template — and treat the two Cluster-D features as its first two consumers. We add **no new disposition**, so the three-places-in-sync transient friction is untouched.

## 6. Edge cases & failure modes

- **Empty state:** brand-new fuzzy item, nothing structured — this is the *primary* case; the in-flow `p` offer catches it. No scaffold exists until you ask.
- **Plan but no action:** you filled Purpose/Vision but added zero `** ` actions and try to file. Fail-soft: the existing malformed-teaching text fires — *"A project needs at least one action; purpose and vision alone aren't a project. Add a `** ` step, or organize this as a single action / someday item."* Teaching voice, no crash.
- **Huge brainstorm:** the dump can be large; it lives only in the WIP drawer, gets triaged into actions, and `x` strips whatever's left empty. Only next-action headings become tasks — the buffer never explodes the project file.
- **User ignores it entirely:** default mechanical creation is unchanged; the scaffold is opt-in. The 80% never meet it.

## 7. Open questions & maintainer decisions

- **Persist or discard the drawer on file?** Proposed: **persist** `:NATURAL_PLANNING:` as reference (supports `WF-35` unsticking). Alternative: strip brainstorm, keep only purpose/vision. Maintainer call.
- **Drawer vs. `#+begin_comment` vs. body prose** for the plan — which reads best and stays clear of the task machinery? (Drawer proposed.)
- **Does the in-flow `p`-on-unstructured offer default to Natural Planning or minimal?** Leaning minimal-default to protect the 80% case.
- V-04 boundary: how much of the structured schema (property descriptors) lands in v5 vs. stays prose.

## 8. Provenance & links

`REC-PRJ-06` · **Not-implemented** · disposition **V-04** (helper-text now, structured NPM → v5) · build route: `gap-implementation-strategies.md` §8 (new object) + §1 (author content), templates via IMPL-018/IMPL-002 · workflows `WF-34` / `WF-35` / `WF-36` (`workflows/planning.feature`) · cluster **D**, sibling `REC-PRJ-10` (shares the shaping transient; consumes `REC-CHK-01` checklist model for the verb list) · related `REC-PRJ-07` (planning-step-as-next-action / trigger list).
