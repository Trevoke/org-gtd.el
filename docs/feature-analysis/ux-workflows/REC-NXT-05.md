# UX Workflow — Read/Review as a someday subcategory

`REC-NXT-05` · "I have reading material to get through when I have a spare pocket of time — but it isn't reference I keep, and it isn't a next action I'll do at my desk." · cluster: `F — optional classification metadata`

> The user has articles, PDFs, a book chapter, a long email thread they mean to *consume*. In GTD this is the **Read/Review** list: not Reference (which you keep to look up), not a next action (no single physical do), but a bucket you graze when you have low-energy time. Today org-gtd has no distinct home for it, so it silts up into Knowledge or clogs Next Actions.

---

## 1. The need (what & why)

- **Problem / who:** Reading material is a real GTD category that most users mis-file. It is *actionable-ish* ("read this") but has no discrete next step and no deadline, so it belongs on a someday-style list you sweep — yet, unlike pure Someday/Maybe, it wants to be *reachable in spare moments*, not buried until the weekly reflect. The user needs a named Read/Review queue that is **distinct from Reference** and **distinct from daily Next Actions**.
- **When they hit it:** at **organize** time, disposing an inbox item that is "stuff to read," and again at **engage** time when they have 15 idle minutes and want to pull from that queue.
- **Source:** `REC-NXT-05` (Not-implemented) · disposition **V-06: model as a Someday/Maybe subcategory**, reusing the named-someday-lists hook (IMPL-033, `org-gtd-someday-lists`, shipped 4.0).

## 2. Entry points & discovery

- **Invoke (attach):** clarify → `C-c C-o` → **`y` someday** → the existing "Someday list:" completion now includes **Read/Review**. No new transient key — Read/Review *is* a someday sub-list, reached through `y`.
- **Invoke (consume):** `M-x org-gtd-engage-reading` (new) — a one-block DSL view of the Read/Review list; and `org-gtd-reflect-someday-review` → pick **Read/Review** at the list prompt (works today for free).
- **Discover:** (1) the list name appears the first time the user picks `y` after opting in — no manual to read; (2) a **command-center** entry under Engage: `r  Read/Review queue`; (3) the generated someday guidance header names Read/Review as the canonical reading bucket, "not Knowledge/Reference." Opt-in is one line: `(setq org-gtd-read-review-list "Read/Review")`.

## 3. Full-lifecycle walkthrough

**Create / start** — Process an inbox item ("read Ubiquity Language essay"):
1. `org-gtd-process-inbox` opens it in the WIP buffer.
2. `C-c C-o` → organize transient → **`y`**.
3. Prompt `Someday list:` → type/complete **Read/Review** → `RET`.
4. `org-gtd-someday--organize` runs: stamps `ORG_GTD: Someday`, `ORG_GTD_SOMEDAY_LIST: Read/Review`, clears TODO, strips timestamps; files to someday storage; continuation pulls inbox item N+1.

**See / preview** — Attaching is a property write, so there is no preview *at attach* (identical to sibling X-04's ENERGY). The preview surface is the **consume** side: `org-gtd-engage-reading` renders the live list; `someday-review` shows one item at a time with running stats.

**Edit / reconfigure** — Re-clarify the item (`org-gtd-clarify-item`, or `C-u` update-in-place) and re-`y` to pick a different list; or change `ORG_GTD_SOMEDAY_LIST` directly. To promote a piece to actually-do-now, **reactivate** it (`org-gtd-reactivate`) then clarify to a next action.

**Save / name / recall** — Persistence is the org property itself (crash-safe, git-friendly). Recall = `org-gtd-engage-reading` or the someday-review list picker. No separate store to manage.

**Delete / undo / back out** — Inside the review session: `c` clarify it out (turn into a project/action), or mark it done/trash when read; from anywhere, move it to a different someday list or archive the subtree. Aborting the organize path is the universal `C-c C-k` — the item stays in the inbox untouched.

**Repeat / recur** — N/A: reading items are one-shot; there is no recurrence. (If a periodical recurs, that's a Habit, not this list.)

## 4. Interaction sketch

**Attach (organize path) — identical to any someday-list disposition:**
```
┌─ *Org GTD Clarify* ─────────────────────────────────────────┐
│ Clarify item.  C-c C-o to file · C-c d dup · C-c C-k cancel  │
│ * read Ubiquity Language essay                                │
└──────────────────────────────────────────────────────────────┘
   C-c C-o
┌─ Organize ───────────────────────────────────────────────────┐
│ Non-actionable   i tickler   y someday   k knowledge   t trash│
└──────────────────────────────────────────────────────────────┘
   y
Someday list: [Read/Review | Learning | Travel | ...]  ← completing-read
   RET
✔ filed → ORG_GTD_SOMEDAY_LIST: Read/Review
```

**Consume (engage the queue):**
```
┌─ GTD · Read/Review ──────────────────────────────────────────┐
│ Read/Review — 6 items                                         │
│  DDD — Ubiquity Language essay          (blog)                │
│  Team — Q3 architecture RFC             (email)               │
│  Book — Ch.4 "Aggregates"               (pdf)                 │
└──────────────────────────────────────────────────────────────┘
```

**Keymap**

| Surface | Key | Action |
|---|---|---|
| organize transient | `y` | someday → triggers the Someday-list prompt |
| list prompt | `TAB`/text | complete to **Read/Review** |
| someday-review session | `d` | defer (keep on list, log Reviewed) |
| someday-review session | `c` | clarify out (promote / re-file) |
| someday-review session | `q` | quit session, restore windows |
| engage-reading view | agenda keys | `RET` visit · `t` todo · standard org-agenda |
| command-center | `r` | open Read/Review queue |

**Live preview** — none at attach (a property, not a composed object). The DSL view *is* the live surface: change `org-gtd-read-review-list` or re-file an item and re-invoke `org-gtd-engage-reading` to see the block update.

## 5. Fit with org-gtd

- **Extends:** the **someday disposition** (`org-gtd-someday--organize`, someday.el:73) and its list-prompt hook (`org-gtd-someday-lists`, IMPL-033); the **reflect someday-review** session engine (`org-gtd-reflect-someday-review`); the **view DSL** for the engage-reading block; a **command-center** entry for discovery.
- **Shared surface / cluster (F):** sibling **REC-X-04 (ENERGY)**. Both attach an *optional classification during organize* and *consume it later through existing surfaces* with **no bespoke buffer**. The attach interaction must feel **identical**: one `completing-read`/property prompt on an existing disposition, off by default, then a DSL filter to read it back. Where NXT-05 legitimately diverges from generic someday (and from X-04): it earns an **engage-time** surface, because reading material is meant to be grazed in spare moments, not sequestered until the weekly reflect — a GTD-fidelity distinction, not new machinery.
- **Reuse vs. new:** *Reused as-is* — the someday organize path, the `ORG_GTD_SOMEDAY_LIST` property, the someday-review session and its `d`/`c`/`q` map (must stay identical to Cluster A). *New* — a `org-gtd-read-review-list` defcustom (ships the canonical name, unions it into the someday completion candidates), a thin `org-gtd-engage-reading` DSL view, and the command-center row.
- **Release tag:** builds on `[R]` `org-gtd-someday-lists` (4.0), `[R]` someday disposition, `[R]` view DSL, `[R]` someday-review. No `[R]` rework required — purely additive, so no fidelity justification is owed.

### Type / extension-UX opportunities

- **First-class `someday-list` DSL filter key.** Today the engage view must match the raw `property` (`ORG_GTD_SOMEDAY_LIST`). Adding `someday-list` to `org-gtd-view-lang--known-filter-keys` (+ a `read-review` computed type default) makes `(someday-list . "Read/Review")` legible and self-documenting. Small, high-clarity.
- **De-duplicate the list prompt.** The completer is hand-copied in `org-gtd-someday.el:80-82` **and** `org-gtd-projects.el:1259-1261` — the exact hand-sync drift the primer warns about. Extract `org-gtd-someday--read-list` once.
- **Cluster-F unification (the real prize):** model **both** `ENERGY` (X-04) and `SOMEDAY_LIST` (NXT-05) as type **`:properties` descriptors** with an `:input-fn` completer, so the organize-time prompt is registry-driven and *provably identical* for both features rather than two hand-coded prompts. This is the "attaching optional metadata feels identical regardless of attribute" contract, made structural.

## 6. Edge cases & failure modes

- **Empty state:** `org-gtd-read-review-list` unset → no prompt appears (honors "off by default"); `org-gtd-engage-reading` shows an empty block with a one-line guidance footer: "No Read/Review items. Organize a someday item and pick the Read/Review list." Teaching, not error.
- **Bad input:** the list prompt is `require-match`, so a typo can't create a stray list; picking `Read/Review` when the feature is off still works (it's just a string). Large queues render fine — it's one agenda block.
- **When it goes wrong:** if a user filed reading into Knowledge previously, nothing crashes — the engage view simply won't show it; the migration is "re-clarify → `y` → Read/Review." Message-level guidance only, never a stack trace.

## 7. Open questions & maintainer decisions

- **Default value:** ship `org-gtd-read-review-list` = `nil` (strict cluster-F "off") vs. = `"Read/Review"` (the feature is literally "ship the list")? Recommendation: default `nil`, document `"Read/Review"` as the one-line opt-in — keeps the no-prompt promise for users who never wanted lists.
- **Engage placement:** is a *dedicated* `org-gtd-engage-reading` warranted, or just a documented DSL snippet + command-center entry? (I argue dedicated, for discovery and for the spare-moments use case.)
- **Descriptor migration:** adopt the cluster-F `:properties` descriptor unification now (couples X-04 + NXT-05) or ship both as hand-coded prompts and unify in synthesis?

## 8. Provenance & links

`REC-NXT-05` · Not-implemented · `gap-implementation-strategies.md` §7 (schema addition) + V-06, reuses IMPL-033 named someday lists · cluster **F** sibling `REC-X-04` · consume-side shares the Cluster-A session engine (`org-gtd-reflect-someday-review`) and its `d`/`c`/`q` map · related `WF` surfaces: organize.feature (someday disposition), reflect.feature (someday review).
