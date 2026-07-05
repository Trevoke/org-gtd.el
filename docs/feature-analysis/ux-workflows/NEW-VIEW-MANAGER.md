# UX Workflow — NEW-VIEW-MANAGER

`NEW-VIEW-MANAGER` · *Create, preview, name, recall, and delete a DSL view without writing elisp* · cluster: `E — Named-object CRUD managers`

> The complete UX for turning the view DSL from an init.el-only, eval-and-throw-away literal into a set of **named objects a user manages interactively** — the flagship "manage my X" pattern. This doc is Wave-0: it **sets** the CRUD-manager idiom that `REC-CHK-01` (and later HOR-07, KNO-06) inherit.

---

## 1. The need (what & why)

- **Problem:** org-gtd already has a powerful view DSL (per-item `prefix` resolution org-agenda can't express — the reason org-ql was rejected), but **a view exists only as an elisp alist inside a `defun` or a one-off `org-gtd-view-show` call**. To make a custom engage view you edit `init.el` and re-eval. That is the "folders on paper" that never got a digital drawer.
- **Who / when:** a practitioner who, mid-reflect or mid-engage, wants "show me next-actions in area *Home* with effort < 30m" — and wants to *keep* it, name it "Weekend errands," and pull it up next Saturday. Today that requires being an elisp programmer.
- **Source:** net-new exemplar (primer §3 "How views live today"); dramatizes the persistence/CRUD gap.

## 2. Entry points & discovery

- **Invoke:**
  - `M-x org-gtd-view-manager` — the top-level list transient.
  - `org-gtd-command-center` gains a **`v` "Views…"** entry (Engage column) → opens the manager. This is the discovery surface — a user browsing the hub sees it.
  - `org-gtd-view-run` (autoloaded) — `completing-read` over saved names, renders immediately (bind-it-yourself, no default key, per primer §5).
- **Discover:** the manager's empty state (see §6) is self-teaching, and every reflect/engage view a user already runs is "one of these, but built-in" — the manager says "make your own."

## 3. Full-lifecycle walkthrough

Primary path — build "Weekend errands," save, recall, delete.

- **Create / start.** In `org-gtd-view-manager`, press **`c`**. A fresh in-memory spec `((name . "Untitled") (type . next-action))` opens the **builder transient** `org-gtd-view-manager--build`. The builder is a transient over `org-gtd-view-lang--known-filter-keys` grouped by DSL family (Type / Time / Structural / Metadata / Prefix).
- **See / preview.** The builder's top group is a live infix summary of the spec. Press **`RET` (Preview)** at any point: org-gtd calls `org-gtd-view-show` on the current spec in the **other window**, so real data renders beside the transient (window config snapshotted first, primer §5). Change any infix and re-preview — the DSL *is* the preview engine, no separate renderer.
  1. `t` → type → `next-action`.
  2. `A` → area-of-focus → `Home` (completing-read over `org-gtd-areas-of-focus`).
  3. `e` → effort → `< 30m` (reuses the DSL duration-regex input).
  4. `x` (Prefix) → `(project area-of-focus "—")`.
  5. `RET` → the agenda block renders with the per-item prefix. Adjust, re-preview until right.
- **Edit / reconfigure.** From the manager list, highlight a saved view and press **`e`** — re-opens the *same* builder on its stored spec. No difference between "new" and "edit" except the starting spec (mirrors clarify's open-then-edit).
- **Save / name / recall.** In the builder press **`s`**: prompt *"Name this view:"* → "Weekend errands." The spec is written to the **`org-gtd-saved-views` defcustom** (a `name → spec` alist, persisted via `customize-save-variable`). Recall later: `org-gtd-view-manager` lists it under **"Your saved views"**; `RET` renders it; or `org-gtd-view-run RET Weekend errands`.
- **Delete / back out.** In the list, highlight and press **`D`** → *"Delete view 'Weekend errands'? (y/n)"* → removed from `org-gtd-saved-views`. Uppercase = forceful variant (primer §5). Backing out of an unsaved build: **`C-c C-k`** discards and restores windows; if the spec is dirty, one guard prompt *"Discard unsaved view? (y/n)."*
- **Repeat / recur.** N/A as recurrence — but a saved view is inherently re-runnable; that *is* the recall path above.

## 4. Interaction sketch

**Manager (list) transient**
```
╭─ org-gtd View Manager ──────────────────────────────╮
│ Your saved views                                    │
│   RET  Render highlighted view                      │
│   →  Weekend errands   (next-action · Home · <30m)  │
│      Waiting on Sam    (delegated · who=Sam)        │
│      Big rocks         (project · not-done)         │
│                                                     │
│ Manage                                              │
│   c  Create new view      e  Edit highlighted       │
│   D  Delete highlighted   d  Duplicate highlighted  │
│ q  Quit                                             │
╰─────────────────────────────────────────────────────╯
```

**Builder transient (with live preview in other window)**
```
╭─ Build view: Weekend errands ───────╮   ┌ *Org GTD View* ─────────────┐
│ Spec  next-action · Home · <30m     │   │ Home — Buy paint            │
│ Type      t next-action             │   │ Home — Fix gate hinge       │
│ Time      w when   D deadline       │   │ Errand — Return library book│
│ Struct    o todo   O done   N !done │   │                             │
│ Meta      A area=Home  e effort<30m │   │ (renders via                │
│           W who   G tags   P prio   │   │  org-gtd-view-show)         │
│ Prefix    x (project area-of-focus) │   └─────────────────────────────┘
│                                     │
│ RET Preview   s Save   C-c C-k Abort│
╰─────────────────────────────────────╯
```

**Keymap**

| Surface | Key | Action |
|---|---|---|
| Manager | `RET` | Render highlighted saved view (via `org-gtd-view-show`) |
| Manager | `c` / `e` | Create new / edit highlighted → builder |
| Manager | `d` / `D` | Duplicate / delete highlighted (D confirms) |
| Manager | `q` | Quit, restore window config |
| Builder | `t w D o O N A e W G P x` | Set the matching DSL filter (infix) |
| Builder | `RET` | Live-preview current spec in other window |
| Builder | `s` | Save (prompts name) → `org-gtd-saved-views` |
| Builder | `C-c C-k` | Abort; guard-prompt if dirty |

**Live preview (before/after one keystroke):** before `e effort<30m`, the preview lists all Home next-actions; after, the 2h "Repaint fence" row drops out on the next `RET`. The filter list you can bind is exactly `org-gtd-view-lang--known-filter-keys`, so the builder and the DSL never drift.

## 5. Fit with org-gtd

- **Extends:** the **view DSL** (`org-gtd-view-show`, `org-gtd-view-lang--known-filter-keys`, `--type-defaults`) as the preview+render engine; the **command-center** transient as the discovery home; **transient.el** for both prefixes, in the established `org-gtd-<verb>` mnemonic idiom.
- **Shared surface / cluster (E):** with **`REC-CHK-01`** (checklist/trigger-list manager). The two back different engines (view DSL vs checklist type) but MUST share: (1) the **list → create → preview → edit → save/name → recall → delete** lifecycle; (2) the CRUD keys `c e d D RET q s`; (3) the "Your saved X" list layout and the highlighted-item model; (4) `C-c C-k` abort with dirty-guard. As the Wave-0 exemplar, **this doc sets those; CHK-01 conforms.** The only per-feature variance is the *builder body* (DSL filters here; checklist items there) and the preview pane (rendered agenda vs. rendered checklist).
- **Reuse vs. new:** *Reused as-is* — `org-gtd-view-show` (rendering), the filter-key/type-default constants (builder infixes), area-of-focus & duration input functions, command-center. *Genuinely new* — the `org-gtd-saved-views` persisted store and the two transients; this is org-gtd's first interactive named-object CRUD manager.
- **Release tag:** view DSL and `org-gtd-view-show` are **[R]** (released 4.6.1); this feature builds a *management layer* on top of them (a persisted store + a transient + live preview via `org-gtd-view-show`) **without reworking the DSL**, so no fidelity justification is owed. The manager, store, and builder are net-new **[U]**. command-center is **[R]**. `org-gtd-reflect-missed-custom-views` (**[R]**, hand-written alist defcustom) is the *predecessor* pattern this supersedes — see §7.

### Type / extension-UX opportunities

- **A registry-shaped store is the reusable primitive.** `org-gtd-saved-views` (name→spec, `customize-save-variable`-backed, interactive CRUD) is the exact shape the primer flags the *type registry* lacks. Building it here proves the pattern the type system should adopt: an interactive editor over `org-gtd-user-types` instead of hand-written alists — i.e. the friendly front-end for the **[U]** `org-gtd-customize-type` and the **[X]** `org-gtd-define-type`. Recommend factoring the store+CRUD-transient scaffold generically so `define-type`'s UI can reuse it.
- **Builder-from-registry.** Generating the builder's infix rows from `org-gtd-view-lang--known-filter-keys` is the same "generate the transient from the registry, don't hand-sync" move the primer wants for the organize transient (its 3-places-in-sync friction). Doing it here first is a template for fixing that.

## 6. Edge cases & failure modes

- **Empty state:** no saved views → the manager shows a teaching line: *"No saved views yet. Press `c` to build one, or try `RET` on a built-in like Engage."* — not a blank pane.
- **Bad input:** an empty/duplicate name on save → *"A view named 'X' exists — overwrite? (y/n)."* A spec that renders nothing → preview shows the normal org-agenda "no matches" line, **not** an error (fail-soft). An unknown/hand-corrupted key in a stored spec → skipped with a `message` warning, rest of the view still renders.
- **When it goes wrong:** `org-gtd-view-show` errors are caught and surfaced in the preview pane as a one-line teaching message (*"Effort filter needs a duration like 30m"*), never a stack trace — matching the header-line-hint voice. Window config is always restored on quit/abort.

## 7. Open questions & maintainer decisions

- **Store granularity:** one flat `org-gtd-saved-views` defcustom, or fold built-in reflect/engage views into the same list so they're editable too? (Leaning: user views only for v5; built-ins stay code.)
- **Migration:** auto-import existing `org-gtd-reflect-missed-custom-views` entries into `org-gtd-saved-views` on first run, or leave both? Deprecating the hand-written defcustom is the clean end-state.
- **Preview placement:** other-window (assumed here) vs. a transient-child agenda buffer — needs a taste call to match how heavy the frame takeover should feel.

## 8. Provenance & links

- Net-new exemplar (no REC id). Deliverable-#3 status: **Not-implemented** (net-new). `gap-implementation-strategies.md` — flagship "manage my X interactively" build route (hint only). Cluster **E** sibling: `REC-CHK-01` (inherits this contract). Consumes-the-output relation: `REC-HOR-07` (a composite view this manager could produce). Predecessor surface: `org-gtd-reflect-missed-custom-views` `[R]`.
