# View Manager — Design

**Feature:** `NEW-VIEW-MANAGER` (corpus id) — an interactive layer to **create, live-preview,
name, save, recall, edit, and delete a custom GTD view without writing elisp.**

**Goal:** Turn the released view DSL from an `init.el`-only, eval-and-throw-away literal into a
set of **named views a user manages interactively** — so the daily "what should I do right now?"
(Engage) step is buildable by a non-programmer: build "next-actions in area *Home* under 30m,"
name it "Weekend errands," pull it up next Saturday.

**Status:** Design. Not yet planned or implemented. Target: v5.

**Provenance:** distilled from the external design handoff vendored at
`docs/design-references/view-manager/` (README + HTML mock + screenshots, "Direction C —
Manager + builder"), reconciled here against the **authoritative view DSL** in
`org-gtd-view-language.el`. Corpus source: `docs/feature-analysis/ux-workflows/NEW-VIEW-MANAGER.md`.
Where the handoff or the corpus doc disagrees with the code, **the code (this document) wins** —
see §2 and §10.

---

## Summary

A **management layer built strictly on top of the released view DSL.** The DSL
(`org-gtd-view-show`, `org-gtd-view-lang--known-filter-keys`, `org-gtd-view-lang--type-defaults`)
is the preview + render engine and is **not reworked.** The new surfaces are:

1. **`org-gtd-view-manager`** — a list transient: the browsable home for saved views. Highlight
   one; render / create / edit / duplicate / delete it.
2. **`org-gtd-view-manager--build`** — a builder transient whose infix rows are derived from the
   DSL's filter keys, with a **live org-agenda preview** in another window.
3. **`views.eld`** — a plain-file `name → spec` store in `org-gtd-directory`.
4. **`org-gtd-view-run`** — autoloaded `completing-read` recall over saved names (no default key).

Discovery: a new **`v` "Views…"** entry in the Engage column of `org-gtd-command-center`.

## Key decisions (divergences from the handoff and the corpus doc)

| Decision | Handoff / corpus said | This design says | Why |
|---|---|---|---|
| **Storage** | Corpus: `org-gtd-saved-views` **defcustom** (`customize-save-variable`). Handoff: `views.eld` plain file (recommended), defcustom as fallback. | **`org-gtd-directory/views.eld` plain file**, `name → spec` alist. | A view spec is an elisp *alist*, not custom-editable prose; a visible file is git-/mobile-syncable and keeps view data out of `custom.el`. Matches the CHK-01 precedent (`checklists.org`, `review-state.eld`) of **plain file over defcustom**. Supersedes the corpus's defcustom. |
| **CRUD manager itself** | CHK-01 (Cluster-E sibling) **rejected** the list/builder/preview transient pair. | **Retained** for the view manager. | The CHK-01 rejection was *because org-file storage replaced the manager* — you edit `checklists.org` by hand. An `.eld` alist is **not** hand-editable, so the builder transient earns its keep here. This is the case Cluster-E was actually for. |
| **Builder infix source** | "Generate infix rows from `org-gtd-view-lang--known-filter-keys`." | Generate from a **curated user-facing filter subset**, validated against `known-filter-keys` at load. | `known-filter-keys` mixes user filters with reserved *structural* keys (`view-type`, `block-type`, `group-by`, `native`, …) that must not become infixes. See §2. |
| **Saved-view grouping** | Handoff groups the list "by moment" (Engage·Today / Reflect·Review); builder `TAB` toggles engage/reflect. | **v1: one flat "Your saved views" list.** Moment-grouping deferred. | "Moment" is manager metadata with no DSL key. A flat list ships the value; grouping is a later polish, not a blocker. YAGNI. |
| **Migration source shape** | "Auto-import `org-gtd-reflect-missed-custom-views`; skip unknown keys." | Same, **plus flatten the nested `(filters . (...))` sub-alist** into top-level keys on import. | Those entries nest filters under a `filters` key; the builder's infixes read *top-level* keys. Render works either way, but editability needs flat. See §7. |
| **Live preview default** | Auto-refresh on every infix change. | **Auto-refresh, debounced ~250ms + skip-if-unchanged**, with an explicit `RET` fallback. | Keeps it smooth over real `org-agenda-files`; the explicit key covers slow file sets. |

---

## 1. Fit with org-gtd (what is reused vs. new)

- **Reused as-is, unchanged `[R released 4.6.1]`:**
  - `org-gtd-view-show` — the render + preview engine. Signature:
    `(org-gtd-view-show VIEW-SPEC-OR-SPECS &optional KEYS)` (`org-gtd-view-language.el:1081`).
  - `org-gtd-view-lang--known-filter-keys`, `org-gtd-view-lang--type-defaults`,
    `org-gtd-view-lang--default-prefix`, `--simple-types`, `--complex-types`
    (`org-gtd-view-language.el:178`–`226`) — the source of truth for filter keys, type values,
    and prefix semantics.
  - `org-gtd-command-center` (`org-gtd-command-center.el:44`) — the discovery home.
  - `org-gtd-directory` / `org-gtd--path` (`org-gtd-files.el`) for locating the store. **Note:
    do _not_ reuse `org-gtd--ensure-file-exists` for `views.eld`** — it calls
    `org-gtd-core-prepare-buffer` → `org-mode-restart` (`org-gtd-core.el:557`), which forces the
    buffer into org-mode. That helper is for `.org` files only; a `.eld` file is written
    directly (see §7).
  - `org-gtd-areas-of-focus`, the DSL's duration-regex input, and the window-config discipline
    from `org-gtd-clarify`.
- **Genuinely new `[U]`:** the `views.eld` store, the two transients
  (`org-gtd-view-manager`, `org-gtd-view-manager--build`), `org-gtd-view-run`, and the
  live-preview loop.
- **Predecessor superseded:** `org-gtd-reflect-missed-custom-views` `[R]` — the hand-written
  alist defcustom this feature replaces (migrated in, then deprecated). See §7.

**Hard constraints (do not re-litigate):** build on top of the DSL — do **not** modify
`org-gtd-view-show` or the view language. **No `org-ql`, no `org-super-agenda`** (the DSL exists
precisely for the per-item `prefix` resolution they cannot express). **No default keybindings**
(org-gtd convention) — reached via `M-x` or the command-center entry.

## 2. Reconciliation with the authoritative DSL

The handoff and corpus doc predate a close reading of the code. Corrections, all sourced from
`org-gtd-view-language.el`:

**2.1 The builder must not surface the whole `known-filter-keys` list.**
`org-gtd-view-lang--known-filter-keys` (line 178) is:

```
name type when deadline scheduled todo done not-done area-of-focus who tags priority
effort clocked last-clocked-out blocks prefix prefix-width view-type agenda-span
show-habits additional-blocks filters not-habit property block-type group-contexts
group-by todo-keyword prefix-format native
```

Only a subset are **user-facing filters**; the rest are **reserved structural keys** (`blocks`,
`view-type`, `agenda-span`, `additional-blocks`, `filters`, `block-type`, `group-contexts`,
`group-by`, `prefix-format`, `native`, `show-habits`). The builder exposes only the
filter subset, in the handoff's five groups. (`not-habit` — an item-level boolean, parallel to
`not-done` — is exposed as a Structural flag infix; it was implemented in the DSL at the same time.)

| Group | Infix keys → DSL key | Notes |
|---|---|---|
| **Type** | `t` → `type` | one of the authoritative type list (2.2) |
| **Time** | `w` → `when`, `D` → `deadline`, `C` → `scheduled` | values `past` / `today` / `future`, or a duration/offset regex (`<30m`, `+7d`) |
| **Structural** | `o` → `todo`, `O` → `done`, `N` → `not-done` | |
| **Metadata** | `A` → `area-of-focus`, `e` → `effort`, `W` → `who`, `G` → `tags`, `P` → `priority` | `effort` value is a comparison list, e.g. `(< "30m")` |
| **Prefix** | `x` → `prefix`, (width) → `prefix-width` | the per-item resolution **chain** — see 2.3 |

**Anti-drift mechanism (revised).** Define a builder-side metadata table
(`org-gtd-view-manager--filter-specs`: key → group, infix letter, reader, formatter) and, at
load, **assert every key it names is present in `org-gtd-view-lang--known-filter-keys`** — so a
DSL rename surfaces as a load-time error, not silent drift. This honors "don't modify the DSL"
while keeping the guarantee the handoff wanted. (Blindly iterating the raw constant would emit
infixes for `native`, `group-by`, etc.)

**2.2 Authoritative `type` values** come from `--simple-types` and `--complex-types` (lines
215–226), not the handoff's abbreviated list:

- Simple: `next-action delegated calendar tickler project someday habit reference trash
  quick-action`.
- Complex/computed: `stuck-project active-project completed-project tickler-project
  incubated-project stuck-delegated stuck-calendar stuck-tickler stuck-habit stuck-next-action`.

The handoff omitted `tickler`, `trash`, `quick-action`, `tickler-project`, `incubated-project`.
The type infix reads its candidates from these constants (with `--type-defaults`, line 188,
supplying per-type name/`when` smart defaults) — never a hand-copied list.

**2.3 `prefix` is a fallback *chain*, not a format string.** e.g.
`(prefix . (project area-of-focus "—"))`: per item, try the parent project's headline (cookies
stripped) → area-of-focus (CATEGORY, falling back through project membership) → the literal.
`--default-prefix` is `(project area-of-focus file-name)`. Preserve prefix control in the
builder — **it is the reason the DSL exists.** The prefix infix edits a list, not a string.

**2.4 `v` is free in the command-center Engage column** (`org-gtd-command-center.el:46`, which
uses `e` / `@` / `n`). The new entry is `("v" "Views…" org-gtd-view-manager)`.

## 3. Surface 1 — `org-gtd-view-manager` (list transient)

- **Purpose:** the browsable home for saved views. Highlight one; act on it.
- **Reached from:** `M-x org-gtd-view-manager`, or the `v` "Views…" command-center entry.
- **Layout:** a `transient-define-prefix`. A **"Your saved views"** group (flat for v1 — see §Key
  decisions). Each row: the **name** and a right-aligned **badge** = the one-line spec summary,
  e.g. `next-action · Home · <30m`, `delegated · who=Sam`, `project · not-done`. One row is the
  **highlight**, moved with `↑`/`↓`; all actions operate on it.

| Key | Action |
|---|---|
| `RET` | Render the highlighted view via `org-gtd-view-show` (other window). |
| `c` | Create — open the builder on a fresh spec `((name . "Untitled") (type . next-action))`. |
| `e` | Edit — open the builder on the highlighted view's **stored spec**. |
| `d` | Duplicate the highlighted view (`"Name copy"`), then edit. |
| `D` | Delete the highlighted view — confirm `y/n`. Uppercase = forceful (org-gtd idiom). |
| `↑` `↓` | Move the highlight. |
| `q` | Quit; restore the saved window configuration. |

- **Empty state (first run):** never a blank pane — a teaching line: *"No saved views yet. Press
  `c` to build one, or `RET` to open Engage."* When the store is empty, **`RET` invokes the
  existing `org-gtd-engage` command** so the key is never dead. It does _not_ render a stored
  "built-in" spec — built-ins remain commands (§12 defers folding them into the store), so there
  is no pseudo-view to delete or edit. This special-cases only the empty state; once the user has
  saved views, `RET` renders the highlighted one via `org-gtd-view-show` as usual.

## 4. Surface 2 — `org-gtd-view-manager--build` (builder + live preview)

- **Purpose:** build/edit a view spec interactively with immediate feedback. No difference
  between "new" and "edit" except the starting spec (mirrors `org-gtd-clarify-item`'s
  open-then-edit).
- **Layout:** a `transient-define-prefix` docked at the bottom of the frame; the **live preview**
  renders in another window above it (an org-agenda buffer titled after the view). Infix rows are
  the five groups from §2.1. Each row shows the current value (or `—` when unset). A summary line
  at the top echoes the compact badge (`next-action · Home · <30m`).
- **Footer keys:** `RET` Preview now · `s` Save · `C-c C-k` Abort. (`TAB` engage/reflect toggle
  is deferred with moment-grouping.)
- **Live preview — auto-refresh (guarded):** re-run `org-gtd-view-show` on the current spec on
  every infix change, over the user's real `org-agenda-files`:
  - **Debounce ~250ms** (idle timer).
  - **Skip the render when the compiled spec is unchanged** (cache the last compiled spec).
  - Use transient's **dynamic descriptions** for an instant in-menu count while the agenda pane
    catches up.
  - Keep an explicit **`RET` Preview** for slow file sets.

## 5. Surface 3 — empty-agenda → sample preview

When `org-agenda-files` is empty (or the spec matches nothing), a live preview is unhelpful.
**Render against a small built-in SAMPLE dataset**: `let`-bind a tiny temp org file (a few
representative headings across types/areas) into `org-agenda-files` **for the preview render
only**, so the user sees the *shape* of the view. Banner in the preview: *"sample data · your
agenda-files are empty — previewing org-gtd's built-in set."* **Fallback** (if sample data is
deferred to a later cut): a self-teaching message — *"No matching items. org-agenda-files is
empty…"* — not an error.

## 6. The view spec (what the builder emits)

The builder compiles infix state into a **flat org-gtd view alist** and hands it to
`org-gtd-view-show`. Example ("Weekend errands"):

```elisp
((name . "Weekend errands")
 (type . next-action)
 (area-of-focus . "Home")
 (effort . (< "30m"))
 (prefix . (project area-of-focus "—"))
 (prefix-width . 12))
```

- Authoritative field list: the filter subset of `org-gtd-view-lang--known-filter-keys` (§2.1).
- Per-type smart defaults: `org-gtd-view-lang--type-defaults`.
- Keys left unset are simply absent (not `nil`), so the DSL applies its own defaults.

## 7. Storage & migration

**Store:** `org-gtd-directory/views.eld` — Emacs read-syntax, a `name → spec` alist. Written
**directly** (a `;;`-comment guidance header + the printed alist via `f-write-text` / a plain
buffer save), created lazily on first access — **not** via `org-gtd--ensure-file-exists`, which
org-mode-restarts the buffer and is `.org`-only (see §1). Read on manager open; written on
save/delete. Consistent with CHK-01's `checklists.org` / `review-state.eld` plain-file precedent.

**Migration (one-time, fail-soft):** on first run, auto-import existing
`org-gtd-reflect-missed-custom-views` entries into `views.eld`, then deprecate that defcustom.
Those entries have the shape (`org-gtd-reflect.el:296`):

```elisp
((name . "My Custom View")
 (filters . ((type . delegated) (area-of-focus . "Work"))))
```

The `filters` sub-alist is **nested**; the builder's infixes read *top-level* keys. So the
importer **flattens** `filters` into top-level keys (`org-gtd-view-show` renders either shape, but
the manager must be able to *edit* the import). Skip any entry with an unknown key and `message`
it; never abort the import on one bad entry.

## 8. Interactions & failure modes

- **Lifecycle:** `list → create/edit → live-preview → save/name → recall → delete`.
- **Save (`s`):** prompt *"Name this view:"*; write spec to the store. Duplicate name →
  *"A view named 'X' exists — overwrite? (y/n)."*
- **Recall:** `RET` in the manager renders it; `org-gtd-view-run` (autoloaded, `completing-read`
  over saved names) gives keyboard recall without opening the manager (bind-it-yourself).
- **Abort (`C-c C-k`):** discard; if the build is dirty, one guard prompt *"Discard unsaved view?
  (y/n)."*; restore windows.
- **Window discipline:** snapshot the window configuration on entry to both transients; **restore
  it on every exit path** (quit / abort / done) — mirrors `org-gtd-clarify`.
- **Fail-soft, teaching voice (never a stack trace):**
  - Bad effort duration → *"Effort needs a duration like 30m."*
  - Spec that matches nothing → the normal org-agenda "no matches" line.
  - Corrupted/unknown stored key → skip that key with a `message`; render the rest.
  - `org-gtd-view-show` errors → caught, surfaced in the preview pane as a one-line teaching
    message matching the header-line-hint voice.

## 9. Reuse & the Cluster-E scaffold

Factor the store + CRUD scaffold **generically** — a `name → spec` store over a store symbol, a
builder function, a list-transient, and a dirty-guard — **not** copy-pasted. This is org-gtd's
first interactive named-object CRUD manager; the same scaffold is the friendly front-end a future
`org-gtd-customize-type` / `define-type` UI should reuse (the Cluster-E idiom). Generating the
builder from the filter-key metadata table (§2.1) is the same "generate the transient from the
registry, don't hand-sync" move the primer wants for the organize transient.

Note the Cluster-E scope here differs from CHK-01: CHK-01 *dropped* the manager because
org-file storage replaced it; the view manager *keeps* it because an `.eld` alist is not
hand-editable. So the reusable scaffold is genuinely first-proven here.

## 10. Corpus impact — supersession of `NEW-VIEW-MANAGER.md`

When implementation lands, update `docs/feature-analysis/` from this section (mirrors the CHK-01
/ REF-02 §8 pattern):

- **Implemented (as specified):** the two transients; `org-gtd-view-run`; live preview via
  `org-gtd-view-show`; empty/first-run teaching states; the CRUD lifecycle and keys
  (`c e d D RET q s`, `C-c C-k`); command-center `v` entry.
- **Implemented (differently — read this doc, not the corpus doc):** storage is
  **`views.eld` plain file**, not the `org-gtd-saved-views` defcustom the corpus specifies;
  builder infixes come from a **curated filter subset** validated against `known-filter-keys`,
  not the raw constant; migration **flattens** the nested `filters` shape.
- **Deferred (valid later pulls):** moment-grouping of the saved-view list + builder `TAB`
  toggle; sample-data preview if cut from v1 (fallback message ships instead); folding built-in
  reflect/engage views into the same editable store (built-ins stay code for v1).
- **Contract for Cluster-E siblings** (a future `define-type` / `customize-type` UI): the scaffold
  they inherit is *a `name → spec` `.eld` store + list-transient + builder + dirty-guard*, per §9
  — not the corpus's defcustom-backed variant.

## 11. Testing (outline)

- **Store round-trip:** write → read `views.eld` preserves specs; unknown key skipped with a
  message; missing file created lazily by the store's own writer (not `org-gtd--ensure-file-exists`).
- **Migration:** a nested-`filters` `org-gtd-reflect-missed-custom-views` entry imports to a flat,
  editable spec; a bad entry is skipped, not fatal.
- **Spec compile:** infix state → the expected flat alist; unset keys absent; effort/prefix shapes
  correct.
- **Anti-drift assertion:** the load-time check fires if a filter-spec key is absent from
  `org-gtd-view-lang--known-filter-keys`.
- **Badge formatter:** representative specs → their one-line summaries.
- **Fail-soft:** empty `org-agenda-files` yields the sample/fallback path, not an error.
- Transient/preview interaction is covered by thin integration tests; the compile/store/migration
  logic is pure and unit-tested (the org-gtd testing convention).

## 12. Deferred (explicitly not in this cut)

- Moment-grouping + `TAB` engage/reflect toggle (flat list ships).
- Editing built-in views (built-ins stay code).
- Sharing/exporting views beyond the git-syncable `views.eld` file.
- The generalized `define-type` UI that reuses this scaffold (this design only *enables* it).

## 13. Provenance

- Design handoff (vendored): `docs/design-references/view-manager/` — `README.md`,
  `View Manager (Direction C).html`, `screenshots/`.
- Corpus: `docs/feature-analysis/ux-workflows/NEW-VIEW-MANAGER.md` (superseded by §10 above).
- Authoritative DSL: `org-gtd-view-language.el` (`--known-filter-keys` :178, `--type-defaults`
  :188, `--default-prefix` :209, `--simple/--complex-types` :215/:220, `org-gtd-view-show` :1081).
- Cluster-E / plain-file precedent: `docs/plans/2026-07-06-checklists-and-guided-review-design.md`
  §8 (CHK-01 rejected the CRUD manager for checklists *because* org-file storage replaced it;
  this design keeps it *because* an `.eld` alist is not hand-editable).
- Predecessor: `org-gtd-reflect-missed-custom-views` (`org-gtd-reflect.el:296`).
- Discovery home: `org-gtd-command-center` (`org-gtd-command-center.el:44`).
