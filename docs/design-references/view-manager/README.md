# Handoff: org-gtd View Manager (Direction C — Manager + builder)

## Overview

Let a user **create, live-preview, name, save, recall, edit, and delete a custom GTD
view without writing elisp.** Today a "view" is an elisp alist inside a `defun` or a
one-off `org-gtd-view-show` call — making one means editing `init.el` and re-evaluating.
This feature adds the missing **management layer**: a dedicated list transient
(`org-gtd-view-manager`) that is the browsable home for saved views, and a **builder
transient** (`org-gtd-view-manager--build`) with a **live org-agenda preview**.

Value lens: make the daily "what should I do right now?" (Engage) step easier for a real
GTD practitioner — build "show me next-actions in area *Home* under 30m," name it
"Weekend errands," and pull it up next Saturday.

**This is a management layer built ON TOP of the released view DSL. Do not rework
`org-gtd-view-show` or the DSL itself** — the DSL is the preview + render engine.

## About the design files

The files in this bundle are **design references created in HTML** — a prototype showing
the intended interaction model, layout, keys, and copy. They are **not** code to ship.
The target here is **Emacs / elisp** (`org-gtd`), so "recreate the design" means
implementing these surfaces with **`transient.el` + `org-agenda` + the existing view
DSL** in the org-gtd package, following org-gtd's established idioms — **not** porting any
HTML/CSS/JS. The HTML exists only so you can see what the transients and preview buffer
should feel like.

- `View Manager (Direction C).html` — self-contained visual reference (open in a browser).
  Shows: the manager list, the builder + live preview, the first-run empty state, the
  empty-agenda/sample-data handling, two keymap tables, and the resolved design notes.

## Fidelity

**Interaction-fidelity, not pixel-fidelity.** This is an Emacs feature — there is no pixel
canvas to match. Treat the mock as the source of truth for **structure, keymaps, grouping,
copy, and behavior**. The Team GTD paper/teal theme in the mock is a *illustrative skin*
(it's what an org-gtd color-theme could look like); the actual faces come from the user's
Emacs theme. Reproduce the **information architecture and the keys**, not the colors.

## Hard constraints (do not re-litigate)

- Build a **management layer** on top of the DSL. **Do not** modify `org-gtd-view-show` or
  the view language.
- **No `org-ql`, no `org-super-agenda`.** The DSL exists precisely because of the per-item
  `prefix` resolution those can't express. Emacs + `transient.el` + `org-agenda` + the
  existing DSL only.
- **Generate the builder's infix rows from `org-gtd-view-lang--known-filter-keys`** so the
  builder and the DSL can never drift out of sync.
- **No default keybindings** (org-gtd convention). Reached via `M-x` or a command-center
  entry.

---

## Surfaces (screens)

### 1. `org-gtd-view-manager` — the list transient

- **Purpose:** the browsable home for saved views. Highlight one; act on it.
- **Reached from:** `M-x org-gtd-view-manager`, or a new **`v` "Views…"** entry in the
  **Engage** column of `org-gtd-command-center`.
- **Layout:** a `transient-define-prefix`. A **"Your saved views"** group listing saved
  views (mock groups them by moment — *Engage · Today* and *Reflect · Review*). Each row:
  a moment glyph, the **name**, and a right-aligned **badge** = the one-line spec summary,
  e.g. `next-action · Home · <30m`, `delegated · who=Sam`, `project · not-done`. One row is
  the **highlight** (moved with `↑`/`↓`); all actions operate on it.
- **Keymap:**
  | Key | Action |
  |---|---|
  | `RET` | Render the highlighted view via `org-gtd-view-show` (in another window). |
  | `c` | Create — open the builder on a fresh spec `((name . "Untitled") (type . next-action))`. |
  | `e` | Edit — open the builder on the highlighted view's **stored spec**. |
  | `d` | Duplicate the highlighted view (`"Name copy"`), then edit. |
  | `D` | Delete the highlighted view — confirm `y/n`. Uppercase = forceful (org-gtd idiom). |
  | `↑` `↓` | Move the highlight. |
  | `q` | Quit; restore the saved window configuration. |
- **Empty state (first run):** never a blank pane — a teaching line: *"No saved views yet.
  Press `c` to build one, or `RET` a built-in like Engage."* Bundled built-ins (Engage,
  Weekly review) still render so `RET` is never dead.

### 2. `org-gtd-view-manager--build` — the builder transient (+ live preview)

- **Purpose:** build/edit a view spec interactively with immediate feedback.
- **Reached from:** `c` (create) or `e` (edit) in the manager. No difference between "new"
  and "edit" except the starting spec (mirrors `org-gtd-clarify-item`'s open-then-edit).
- **Layout:** a `transient-define-prefix` docked at the bottom of the frame; the **live
  preview** renders in another window above it (an org-agenda buffer titled after the
  view). A header-line advertises the keys. **The infix rows are generated from
  `org-gtd-view-lang--known-filter-keys`, grouped by DSL family:**
  - **Type** — `t` type (`next-action · delegated · calendar · project · someday · habit ·
    reference` + computed `stuck-* · active-project · completed-project …`).
  - **Time** — `w` when, `D` deadline, `C` scheduled (duration regex like `<30m`, `+7d`,
    and `past/today/future`).
  - **Structural** — `o` todo, `O` done, `N` not-done.
  - **Metadata** — `A` area-of-focus, `e` effort, `W` who, `G` tags, `P` priority.
  - **Prefix** — `x` prefix, the per-item resolution **chain** (fallback list, not a format
    string), e.g. `(project area-of-focus "—")`. **Preserve prefix control — it is the
    reason the DSL exists.**
  Each row shows the current value (or `—` when unset). A summary line at the top echoes the
  compact badge (`next-action · Home · <30m`).
- **Footer keys:** `RET` Preview now · `s` Save · `TAB` toggle engage/reflect · `C-c C-k`
  Abort.
- **Live preview behavior — AUTO-REFRESH (the chosen default):** re-run `org-gtd-view-show`
  on the current spec **on every infix change**, over the user's real `org-agenda-files`.
  It must feel smooth **and** be guarded:
  - **Debounce ~250ms.**
  - **Skip the render when the compiled spec is unchanged** (cache the last compiled spec).
  - Use transient's own **dynamic descriptions** for the instant in-menu count while the
    agenda pane catches up.
  - Keep an explicit **`RET` Preview** available for slow file sets.

### 3. Empty-agenda → sample preview

When `org-agenda-files` is empty (or the spec matches nothing), a live preview is
unhelpful. **Recommended (assessed feasible and cheap): render against a small built-in
SAMPLE dataset** — `let`-bind a tiny temp org file (a few representative headings) into
`org-agenda-files` **just for the preview render**, so the user still sees the *shape* of
the view. Show a one-line banner in the preview: *"sample data · your agenda-files are
empty — previewing org-gtd's built-in set."* **Fallback** (if sample data is deferred): a
self-teaching message — *"No matching items. org-agenda-files is empty…"* — not an error.

---

## The view spec (what the builder emits)

The builder compiles the infix state into an **org-gtd view alist** and hands it to
`org-gtd-view-show`. Example for "Weekend errands":

```elisp
((name . "Weekend errands")
 (type . next-action)
 (area-of-focus . "Home")
 (effort . (< "30m"))
 (prefix . (project area-of-focus "—"))
 (prefix-width . 12))
```

- The authoritative field list is `org-gtd-view-lang--known-filter-keys`; per-type smart
  defaults are `org-gtd-view-lang--type-defaults` — reuse both.
- `prefix` resolves **per item, structurally**: parent project's headline (cookies
  stripped) → area-of-focus (CATEGORY, falling back through project membership) → literal.

---

## Storage (resolved)

A view spec is an **elisp alist** (not human-readable prose like a checklist), so a **plain
file is the right home, not a defcustom form.**

- **Recommended: `org-gtd-directory/views.eld`** — Emacs read-syntax, visible, git- and
  mobile-syncable, keeps view data out of `custom.el`. Create lazily via
  `org-gtd--ensure-file-exists` with a guidance-comment header (org-gtd idiom for generated
  files). Read/write as a `name → spec` alist.
- **Fallback: a `org-gtd-saved-views` defcustom** (`customize-save-variable`) — choose this
  only if customize-UI editing is wanted; it slightly fights the "no elisp" goal.
- **Migration:** on first run, auto-import existing `org-gtd-reflect-missed-custom-views`
  entries into the store, then deprecate that hand-written defcustom. One-time, fail-soft:
  skip any entry with an unknown key and `message` it.

---

## Interactions & behavior

- **Lifecycle:** `list → create/edit → live-preview → save/name → recall → delete`.
- **Save (`s`):** prompt *"Name this view:"*; write spec to the store. Duplicate name →
  *"A view named 'X' exists — overwrite? (y/n)."*
- **Recall:** `RET` in the manager renders it; also provide `org-gtd-view-run` (autoloaded,
  `completing-read` over saved names) for keyboard recall without opening the manager
  (bind-it-yourself, no default key).
- **Edit = reopen builder on stored spec** (no separate edit UI).
- **Abort (`C-c C-k`):** discard; if the build is dirty, one guard prompt *"Discard unsaved
  view? (y/n)."*; restore windows.
- **Window discipline:** snapshot the window configuration on entry to both transients;
  **restore it on every exit path** (quit / abort / done) — mirrors `org-gtd-clarify`.

## Failure modes (fail-soft, teaching voice — never a stack trace)

- Bad effort duration → one-line hint: *"Effort needs a duration like 30m."*
- Spec that matches nothing → the normal org-agenda "no matches" line (not an error).
- Corrupted/unknown stored key → skip that key with a `message` warning; render the rest.
- `org-gtd-view-show` errors → caught and surfaced in the preview pane as a one-line
  teaching message, matching the header-line-hint voice.

## Reuse & extension-UX note

Factor the store + CRUD scaffold **generically** (a `name→spec` store over a store symbol +
a builder function + list-transient + dirty-guard), not copy-pasted. This is org-gtd's
first interactive named-object CRUD manager; the same scaffold is what the **type registry**
lacks — it's the friendly front-end a future `org-gtd-customize-type` / `define-type` UI
should reuse (Cluster-E idiom). Generating the builder from `known-filter-keys` is the same
"generate the transient from the registry, don't hand-sync" move the org-gtd primer wants
for the organize transient.

## Fit with org-gtd (anchors)

- **Extends:** the view DSL (`org-gtd-view-show`, `org-gtd-view-lang--known-filter-keys`,
  `--type-defaults`) as preview+render engine `[R released 4.6.1]`; `org-gtd-command-center`
  as the discovery home `[R]`; `transient.el` in the `org-gtd-<verb>` idiom; WIP/window-config
  discipline from `org-gtd-clarify`.
- **Genuinely new `[U]`:** the persisted `views.eld` store, `org-gtd-view-manager`,
  `org-gtd-view-manager--build`, `org-gtd-view-run`, the live-preview loop.

---

## Design tokens (for reading the HTML reference only — NOT for the Emacs UI)

The Emacs faces come from the user's theme. These are only the mock's palette/type, in case
you build any companion web docs.

- Colors: paper `#F6F3EC`, ink `#1B2422`, Flow teal `#0E8A6E`, Sun gold `#F2A41C`,
  Horizon coral `#E07A45`; hairline `#E5DFD1`.
- Type: Spectral (display serif), Hanken Grotesk (UI sans), IBM Plex Mono (keys, tokens,
  timestamps, agenda rows).
- State keywords are UPPERCASE mono: `NEXT` `TODO` `WAIT` `DONE` `CNCL`.

## Files in this bundle

- `README.md` — this spec (self-sufficient; implement from this alone).
- `View Manager (Direction C).html` — self-contained visual reference.
- `screenshots/` — stills of the reference:
  - `1-masthead.png` — intent + the locked direction.
  - `2-manager-and-builder.png` — the list transient and the builder + live preview.
  - `3-preview-and-notes.png` — the compiled spec + auto-refresh guidance.
  - `4-keymaps.png` — the two surfaces, key by key.
  - `5-design-notes.png` — resolved open questions (storage, migration, fail-soft).

## Source files in the design project (for reference)

- `ui_kits/app/View Manager (Direction C).html` + `vmc-page.jsx` — the C-only reference page.
- `ui_kits/app/vm-components.jsx` — the shared Emacs-frame + transient components.
- `ui_kits/app/vm-data.jsx` — saved-views sample, builder rows (from the DSL families),
  compiled spec, preview + sample rows.
- `ui_kits/app/View Manager.html` — the fuller exploration doc (three friction directions
  A/B/C, tweakable) that this handoff distilled Direction C from. Not needed to implement;
  useful if you want to see the alternatives that were considered.
