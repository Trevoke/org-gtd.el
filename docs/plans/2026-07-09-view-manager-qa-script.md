# View Manager — Manual QA Script

A walk-through to verify the View Manager's UI / UX / core behavior by hand.
The acceptance suite proves the logic; this catches the things it can't — a
mislabeled key, an awkward prompt, a preview that doesn't refresh, a window
that doesn't come back.

**Feature under test:** `org-gtd-view-manager`, `org-gtd-view-run`, the builder
transient, live preview, and the `views.eld` store — plus the two recent fixes:
the **`not-done` flag now toggles off**, and **`not-habit` is a real, exposed
filter**.

Tick each `[ ]` as you go. Expected results are in _italics_.

---

## 0. Prep — a clean, correct sandbox

> **Why a sandbox:** QA against an incomplete org-gtd config produces false
> symptoms that look like bugs (unrecognized keywords → empty agendas, etc.).
> Start from a complete, throwaway setup. This block touches nothing in your
> real `~/gtd`.

**0.1** From the worktree, launch an Emacs that loads *this branch's* build:

```bash
cd /home/stag/src/projects/org-gtd.el/.claude/worktrees/view-manager-design
~/bin/eldev emacs
```

**0.2** In that Emacs, evaluate this setup block (`M-:`, or paste into
`*scratch*` and `C-x C-e` after it). It writes a small task set that includes a
**habit**, actions in two areas with different efforts, a delegated item, and a
completed item:

```elisp
(let ((dir (expand-file-name "org-gtd-view-qa/" temporary-file-directory)))
  (make-directory dir t)
  (setq org-gtd-directory dir)
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))
  (setopt org-gtd-keyword-mapping
          '((todo . "TODO") (next . "NEXT") (wait . "WAIT")
            (done . "DONE") (canceled . "CNCL")))
  (setq org-gtd-areas-of-focus '("Home" "Work"))
  (require 'org-gtd)
  (with-temp-file (expand-file-name "org-gtd-tasks.org" dir)
    (insert "\
* NEXT Buy stamps :errand:
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Home
:Effort:   15
:END:
* NEXT Repaint the fence
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Home
:Effort:   2:00
:END:
* NEXT Draft quarterly plan
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Work
:Effort:   45
:END:
* WAIT Response from Sam
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Sam
:END:
* NEXT Water the plants
SCHEDULED: <2026-07-10 Fri .+2d>
:PROPERTIES:
:ORG_GTD: Habit
:STYLE: habit
:CATEGORY: Home
:END:
* DONE Old finished thing
CLOSED: [2026-07-08 Wed 09:00]
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Work
:END:
"))
  (setq org-agenda-files (list (expand-file-name "org-gtd-tasks.org" dir)))
  (org-gtd-mode 1)
  (message "View Manager QA sandbox ready in %s" dir))
```

- [ ] Block evaluates without error; echo area shows _"View Manager QA sandbox ready in …"_.
- [ ] `M-x org-gtd-view-run` right now → _errors politely_: **"No saved views yet — build one with M-x org-gtd-view-manager"** (no stack trace).

---

## 1. Discovery & entry points

**1.1** `M-x org-gtd-command-center`.
- [ ] A `v` entry labelled **"Views…"** is present.
- [ ] Press `v` → the View Manager opens. (`q` there to come back if needed.)

**1.2** `M-x org-gtd-view-manager` directly.
- [ ] Opens the same manager transient.

---

## 2. Empty state (no saved views yet)

With an empty store, the manager list should teach, not blank out.

- [ ] The list area reads: **"No saved views yet.  Press c to build one, or RET to open Engage."**
- [ ] Press `RET` → _the daily **Engage** view opens_ (not an error, not a blank buffer).
- [ ] Reopen the manager (`M-x org-gtd-view-manager`).

---

## 3. Build a view + live preview

**3.1 Open the builder.** Press `c`.
- [ ] A builder transient docks at the bottom; a preview/agenda window opens above it.
- [ ] The header reads **`View: Untitled  —  next-action`** (default spec).
- [ ] Infix columns are visible: **Type / Time / Structural / Metadata / Prefix**.

**3.2 Preview real data.** Press `RET` (Preview).
- [ ] The window above renders next actions from your sandbox — _**Buy stamps**, **Repaint the fence**, **Draft quarterly plan**_ (the three `NEXT` Actions). The habit and the `WAIT`/`DONE` items are **not** here (this is a `next-action` view).

**3.3 Narrow by area — watch the preview react.** Press `A`, choose `Work`.
- [ ] Completing-read offers **Home** and **Work**.
- [ ] The header badge updates to include **`Work`**.
- [ ] The preview refreshes on its own within a moment (debounced). If impatient, press `RET` to force it.
- [ ] _Only **Draft quarterly plan** (the Work action) remains; Buy stamps and Repaint (Home) drop out._
- [ ] Press `A` again, choose `Home` → the preview shows **Buy stamps** and **Repaint the fence**.

**3.4 Effort filter — and a known input quirk.** Press `e`.
- [ ] Enter `<0:30` (that's `H:MM` — "less than 30 minutes"). Badge shows **`<0:30`**; preview narrows to **Buy stamps** only (15m) — Repaint (2:00) and Draft (45m) drop out.
- [ ] Now press `e` again and enter `<30m`. **Nothing narrows** — everything stays. _This is expected but confusing:_ in org's duration syntax the `m` suffix means **months**, not minutes, so `<30m` is "< 30 months". Use `H:MM` (`0:30`) or `<1h` for real effort thresholds. Worth noting that the infix's own hint suggests `<30m` — flag it if that trips you up.
- [ ] Empty result is fine: press `e`, `<0:01` → preview empties (no sub-minute tasks); org-agenda shows its normal "no matches", **not** an error. Clear it back (`e`, `<1h`).

**3.5 Empty-agenda sample data.** Preview only re-renders when the spec
*changes*, so open a **fresh** builder to force a first render: abort this one
(`C-c C-k`, answer `y` to the "Discard unsaved view?" prompt), then `M-:`
`(setq org-agenda-files nil)` `RET`, press `c` for a new builder, and press `RET`.
- [ ] The preview shows **sample data** with a banner: **"sample data · your agenda-files are empty — previewing org-gtd's built-in set"**. Since a fresh builder is a `next-action` view, you'll see the two sample next-actions — **Buy stamps** and **Draft quarterly plan** (the sample's `WAIT` item isn't a next action).
- [ ] Restore real data (and drop this scratch builder with `C-c C-k`): `M-:` `(setq org-agenda-files (list (expand-file-name "org-gtd-view-qa/org-gtd-tasks.org" temporary-file-directory)))` `RET`.

---

## 4. The two recent fixes (focus here)

### 4.1 `not-done` is a proper on/off toggle

Open a builder (`M-x org-gtd-view-manager` → `c`), then in the `Structural`
column (key `N`):

- [ ] Press `N` once → header badge **gains `not-done`**.
- [ ] Press `N` **again** → badge **loses `not-done`** (this is the fix — previously it could be set but never cleared).
- [ ] Press `N` a third time → it comes back. Leave it **off** before continuing.

### 4.2 `not-habit` is exposed and works

**UI (Structural column, key `H`):**
- [ ] Press `H` → badge gains **`not-habit`**; press `H` again → it clears. (Same toggle behaviour as `N`.) Leave it **off**.

**End-to-end exclusion.** A view has to carry a `type`, and the only type that
matches habit items is `habit` itself — so a `habit` view is where `not-habit`'s
effect is visible. Evaluate:

```elisp
(org-gtd-view-show '((name . "Habits") (type . habit)))
```
- [ ] The habit **"Water the plants"** renders.

```elisp
(org-gtd-view-show '((name . "Habits, excluded") (type . habit) (not-habit . t)))
```
- [ ] The view is now **empty** — the habit was excluded. (Before the fix, `not-habit` was silently ignored, so it would still have shown.) In a hand-written mixed/native view that swept in habits, this same flag trims them out.

Return to the manager/builder for the rest (`M-x org-gtd-view-manager` if needed).

---

## 5. Save, name, and the guards

Build a keeper: press `c` for a fresh view, set type `t` → `next-action`, effort
`e` → `<30m`, area `A` → `Home`.

**5.1 Save.** Press `s`.
- [ ] Prompt: **"Name this view: "** defaulting to the current name.
- [ ] Enter `Weekend errands` → echo area: **"Saved view 'Weekend errands'"**; the builder closes and _your original window layout is restored_.
- [ ] The manager list now shows a row: **`Weekend errands`** with a badge like **`next-action · Home · <30m`**.

**5.2 Blank-name guard.** `c` → `s` → clear the name (delete to empty) → `RET`.
- [ ] It **refuses**: error **"A view needs a name"**, nothing is saved, and the builder reopens so you can try again. Abort it with `C-c C-k`.

**5.3 Overwrite guard.** `c`, build anything, `s`, name it `Weekend errands` (a name that already exists).
- [ ] Prompt: **"A view named 'Weekend errands' exists — overwrite? (y/n)"**.
- [ ] Answer `n` → **"Save cancelled"**, builder reopens, nothing overwritten. Abort with `C-c C-k`.

**5.4 Abort dirty guard.** `c`, change one infix, `C-c C-k`.
- [ ] Prompt **"Discard unsaved view? (y/n)"**; `y` discards and restores windows.

---

## 6. Manage the list

Make sure you have ≥2 saved views (repeat §5.1 to add e.g. `Waiting on Sam`:
type `delegated`, who `W` → `Sam`).

**6.1 Highlight & render.**
- [ ] `↑` / `↓` move the **▸** marker between rows.
- [ ] `RET` on a highlighted row renders **that** view as a real agenda (real data, no sample banner).

**6.2 Edit.** Highlight `Weekend errands`, press `e`.
- [ ] The builder opens **seeded with that view's spec** (header shows its name + badge). Change effort to `<1h`, `s`, keep the same name.
- [ ] No overwrite prompt (saving a view back onto itself isn't an overwrite). Badge updates in the list.

**6.3 Rename = move (no orphan).** Highlight `Weekend errands`, `e`, `s`, rename to `Errands`.
- [ ] After saving, the list shows **`Errands`** and **no** leftover `Weekend errands` row (the old name was removed, not duplicated).

**6.4 Duplicate.** Highlight `Errands`, press `d`.
- [ ] The builder opens on a copy named **`Errands copy`**. Save it (`s`, `RET`).
- [ ] List now has both `Errands` and `Errands copy`.
- [ ] **Duplicate-then-abort leaves no orphan:** `d` on `Errands` again → builder on `Errands copy`… → `C-c C-k` (discard). Back in the list there is **no** stray second `Errands copy`.

**6.5 Delete.** Highlight `Errands copy`, press `D`.
- [ ] Prompt **"Delete view 'Errands copy'? (y/n)"**; `y` removes it; the ▸ highlight stays sensible (clamped, no error).

**6.6 Quit.** Press `q`.
- [ ] The manager closes and _the window layout from before you opened it is restored_.

---

## 7. Recall by name

**7.1** `M-x org-gtd-view-run`.
- [ ] Completing-read lists your saved view names.
- [ ] Pick one → it renders as an agenda immediately.

---

## 8. Persistence across a restart

**8.1** Confirm the store file exists:
```elisp
(find-file (expand-file-name "org-gtd-view-qa/views.eld" temporary-file-directory))
```
- [ ] It's a readable elisp alist of your saved views (name → spec). Close it without editing.

**8.2** Quit Emacs, relaunch (`~/bin/eldev emacs`), and **re-eval the §0.2 setup block** (it points at the same temp dir; it will rewrite the tasks file but leaves `views.eld` alone).
- [ ] `M-x org-gtd-view-manager` → **your saved views are still there**.
- [ ] `M-x org-gtd-view-run` → still lists them.

---

## 9. Migration from the legacy variable

Legacy `org-gtd-reflect-missed-custom-views` entries should import automatically
on first open. This runs once per session, so test it in a **fresh** Emacs.

**9.1** Relaunch Emacs, re-eval §0.2, then **before opening the manager** set a
legacy view:

```elisp
(setq org-gtd-reflect-missed-custom-views
      '(((name . "Legacy big rocks") (type . project) (not-done . t))))
```

**9.2** `M-x org-gtd-view-manager`.
- [ ] A row **`Legacy big rocks`** (badge `project · not-done`) appears alongside any you saved earlier — imported without you recreating it.
- [ ] It renders (`RET`) and edits (`e`) like any other view.

---

## 10. Known v1 limitations (do NOT file these as bugs)

- **Deleted migrated view returns next session.** If you delete a view that came
  from `org-gtd-reflect-missed-custom-views` (§9) and restart, it re-imports.
  The migration guard is session-scoped; clearing the legacy variable stops it
  for good. _Expected, documented._
- **Every view needs a type.** The builder can't clear the type (only swap it),
  and a typeless spec passed to `org-gtd-view-show` errors ("Unsupported view
  spec"). So `not-habit` is only combinable with a type — practically, the
  `habit` type (§4.2). _Expected / pre-existing DSL behavior._
- **Effort `m` = months.** The effort infix hint says `<30m` but org reads `m`
  as months; use `H:MM` (`<0:30`) for minutes (§3.4). _Pre-existing; noted for
  awareness._
- **Flat list only.** No "by moment" grouping / `TAB` engage-vs-reflect toggle —
  deferred. _Expected._

---

### If something misbehaves

Capture: the exact keys pressed, the header/badge text, the prompt wording, and
whether real vs. sample data was showing. A wrong **key label**, a **missing
prompt**, a **preview that never refreshes**, or a **window that doesn't return**
on quit/abort are the high-value UI bugs this script is meant to surface.
