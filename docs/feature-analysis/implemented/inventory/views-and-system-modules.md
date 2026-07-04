# Implemented Feature Inventory: Views & System Modules

Bottom-up inventory derived directly from source code (not from specs or plans).
Scope: engagement views, review/reflect, view language, horizons/areas of focus,
mode & system infrastructure, upgrades/compat.

Modules covered: `org-gtd-agenda.el`, `org-gtd-agenda-property.el`,
`org-gtd-agenda-transient.el`, `org-gtd-engage.el`, `org-gtd-reflect.el`,
`org-gtd-someday-review.el`, `org-gtd-skip.el`, `org-gtd-view-language.el`,
`org-gtd-context.el`, `org-gtd-areas-of-focus.el`, `org-gtd-horizons.el`,
`org-gtd-command-center.el`, `org-gtd-mode.el`, `org-gtd-hooks.el`,
`org-gtd-files.el`, `org-gtd-core.el`, `org-gtd-configure.el`,
`org-gtd-upgrades.el`, `org-gtd-backward-compatibility.el`, `org-gtd.el`.

Notes on scope decisions:
- There is no `org-gtd-review.el` or `org-gtd-oops.el`; their functionality lives
  in `org-gtd-reflect.el` (with `org-gtd-review-*` / `org-gtd-oops-*` obsolete aliases)
  and `org-gtd-someday-review.el`.
- `org-gtd-context.el` and `org-gtd-configure.el` are almost entirely internal
  plumbing (context structs, type-based property configuration); only their
  user-visible surfaces are listed.
- All file:line references are to repo root `/home/stag/src/projects/org-gtd.el/`.

## Summary

| Area | Feature count |
|---|---|
| Engage / daily views | 7 (IMPL-VW-01..07) |
| Review / reflect | 12 (IMPL-VW-08..19) |
| View language & custom views | 12 (IMPL-VW-20..31) |
| Horizons & areas of focus | 5 (IMPL-VW-32..36) |
| Mode & system infrastructure | 13 (IMPL-VW-37..49) |
| Upgrades / compat | 5 (IMPL-VW-50..54) |
| **Total** | **54** |

---

## Area 1: Engage / daily views

### IMPL-VW-01 — Daily engage view
**Capability:** The user opens a single composite agenda for the day: today's schedule (Calendar + Habit items), tickler items due today, delegated check-ins due today, and all next actions.
**Entry points:** `org-gtd-engage` (command, autoloaded) — org-gtd-engage.el:84; view spec built at org-gtd-engage.el:47.
**Kind:** command / view.
**Notable behaviors:** Built entirely on the view-language DSL (`org-gtd-view-show`); uses prefix chain `(project area-of-focus "—")` so each line is prefixed with its project or area of focus; the "Today's Schedule" block is a native one-day org-agenda filtered to Calendar/Habit items that are not done (org-gtd-view-language.el:282–301).

### IMPL-VW-02 — Next actions filtered by tag
**Capability:** The user views next actions matching a chosen tag (e.g. a GTD context tag like `@phone`), with tag completion from all agenda files.
**Entry points:** `org-gtd-engage-tagged` (command, autoloaded) — org-gtd-engage.el:90; obsolete alias `org-gtd-engage-grouped-by-context` — org-gtd-engage.el:108.
**Kind:** command / view.
**Notable behaviors:** Prompts via `org-global-tags-completion-table`; accepts any tag, not just `@`-prefixed context tags (a deliberate 4.0 generalization, per the obsolete-alias docstring).

### IMPL-VW-03 — All next actions list
**Capability:** The user sees every NEXT action across all agenda files in one flat list.
**Entry points:** `org-gtd-show-all-next` (command, autoloaded) — org-gtd-engage.el:99.
**Kind:** command / view.

### IMPL-VW-04 — Agenda task action menu (transient)
**Capability:** From any line in an org-agenda buffer, the user opens a context-aware menu to act on the task: mark DONE/WAITING/NEXT/cancel/cycle TODO, defer its GTD timestamp by one day, set a new date, clock in/out, set effort/priority/tags, add a note, set area of focus, or send the item into the clarify workflow (refile or in-place).
**Entry points:** `org-gtd-agenda-transient` (transient prefix, autoloaded) — org-gtd-agenda-transient.el:203; suggested binding `C-c .` in `org-agenda-mode-map` (docstring).
**Kind:** command (transient menu).
**Notable behaviors:** Header shows the selected task's title; the "Time" section (defer/set date) only appears for types with deferrable timestamps — Calendar, Delegated, Tickler, explicitly excluding Habit (org-gtd-agenda-transient.el:54–58); state changes go through `org-agenda-todo` so the agenda display updates immediately, then the underlying buffer is saved (org-gtd-agenda-transient.el:84–95); "Defer 1 day" adds one day to `ORG_GTD_TIMESTAMP` (org-gtd-agenda-transient.el:134); clarify-in-place simulates `C-u` on `org-gtd-clarify-agenda-item` (org-gtd-agenda-transient.el:155).

### IMPL-VW-05 — Set/change GTD timestamp from anywhere
**Capability:** The user sets or changes the date of a Calendar, Delegated, or Tickler item from either an org heading or an agenda line, with a date prompt that supports time ranges.
**Entry points:** `org-gtd-set-timestamp` (command, autoloaded) — org-gtd-core.el:83; date prompt with time-range support `org-gtd-prompt-for-active-date` — org-gtd-configure.el:45; related heading-only command `org-gtd-set-event-date-on-heading-at-point` — org-gtd-core.el:446.
**Kind:** command.
**Notable behaviors:** Rejects items that are not one of the three timestamped types with a message (org-gtd-core.el:55–80); `org-gtd-set-event-date-on-heading-at-point` also rewrites the inline timestamp in the entry body, not just the property.

### IMPL-VW-06 — Smart agenda line prefixes (project / area of focus / file)
**Capability:** Agenda lines in org-gtd views automatically show a left-hand prefix resolved through a fallback chain — parent project name, then area of focus, then file name (or literal strings) — truncated/padded to a configurable width.
**Entry points:** resolver chain `org-gtd-agenda--resolve-prefix-chain` — org-gtd-agenda.el:110 (invoked from generated `org-agenda-prefix-format` strings, org-gtd-view-language.el:947–954); customizations `org-gtd-prefix-width` — org-gtd-core.el:230 (obsolete alias `org-gtd-engage-prefix-width`, org-gtd-engage.el:42) and `org-gtd-agenda-truncate-ellipsis` — org-gtd-core.el:239.
**Kind:** automatic-behavior + customization.
**Notable behaviors:** Project name resolution strips org links and `[n/m]` progress cookies (org-gtd-agenda.el:56–62); area-of-focus resolution distinguishes an explicit `CATEGORY` drawer entry from one inherited via the parent project's ID (`ORG_GTD_PROJECT_IDS` lookup, org-gtd-agenda.el:71–89); `"???"` categories are treated as unset.

### IMPL-VW-07 — Org property display on agenda lines
**Capability:** The user sees chosen org properties (default: `LOCATION`) rendered beside or below each agenda item, in a comment face.
**Entry points:** `org-gtd-agenda-property-add-properties` (autoloaded, installed on `org-agenda-finalize-hook` when `org-gtd-mode` is on — org-gtd-mode.el:152) — org-gtd-agenda-property.el:117; customizations `org-gtd-agenda-property-list` (:86), `org-gtd-agenda-property-separator` (:91), `org-gtd-agenda-property-column` (:96), `org-gtd-agenda-property-position` (:101, same-line / next-line / where-it-fits), face `org-gtd-agenda-property-face` (:111); helper command `org-gtd-agenda-property-customize` (:81).
**Kind:** automatic-behavior + customization.
**Notable behaviors:** Vendored fork of Malabarba's `org-agenda-property` package; property display lines carry the `org-marker` text property so agenda commands still work when point is on the property line (org-gtd-agenda-property.el:156–167); org links inside property values are replaced by their descriptions.

---

## Area 2: Review / reflect

### IMPL-VW-08 — Review by area of focus
**Capability:** The user picks one of their areas of focus and gets a multi-block overview: active projects, tickler projects, next actions, reminders (calendar), routines (habits), and future tickler items — all filtered to that area.
**Entry points:** `org-gtd-reflect-area-of-focus` (command, autoloaded) — org-gtd-reflect.el:70; view specs at org-gtd-reflect.el:40; obsolete alias `org-gtd-review-area-of-focus` (:341).
**Kind:** command / view.
**Notable behaviors:** Validates the chosen area against `org-gtd-areas-of-focus` and signals a dedicated error `org-gtd-invalid-area-of-focus` otherwise (org-gtd-reflect.el:82, 214); names the agenda buffer `*Org Agenda: AREA*`.

### IMPL-VW-09 — Missed items view
**Capability:** The user sees calendar events, tickler items, and delegated check-ins whose timestamps are in the past.
**Entry points:** `org-gtd-reflect-missed-items` (command) — org-gtd-reflect.el:105; specs `org-gtd-reflect-missed-items-view-specs` (:91); obsolete aliases `org-gtd-review-missed-items` (:345) and var alias (:88).
**Kind:** command / view.

### IMPL-VW-10 — Stuck-metadata views (calendar / delegated / habit / tickler)
**Capability:** The user audits items whose required metadata is missing or invalid: calendar/habit/tickler items lacking a valid timestamp, and delegated items lacking a timestamp or a delegate.
**Entry points:** `org-gtd-reflect-stuck-calendar-items` — org-gtd-reflect.el:113; `org-gtd-reflect-stuck-delegated-items` — :121; `org-gtd-reflect-stuck-habit-items` — :131; `org-gtd-reflect-stuck-tickler-items` — :139 (all commands); obsolete `org-gtd-review-stuck-*` aliases (:349–:362, including `stuck-incubated-items` → tickler).
**Kind:** command / view.
**Notable behaviors:** Backed by `org-gtd-view-lang--build-skip-function-for-stuck-type` (org-gtd-view-language.el:517), which uses OR logic across missing fields and excludes tasks whose projects are all done/cancelled.

### IMPL-VW-11 — Stuck projects view
**Capability:** The user sees projects that still have undone tasks but no NEXT or WAIT task — i.e., no defined next action.
**Entry points:** `org-gtd-reflect-stuck-projects` (command, autoloaded) — org-gtd-reflect.el:160; obsolete alias `org-gtd-review-stuck-projects` (:365).
**Kind:** command / view.
**Notable behaviors:** Done/cancelled projects are excluded (org-gtd-view-language.el:563–568).

### IMPL-VW-12 — Stuck single actions view
**Capability:** The user finds single actions (ORG_GTD=Actions) that are undone but not in NEXT state — anomalous, since single actions should always be NEXT.
**Entry points:** `org-gtd-reflect-stuck-next-action-items` (command) — org-gtd-reflect.el:170; obsolete aliases `org-gtd-reflect-stuck-single-action-items` (:180, "4.1.0") and `org-gtd-review-stuck-single-action-items` (:369).
**Kind:** command / view.

### IMPL-VW-13 — Someday/maybe list view
**Capability:** The user lists all someday/maybe items in one agenda view for periodic review.
**Entry points:** `org-gtd-reflect-someday-maybe` (command, autoloaded) — org-gtd-reflect.el:148.
**Kind:** command / view.

### IMPL-VW-14 — Recently completed items view
**Capability:** The user reviews everything completed in the last N days (default 7), e.g. for a weekly review; a numeric prefix argument changes the window.
**Entry points:** `org-gtd-reflect-completed-items` (command, autoloaded, `"P"` prefix) — org-gtd-reflect.el:184; obsolete alias `org-gtd-review-completed-items` (:373).
**Kind:** command / view.
**Notable behaviors:** Uses the DSL `done` filter, which matches on the `CLOSED` timestamp (org-gtd-view-language.el:672–726).

### IMPL-VW-15 — Completed projects view
**Capability:** The user lists projects whose tasks are all done, to identify candidates for archiving.
**Entry points:** `org-gtd-reflect-completed-projects` (command, autoloaded) — org-gtd-reflect.el:200; obsolete alias (:377).
**Kind:** command / view.

### IMPL-VW-16 — Missed engagements views (formerly "oops")
**Capability:** The user reviews everything they failed to act on in time: missed delegated check-ins, missed appointments, projects past their deadline, and projects past their scheduled start — together or per category.
**Entry points:** `org-gtd-reflect-missed-engagements` (all four blocks) — org-gtd-reflect.el:254; `org-gtd-reflect-missed-delegated` — :261; `org-gtd-reflect-missed-calendar` — :267; `org-gtd-reflect-missed-projects` — :273 (all commands, autoloaded); specs at :228; obsolete aliases for the whole `org-gtd-oops*` and `org-gtd-review-missed*` families (:320–:394).
**Kind:** command / view.
**Notable behaviors:** "Projects that should have started" excludes habits via a `not-habit` filter key (org-gtd-reflect.el:241–244).

### IMPL-VW-17 — Upcoming delegated check-ins view
**Capability:** The user sees delegated items whose check-in date is in the future, for planning follow-ups.
**Entry points:** `org-gtd-reflect-upcoming-delegated` (command, autoloaded) — org-gtd-reflect.el:281; spec at :247; obsolete alias (:397).
**Kind:** command / view.

### IMPL-VW-18 — User-defined custom missed views
**Capability:** The user appends their own DSL view specs to the missed-engagements review via a customization variable, and runs the combined review with one command.
**Entry points:** customization `org-gtd-reflect-missed-custom-views` — org-gtd-reflect.el:295 (obsolete var aliases `org-gtd-oops-custom-views`, `org-gtd-review-missed-custom-views`); command `org-gtd-reflect-missed-with-custom` (autoloaded) — :309.
**Kind:** customization + command / view.

### IMPL-VW-19 — Interactive one-at-a-time someday review session
**Capability:** The user steps through someday/maybe items one per buffer in a dedicated read-only review mode, choosing per item to defer (stamps a "Reviewed" entry in the LOGBOOK), clarify (reactivates the item into the clarify pipeline), or quit; a session summary reports counts at the end.
**Entry points:** `org-gtd-reflect-someday-review` (command, autoloaded) — org-gtd-someday-review.el:185; major mode `org-gtd-someday-review-mode` (autoloaded) — :158; keys `d`/`c`/`q` → `org-gtd-someday-review-defer` (:208), `org-gtd-someday-review-clarify` (:227), `org-gtd-someday-review-quit` (:244).
**Kind:** command + major mode / workflow.
**Notable behaviors:** When `org-gtd-someday-lists` (defined in org-gtd-someday.el:41) is configured, prompts which list to review, including an "Unassigned" option for items without `ORG_GTD_SOMEDAY_LIST` (:190–:198); deferring writes `- Reviewed [timestamp]` into the item's LOGBOOK (:122); each item is shown in a WIP temp buffer with a header line showing position `(n/total)` (:251–:271); explicit evil-mode integration forces emacs state in the review buffer (:178–:180).

---

## Area 3: View language & custom views (DSL)

### IMPL-VW-20 — `org-gtd-view-show`: declarative custom agenda views
**Capability:** The user (or any org-gtd module) displays an agenda from a declarative alist spec — `((name . "...") (type . delegated) (when . past))` — instead of hand-writing `org-agenda-custom-commands`; a single spec or a list of specs (rendered as stacked blocks) is accepted.
**Entry points:** `org-gtd-view-show` (autoloaded, interactive) — org-gtd-view-language.el:1081; translation pipeline `org-gtd-view-lang--create-custom-commands` — :342.
**Kind:** DSL / public API.
**Notable behaviors:** Optional KEYS argument assigns the agenda dispatch key, so with `org-agenda-sticky` users can keep multiple independent GTD views open simultaneously (`*Org Agenda(a)*`, `*Org Agenda(c)*`, ...) (docstring :1116–:1127); unknown filter keys raise a `user-error` listing them (:776–:780); obsolete type key `stuck-single-action` is silently normalized to `stuck-next-action` (:604–:621).

### IMPL-VW-21 — Type filters (simple GTD types)
**Capability:** The DSL lets users select items by GTD type: `next-action`, `delegated`, `calendar`, `tickler`, `someday`, `project`, `habit`, `reference`, `trash`, `quick-action`.
**Entry points:** type list — org-gtd-view-language.el:215; match-string builder — :479; commentary spec :34–:44.
**Kind:** DSL.
**Notable behaviors:** Types are translated to `ORG_GTD` property matches plus implied TODO keywords (`next-action` → NEXT, `delegated` → WAIT) (:497–:507); next-action/delegated use `tags-todo` blocks, the rest use `tags` blocks (:940–:943); each type carries smart defaults — time-sensitive types (calendar/delegated/tickler) default to `(when . today)` and every type has a default block name (:189–:207).

### IMPL-VW-22 — Computed type filters (state queries)
**Capability:** The DSL answers system-health questions with computed types: `stuck-project`, `active-project`, `completed-project`, `tickler-project`, `incubated-project`, `stuck-delegated`, `stuck-calendar`, `stuck-tickler`, `stuck-habit`, `stuck-next-action`.
**Entry points:** type list — org-gtd-view-language.el:220; specialized skip-function builders — :517 (stuck metadata), :553 (project states), :581 (tickler-project), :591 (incubated-project), :623 (stuck-next-action).
**Kind:** DSL.
**Notable behaviors:** `tickler-project`/`incubated-project` match items whose `PREVIOUS_ORG_GTD` is "Projects", i.e. projects that were put to sleep (:581–:602); stuck queries skip tasks belonging only to inactive projects (fail-open if a project ID can't be resolved — org-gtd-skip.el:408–:427).

### IMPL-VW-23 — Semantic time filters and time-window comparison expressions
**Capability:** The user filters by time semantically (`(when . past/today/future)` resolved against the type's own timestamp property, plus `deadline` and `scheduled` variants) or by precise windows using comparison expressions `(FILTER . (< "7d"))` with units m/h/d/w/M/y and signed durations.
**Entry points:** predicate wiring — org-gtd-view-language.el:824–:912; duration parsing `org-gtd--parse-relative-time` / `org-gtd--duration-to-reference-time` — org-gtd-skip.el:509, :524; validation — org-gtd-view-language.el:752.
**Kind:** DSL.
**Notable behaviors:** `when` requires a `type` and errors if the type has no `:when` property (:785–:789); invalid operators or duration strings produce targeted `user-error`s (:752–:764); `"today"` is accepted as a special duration.

### IMPL-VW-24 — Completion (`done`) filters
**Capability:** The user queries completed work: `(done . t)`, symbolic windows (`recent`, `today`, `past-day/week/month/year`), a number of days, or comparison expressions like `(done . (< "7d"))`.
**Entry points:** block builder — org-gtd-view-language.el:728; skip-function builder — :672; day mapping — :648.
**Kind:** DSL.
**Notable behaviors:** Matches against the `CLOSED` timestamp; time flows backward for done filters — explicit future durations like `"+7d"` are rejected with an explanatory error (:662–:669).

### IMPL-VW-25 — Metadata & structural filters
**Capability:** The user further narrows views by: TODO keywords (`todo`), area of focus (`area-of-focus`), delegation recipient (`who`, where nil/"" means "missing recipient"), tags (OR semantics), priority (single, list, or comparisons like `(>= B)`; nil = no priority), effort ranges (`<`, `>`, `between` on durations; nil = no effort), total clocked time (`clocked`), and recency of last clock-out (`last-clocked-out`, nil = never clocked).
**Entry points:** predicate composition — org-gtd-view-language.el:812–:921; predicate library — org-gtd-skip.el:75–:442 (priority :148, tags :168, area-of-focus :181, todo :286, clocked :296, last-clocked-out :326, effort :357).
**Kind:** DSL.
**Notable behaviors:** Priority, tags, and area-of-focus fall back to the parent project's value via `ORG_GTD_PROJECT_IDS` when the task itself has none (org-gtd-skip.el:159–:164, :176–:179, :186–:190); `last-clocked-out` on a project heading aggregates over all its tasks (org-gtd-skip.el:334–:338); all simple-type views implicitly exclude done items and tasks from fully inactive projects (org-gtd-view-language.el:916–:919).

### IMPL-VW-26 — Native escape hatch
**Capability:** Inside any view spec, the user can embed a raw org-agenda block — `(native . (BLOCK-TYPE MATCH SETTINGS))` — passed through untransformed, and mix it freely with DSL blocks (e.g. for custom sorting strategies).
**Entry points:** org-gtd-view-language.el:251–:255; commentary :120–:137.
**Kind:** DSL.

### IMPL-VW-27 — Multi-block views, implicit block expansion, and layered defaults
**Capability:** The user composes multi-section views either explicitly via a `blocks` key or implicitly by repeating `type` keys in one spec (each expands to its own block); shared filters written at top level (e.g. `area-of-focus`) are inherited into each block.
**Entry points:** implicit expansion `org-gtd-view-lang--expand-implicit-blocks` — org-gtd-view-language.el:1037; four-tier precedence `org-gtd-view-lang--apply-defaults` — :1010.
**Kind:** DSL.
**Notable behaviors:** Precedence is block-explicit > top-level explicit > per-type smart default > global default (:1010–:1035); special block types exist for a calendar-day agenda (`block-type . calendar-day`), raw todo blocks, native day agendas (`view-type . agenda` with `agenda-span`/`show-habits`), and tag-grouped views including dynamic context discovery from `@`-tags present in agenda files (`view-type . tags-grouped`, `group-by . context`) (:264–:272, :388–:449).

### IMPL-VW-28 — Prefix fallback-chain DSL
**Capability:** The user declares per-view agenda prefixes as an ordered fallback chain of symbols/literals — `(prefix . (project area-of-focus "—"))` with optional `prefix-width` — and the first element that resolves is shown.
**Entry points:** expansion `org-gtd-view-lang--expand-prefix` — org-gtd-view-language.el:947; default chain `(project area-of-focus file-name)` — :209; resolution at display time — org-gtd-agenda.el:96–:116.
**Kind:** DSL.
**Notable behaviors:** Parent views' prefix settings are inherited by their blocks unless a block overrides them (:967–:982); a default prefix chain is injected when none is specified (:1049–:1053).

### IMPL-VW-29 — Reusable skip functions for hand-rolled agendas
**Capability:** Users building their own `org-agenda-custom-commands` can reuse org-gtd's skip functions: keep only not-done items, or only items in a given area of focus.
**Entry points:** `org-gtd-skip-unless-in-progress` — org-gtd-skip.el:57; `org-gtd-skip-unless-area-of-focus` / `-func` (factory) — org-gtd-skip.el:61, :69.
**Kind:** public helper API (borderline; the rest of org-gtd-skip.el is the internal predicate engine behind the DSL).

### IMPL-VW-30 — Filter-key validation with actionable errors
**Capability:** When a user writes a view spec with a typo'd or unsupported key, org-gtd refuses to render and names the offending key(s) instead of silently ignoring them.
**Entry points:** known-key list — org-gtd-view-language.el:178; validation in `org-gtd-view-lang--build-skip-function` — :776.
**Kind:** automatic-behavior (DSL ergonomics).

### IMPL-VW-31 — Multiple simultaneous sticky GTD views
**Capability:** With `org-agenda-sticky` enabled, the user keeps several independent GTD views open at once by giving each `org-gtd-view-show` call a distinct dispatch key.
**Entry points:** KEYS parameter of `org-gtd-view-show` — org-gtd-view-language.el:1081 (key default "g", :1133).
**Kind:** view capability (documented usage pattern built into the API).

---

## Area 4: Horizons & areas of focus

### IMPL-VW-32 — Configurable areas of focus (GTD Horizon 2)
**Capability:** The user declares their life areas (default: Home, Health, Family, Career) as a customization; these drive completion, validation, and review filtering throughout the package.
**Entry points:** `org-gtd-areas-of-focus` (defcustom) — org-gtd-areas-of-focus.el:39.
**Kind:** customization.

### IMPL-VW-33 — Assign area of focus to a heading
**Capability:** The user tags the org heading at point with one of their areas of focus (stored in the `CATEGORY` property, with required-match completion).
**Entry points:** `org-gtd-area-of-focus-set-on-item-at-point` (command) — org-gtd-areas-of-focus.el:47.
**Kind:** command.
**Notable behaviors:** `CATEGORY` doubles as the org-agenda category, so the area shows up in agenda prefixes (org-gtd-core.el:141–:143).

### IMPL-VW-34 — Assign area of focus from the agenda (project-aware)
**Capability:** From an agenda line, the user sets the area of focus; if the item is a project task, the area is propagated to every task in the project (prompting once).
**Entry points:** `org-gtd-area-of-focus-set-on-agenda-item` (command) — org-gtd-areas-of-focus.el:57; project-wide propagation — :93.
**Kind:** command.
**Notable behaviors:** Errors if the item has no `ORG_GTD` property; multi-project membership is resolved through `org-gtd-project--get-marker-at-point`.

### IMPL-VW-35 — Area-of-focus prompt as an organize hook
**Capability:** The user can have org-gtd ask "Which area of focus?" automatically every time an item is organized, by adding `org-gtd-set-area-of-focus` to their organize hooks.
**Entry points:** `org-gtd-set-area-of-focus` (alias) / `org-gtd-areas-of-focus--set` — org-gtd-areas-of-focus.el:89, :108.
**Kind:** hook function (opt-in automatic behavior).
**Notable behaviors:** Skips item types where an area makes no sense: project tasks, trash, knowledge, quick actions (:114).

### IMPL-VW-36 — Horizons file (purpose/vision/goals reference)
**Capability:** The user maintains a horizons file (default `horizons.org` in the GTD directory) — auto-created from a template with Purpose/Vision/Goals/Areas-of-focus headings — which can be shown as a side window during clarification to keep decisions aligned with higher horizons.
**Entry points:** customization `org-gtd-horizons-file` — org-gtd-horizons.el:40; template — :51; buffer accessor `org-gtd--horizons-file` — :62; consumed by `org-gtd-clarify-show-horizons` and `org-gtd-clarify-toggle-horizons-window` (org-gtd-clarify.el:53, :279 — clarify module, listed here for the horizons surface).
**Kind:** customization + automatic-behavior (auto-creation).

---

## Area 5: Mode & system infrastructure

### IMPL-VW-37 — `org-gtd-mode` global minor mode
**Capability:** The user enables one global mode that wires org-gtd into Emacs: an inbox-count mode-line lighter (`GTD[5]`), org-edna task dependencies, all automatic state-change behaviors, agenda property display, and WIP temp-file cleanup at exit.
**Entry points:** `org-gtd-mode` (define-minor-mode, autoloaded) — org-gtd-mode.el:83; enable/disable plumbing — :131, :104.
**Kind:** command / mode (automatic-behavior bundle).
**Notable behaviors:** Saves and restores the user's previous `org-edna-mode` state on disable (:141, :113); also advises every `org-agenda*` command with `org-gtd--wrap`, which since v4 is a pass-through retained for compatibility (:138, :160–:167).

### IMPL-VW-38 — Inbox count in the mode line
**Capability:** The user sees a live count of unprocessed inbox items (main inbox plus any `org-gtd-additional-inbox-files`) in the mode line, refreshed periodically to catch external changes (e.g. mobile capture syncing).
**Entry points:** lighter `org-gtd-mode-lighter` — org-gtd-mode.el:207; counting `org-gtd-inbox-count` — :182; customizations `org-gtd-mode-update-interval` (seconds, nil disables timer) — :57 and `org-gtd-mode-lighter-display` (always / never / when-non-zero) — :66.
**Kind:** automatic-behavior + customization.

### IMPL-VW-39 — Automatic CLOSED timestamps on GTD items
**Capability:** When the user marks any GTD-managed heading done, a CLOSED timestamp is added automatically (without requiring `org-log-done`), so completion-based reviews work.
**Entry points:** `org-gtd--add-closed-timestamp` on `org-after-todo-state-change-hook` (installed by org-gtd-mode) — org-gtd-mode.el:171, :144.
**Kind:** automatic-behavior.
**Notable behaviors:** Only fires for headings with an `ORG_GTD` property; never overwrites an existing CLOSED stamp.

### IMPL-VW-40 — Automatic project bookkeeping on TODO state changes
**Capability:** While `org-gtd-mode` is on, completing or changing task states automatically keeps projects consistent: project progress cookies update, a single action set to WAIT is converted into a Delegated item, and cancelling is detected at the project level.
**Entry points:** hooks installed at org-gtd-mode.el:146–:150 — `org-gtd-project--maybe-update-cookies`, `org-gtd-next-action--maybe-convert-to-delegated`, `org-gtd-project--maybe-cancel-from-hook` (implementations live in the projects/next-action modules; the wiring is this module's feature).
**Kind:** automatic-behavior.

### IMPL-VW-41 — GTD command center menu
**Capability:** The user opens a single discoverable transient menu covering the whole workflow — engage views, capture/process/clarify, reflect views, archiving — with nested submenus for stuck-item and missed-item reviews.
**Entry points:** `org-gtd-command-center` (transient prefix, autoloaded) — org-gtd-command-center.el:42; submenus `--stuck` — :67 and `--missed` — :78.
**Kind:** command (transient menu).

### IMPL-VW-42 — Six-stage organization hook system
**Capability:** Users and extensions can attach functions at six pipeline stages — before/after clarify, before/after organize, before/after file — globally (defvar hooks) or per GTD type (via `:hooks` in the type plist); each hook receives the heading's point-or-marker.
**Entry points:** global hooks `org-gtd-before-clarify-hook` ... `org-gtd-after-file-hook` — org-gtd-hooks.el:40–:68; dispatcher `org-gtd-hooks-run` — :97.
**Kind:** hook point.
**Notable behaviors:** Hooks are observers only — they cannot abort the pipeline; errors are caught and logged so a buggy hook can't break organization (:87–:93); global hooks run before type-local hooks.

### IMPL-VW-43 — GTD directory and automatic file creation
**Capability:** The user sets one directory (`~/gtd/` by default) as the home of their GTD system; the default tasks file (`org-gtd-tasks.org`) and horizons file are created automatically with proper org-gtd setup when first needed.
**Entry points:** customization `org-gtd-directory` — org-gtd-core.el:187; file creation `org-gtd--default-file` / `org-gtd--ensure-file-exists` / `org-gtd--path` — org-gtd-files.el:41, :47, :55.
**Kind:** customization + automatic-behavior.
**Notable behaviors:** `org-gtd-buffer-p` (org-gtd-core.el:542) defines "managed buffer" as any file under this directory; `org-gtd-archive-location` (org-gtd-core.el:202) defaults to a per-year datetree archive file in this directory (a `defvar` holding a lambda, overridable before load).

### IMPL-VW-44 — Auto-save after organizing
**Capability:** The user can opt in to having all modified org-gtd buffers saved automatically after each organize step.
**Entry points:** customization `org-gtd-save-after-organize` — org-gtd-core.el:212; effector `org-gtd-save-buffers` — :551.
**Kind:** customization.
**Notable behaviors:** Only saves buffers whose files live under `org-gtd-directory`.

### IMPL-VW-45 — Project progress-cookie position
**Capability:** The user controls where `[n/m]` progress cookies appear on project headings: disabled, after the TODO keyword, or at the end before tags (default).
**Entry points:** customization `org-gtd-project-progress-cookie-position` — org-gtd-core.el:218 (consumed by the projects module).
**Kind:** customization.

### IMPL-VW-46 — Custom TODO keyword mapping with validation
**Capability:** The user maps GTD semantic states (todo/next/wait/done/canceled) onto their own `org-todo-keywords`, so org-gtd works with any keyword scheme; invalid mappings are rejected with a corrective error message including an example configuration.
**Entry points:** customization `org-gtd-keyword-mapping` (with `:set` validator) — org-gtd-core.el:377; validation — :261–:342; semantic accessors `org-gtd-keywords--next` etc. — :516–:534; obsolete individual variables `org-gtd-todo-keyword` etc. — :411–:437.
**Kind:** customization + automatic validation.
**Notable behaviors:** Validates that all five keywords exist in `org-todo-keywords` and share a single sequence; handles the `"NEXT(n/@)"` shortcut DSL; validation is skipped during byte compilation/loading.

### IMPL-VW-47 — Keyword setup wizard
**Capability:** The user runs an interactive wizard that walks through mapping each GTD semantic state to one of their existing TODO keywords and persists the result via Customize.
**Entry points:** `org-gtd-setup-keywords-wizard` (command, autoloaded) — org-gtd-core.el:348.
**Kind:** command.
**Notable behaviors:** Saves with `customize-save-variable` so the validated mapping survives restarts; errors helpfully if no TODO keywords are configured. (Note: the wizard saves todo/next/wait/canceled but not an explicit `done` mapping — the validator requires `done`, a potential gap.)

### IMPL-VW-48 — Set event date on heading (timestamp + body sync)
**Capability:** The user updates a GTD event's date on the heading at point and the inline timestamp in the entry body is rewritten to match.
**Entry points:** `org-gtd-set-event-date-on-heading-at-point` (command) — org-gtd-core.el:446.
**Kind:** command.

### IMPL-VW-49 — Multi-context resolution (org buffer / agenda / graph view)
**Capability:** Project-level commands work identically whether the user invokes them from an org buffer, an agenda line, or the project graph view; org-gtd resolves project/task identity from whichever context point is in.
**Entry points:** `org-gtd-context-at-point` — org-gtd-context.el:63 (struct at :42; per-mode resolvers :77, :94, :111).
**Kind:** automatic-behavior (infrastructure with direct user-visible effect: context-agnostic commands; signals "Must be in org buffer, agenda, or graph view" otherwise).

---

## Area 6: Upgrades / compat

### IMPL-VW-50 — v2 → v3 data migration
**Capability:** The user migrates v2 data with one command: calendar/delegated/incubated items move off the misused SCHEDULED keyword onto `ORG_GTD_TIMESTAMP`, and habits are relocated into their own subtree.
**Entry points:** `org-gtd-upgrade-v2-to-v3` (command) — org-gtd-upgrades.el:38; per-category steps — :53, :72, :90, :113.
**Kind:** command (migration).

### IMPL-VW-51 — v3 → v4 data migration
**Capability:** The user migrates v3 data to the v4 property-based dependency model with one command running five steps: split Incubated into Tickler/Someday, move `ORG_GTD` from category headings onto items (renaming the heading property to `ORG_GTD_REFILE`), split delegated items out of Actions, stamp habits with `ORG_GTD`, and build project dependency properties (`ORG_GTD_DEPENDS_ON`/`ORG_GTD_BLOCKS`/`ORG_GTD_FIRST_TASKS`) with recalculated NEXT/TODO states.
**Entry points:** `org-gtd-upgrade-v3-to-v4` (command, autoloaded) — org-gtd-upgrades.el:158; steps — :289, :342, :236, :262, :413.
**Kind:** command (migration).
**Notable behaviors:** Requires a yes/no backup confirmation before touching files; steps are designed to be idempotent ("Safe to run multiple times"); progress messages per item.

### IMPL-VW-52 — Major-version upgrade warning with acknowledgment
**Capability:** On load, the user is shown a prominent warning when their acknowledged version (`org-gtd-update-ack`) is older than the current major version, with pointers to the upgrade command and manual; setting the variable silences it.
**Entry points:** `org-gtd-update-ack` (defvar) — org-gtd.el:94; warning logic — :104–:128.
**Kind:** automatic-behavior + configuration variable.

### IMPL-VW-53 — Backward-compatibility alias layer
**Capability:** Users upgrading from v3.x keep working configurations and muscle memory: the entire `org-gtd-oops-*` and `org-gtd-review-*` command/variable families alias to `org-gtd-reflect-*`, `org-gtd-engage-grouped-by-context` aliases to `org-gtd-engage-tagged`, `org-gtd-incubate` aliases to `org-gtd-tickler`, the deprecated `with-org-gtd-context` macro degrades to a warning no-op, and obsolete DSL type keys are normalized.
**Entry points:** reflect aliases — org-gtd-reflect.el:316–:402; engage alias — org-gtd-engage.el:108; `with-org-gtd-context` — org-gtd-core.el:465; incubate alias — org-gtd-core.el:110; DSL key normalization — org-gtd-view-language.el:604.
**Kind:** automatic-behavior (compatibility).

### IMPL-VW-54 — Older-Emacs compatibility shims
**Capability:** The package runs on Emacs 28.1+ by pulling in the `compat` library for functions from newer Emacs versions (`ensure-list`, `file-name-concat`, `string-pad`).
**Entry points:** org-gtd-backward-compatibility.el:39–:41 (required by org-gtd-core.el:34).
**Kind:** automatic-behavior (infrastructure).

---

*Numbering note: 54 features, IMPL-VW-01 through IMPL-VW-54, no gaps. IMPL-VW-29 (reusable skip helpers) is flagged borderline (public helper API vs internal engine) but counted.*
