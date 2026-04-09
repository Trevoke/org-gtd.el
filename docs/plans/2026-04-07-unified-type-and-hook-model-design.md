# Unified Type and Hook Model — Design

**Date:** 2026-04-07
**Status:** Draft (revised after subsystem review)

## Problem

The `org-gtd-[type]` commands (`org-gtd-calendar`, `org-gtd-tickler`, `org-gtd-someday`, …) are intended as DWIM entry points: take the heading at point — inbox item, existing GTD item, or project — and turn it into an item of that type. In practice they are inconsistent:

- Only `someday` and `tickler` detect project-heading / project-task context; the rest silently misbehave on non-inbox headings.
- Only `someday` and `tickler` call `org-gtd-save-state` before reclassifying, so reactivation is lossy for the others.
- Archive-bound types (`knowledge`, `quick-action`, `trash`) ignore `org-gtd-clarify--skip-refile`.
- `trash` lacks the standard `--configure` / `--finalize` / `--apply` split.
- `-create` helpers exist for some types and not others.

The shape of each per-type module is largely copy-paste, but the copies have drifted. We want one flow, one set of seams for customization, and a type table that stays GTD-flavored rather than becoming a schema registry.

## Foundation that already exists

The refactor is additive on top of machinery already in the tree. It is worth stating this explicitly because it shrinks the work considerably.

- **`org-gtd-types.el`** already holds a type registry as `org-gtd-types` (alist of `(type-name . plist)`) with `:org-gtd`, `:state`, and `:properties`. The `:properties` shape is rich: each entry carries `:org-property`, `:type` (text / timestamp / repeating-timestamp), `:required`, `:prompt`, `:default`, `:input-fn`. Accessors (`org-gtd-type-get`, `org-gtd-type-property`, `org-gtd-type-from-org-gtd-value`) and merge helpers (`org-gtd--merge-type-definitions`) are in place.
- **`org-gtd-user-types`** is an existing defcustom that lets users override fields on built-in types via merge semantics. It currently forbids adding new types; we will relax that.
- **`org-gtd-configure-as-type`** in `org-gtd-configure.el` is already the generic "configure heading as type X" engine: it reads the type's `:properties`, sets `ORG_GTD`, sets the TODO `:state`, and loops over required properties prompting / defaulting / calling `:input-fn`. The per-type `--configure` functions are already thin wrappers around it.
- **`org-gtd-refile--do`** already takes `(type refile-target-element)` and is parameterized by type. `org-gtd-refile-prompt-for-types` is the user-facing knob for "which types should prompt on refile."
- **View DSL (`org-gtd-view-language.el`)** consumes `org-gtd-types` directly to resolve time filters from the type's timestamp property.
- **Skip predicates (`org-gtd-skip.el`)** also consume the registry via closures.

Much of what an earlier draft of this design proposed as "new" is actually "extend these and delete the per-type duplication." The design below reflects that.

## Goals

1. One uniform flow for `org-gtd-<type>` commands covering inbox, existing GTD, and project contexts.
2. Make type conversions non-lossy across the board (save-state everywhere, foreign-property clearing everywhere).
3. Give users a visually obvious, minimal surface for customizing types and adding new ones.
4. Give users hook seams at every meaningful stage of the flow without letting the type table grow unboundedly.
5. Preserve the existing DSL view language, skip predicates, reactivate, clarify/WIP, and refile machinery untouched wherever possible.

## Non-goals

- Redesigning computed view types (`stuck-delegated`, `active-project`, …). They stay hardcoded.
- Replacing `org-gtd-user-types`. It continues to work; the new customization API coexists.
- Changing the clarify/WIP buffer mechanics or the transient organize menu.
- Reworking the refile subsystem beyond what's needed to drive it from the type table.

## Two-layer model

Two layers, nothing more:

**Layer 1 — The type table (`org-gtd-types.el`, extended).** GTD vocabulary *and* implementation wiring live together, but visually grouped so the GTD fields read first. Each entry is declared with one form, `org-gtd-define-type`, which takes a keyword plist. Core fields (all existing today):

| Field | Purpose |
|---|---|
| `:org-gtd` | String value of the `ORG_GTD` property. GTD vocabulary. |
| `:state` | TODO keyword or `nil`. GTD vocabulary. |
| `:properties` | List of org-property descriptors (existing shape). What data the type captures. |

New fields (implementation wiring):

| Field | Purpose | Default |
|---|---|---|
| `:organize-fn` | Function that configures the heading as this type. Receives a config alist. | `#'org-gtd-configure-as-type` |
| `:disposition` | How the item leaves the clarify flow: `'list`, `'done-and-archive`, `'cancel-and-archive`, `'externalize`. | `'list` |
| `:supports` | Flags: `reactivate` (pausing type → save-state on entry, reactivate on exit), `project-handler` (type-level command dispatches to a project-level function when invoked on a project heading). | `nil` |
| `:project-fn` | If `:supports` includes `project-handler`, the function called when the command fires on a project heading. | `nil` |
| `:prompt-to-refile` | Whether the refile engine should prompt for destination on items of this type. Falls back to `org-gtd-refile-prompt-default` when unset. | unset |
| `:transient-key` | Optional key binding for the organize transient menu. | `nil` |
| `:hooks` | Plist of local hook lists (see below). | `nil` |

The intent is that most types declare GTD fields + maybe `:disposition` + maybe `:supports`, and the implementation defaults do the rest. `org-gtd-define-type` is a thin wrapper that merges into `org-gtd-types` via the existing `org-gtd--merge-type-definitions`.

**Layer 2 — Hooks.** All behavior growth that isn't "new GTD vocabulary" lives in hooks. Hooks are how you add validation, notifications, external side-effects, analytics, and so on without growing the type table.

Six stages, each a list of functions called with one argument, a point-or-marker:

```
before-clarify   after-clarify
before-organize  after-organize
before-file      after-file
```

Each stage exists in two flavors:

- **Global** — `org-gtd-before-organize-hook`, etc. Fire for every type.
- **Local** — declared on the type via `:hooks '(:before-organize (fn1 fn2) …)`. Fire only for that type.

Semantics:

- Signature is `(pom)`. No type argument: global hooks don't care about type; local hooks know their type from registration site.
- Return value is ignored. Hooks observe; they do not gate.
- Errors are caught and logged. A failing hook aborts *itself* and the pipeline continues to the next hook and the next stage.
- Order: global before local within a stage. (Open question — see below.)

This is the key governance move: the type table stays small and GTD-flavored; unbounded behavior extension happens in hooks, which are unbounded by design.

## Three verbs, three layers of authority

- **Hooks observe.** They cannot veto, modify arguments, or change flow.
- **`:organize-fn` decides.** Validation of user input for this type belongs here. It can `user-error` to abort.
- **Commands gate.** `org-gtd-<type>` is what decides whether to enter clarify, whether to honor `skip-refile`, whether to treat the heading as a project.

Keeping these separate is what makes the hook surface safe to expose.

## Two primitives, two callers

Underneath the per-type commands are two internal primitives:

- `org-gtd-process-heading (pom type &optional config)` — the single-heading flow. Enters clarify if appropriate, runs before-organize → `:organize-fn` → after-organize → before-file → refile/update-in-place → after-file. Honors `:supports 'reactivate` (save-state on entry). Clears foreign properties from the previously active type.
- `org-gtd-process-project (pom type &optional config)` — the project-level flow. Dispatches to `:project-fn` when the type declares one, otherwise errors.

These primitives have two callers:

**DWIM wrappers.** Every `org-gtd-<type>` command becomes a thin wrapper that runs context detection and calls the right primitive:

```elisp
(defun org-gtd-calendar ()
  (interactive)
  (org-gtd--dispatch 'calendar))
```

`org-gtd--dispatch` reads `org-get-at-bol 'org-marker` first so it works uniformly from org buffers and agenda buffers, then inspects context (project heading? project task? single item?) and routes to `process-heading` or `process-project`. This collapses the previous agenda-specific commands (`org-gtd-delegate-agenda-item` and friends) into the main command — they become obsolete aliases.

**The organize transient.** The transient fires from inside a clarify/WIP buffer during inbox processing, where there is no context detection to do. It calls `org-gtd-process-heading` directly, bypassing `--dispatch`. The transient's menu entries are built by iterating `org-gtd-types` and reading `:transient-key`, so user-defined types appear in the menu automatically — no hand-curation, no core edits when adding a type.

The per-type `--configure`, `--finalize`, `--apply` trio goes away for types that don't need anything special; a type only keeps a custom `:organize-fn` when it actually has logic that `configure-as-type` can't express.

## User-facing API

Three forms, designed to be copy-pasted into `init.el` and read top-to-bottom:

```elisp
;; Define a new type.
(org-gtd-define-type 'watching
  :org-gtd "Watching"
  :state "WAIT"
  :properties '((:when
                 :org-property "ORG_GTD_TIMESTAMP"
                 :type repeating-timestamp
                 :required t
                 :prompt "Check back on: "))
  :disposition 'list)

;; Customize an existing type. Lists append, scalars replace.
(org-gtd-customize-type 'calendar
  :hooks '(:after-file (my/sync-to-google-calendar)))

;; Customize several types at once — same merge rules.
(org-gtd-customize-type '(project knowledge)
  :prompt-to-refile t)

(org-gtd-customize-type '(next-action delegated calendar)
  :hooks '(:after-organize (my/prompt-area-of-focus)))

;; Global hooks.
(add-hook 'org-gtd-before-organize-hook #'my/validate-heading)
```

`org-gtd-customize-type` is the marquee affordance. The first argument is either a type symbol or a list of type symbols; when it's a list, the same merge is applied to each. Lists append, scalars replace. The whole point is that a user can read one block and see the GTD shape of a type *and* the behavior they've bolted onto it, without bouncing between defcustoms.

`org-gtd-refile-prompt-default` is the single defcustom that sets the baseline prompt-on-refile behavior; per-type `:prompt-to-refile` overrides it. `org-gtd-refile-prompt-for-types` is removed — its contents migrate to `:prompt-to-refile t` on the listed types at load time, with a one-time deprecation warning.

`org-gtd-user-types` continues to work — `customize-type` is a thinner, hook-aware sibling, not a replacement. Both merge into the same registry through `org-gtd--merge-type-definitions`.

## Subsystem interactions

A pass over the codebase found the following impact:

| Subsystem | File | Impact |
|---|---|---|
| Type registry | `org-gtd-types.el` | Extend plist shape with new fields; keep `:properties` as-is. |
| Configure engine | `org-gtd-configure.el` | Unchanged. Becomes the default `:organize-fn`. |
| Refile | `org-gtd-refile.el` | `refile--should-prompt-p` reads `:prompt-to-refile` from the type (falling back to `org-gtd-refile-prompt-default`) instead of the old defcustom list. Otherwise unchanged. |
| Transient menu | `org-gtd-organize.el` | Menu entries built by iterating `org-gtd-types` and reading `:transient-key`. Each entry calls `org-gtd-process-heading` directly (not the DWIM wrapper). User-defined types appear automatically. |
| Agenda-specific commands | various | `org-gtd-delegate-agenda-item` and similar become obsolete aliases pointing at the unified DWIM command, which handles agenda markers via `org-get-at-bol`. |
| View DSL | `org-gtd-view-language.el` | Unchanged. User-defined types become filterable for free. |
| Skip predicates | `org-gtd-skip.el` | Unchanged. Reads the registry through existing accessors. |
| Reactivate | `org-gtd-reactivate.el` | Unchanged. `:supports 'reactivate` gates whether the primitive calls `save-state`. |
| Clarify / WIP | `org-gtd-clarify.el`, `org-gtd-wip.el` | Unchanged. |
| Organize core | `org-gtd-organize-core.el` | Gains the two primitives and the dispatch helper. |
| Per-type modules | `org-gtd-calendar.el`, `-tickler.el`, `-someday.el`, `-delegate.el`, `-single-action.el`, `-habit.el`, `-knowledge.el`, `-quick-action.el`, `-trash.el` | Shrink to: command wrapper + type declaration + (rare) custom `:organize-fn`. `--configure`/`--finalize`/`--apply` removed where possible. Private functions remain in the type's own file (co-location). |
| Archive dispositions | `org-gtd-archive.el` (new extraction) or `organize-core.el` | Disposition runners (`done-and-archive`, `cancel-and-archive`, `externalize`) live in one place. |

The footprint outside `organize-core` and the per-type modules is essentially zero. This is very different from the earlier draft, which implied a larger rewrite.

## Migration plan

The existing foundation reduces this to four steps:

1. **Extend `org-gtd-types.el`.** Add `:organize-fn`, `:disposition`, `:supports`, `:project-fn`, `:prompt-to-refile`, `:transient-key`, `:hooks` to the plist shape. Update the merge helper to handle them. Ship `org-gtd-define-type` and `org-gtd-customize-type` (accepting a symbol or list) as thin wrappers. Add `org-gtd-refile-prompt-default` and the migration shim for `org-gtd-refile-prompt-for-types`. No behavior change yet.
2. **Add primitives and dispatch in `organize-core`.** Implement `org-gtd-process-heading`, `org-gtd-process-project`, `org-gtd--dispatch`. Wire the six hook stages. Add disposition runners.
3. **Migrate per-type modules one at a time.** For each type: populate its declaration with the new fields, delete the duplicated `--configure`/`--finalize`/`--apply` trio, rewrite the command as a dispatch wrapper. Start with the cleanest (`calendar`), end with the oddballs (`trash`, archive-bound types).
4. **Document, changelog, deprecate carefully.** `org-gtd-user-types` keeps working. `org-gtd-incubate*` aliases keep working. Announce the new API.

Each step leaves the tree green. There is no flag day.

## Open questions

- **Hook order within a stage** — global-then-local or local-then-global? Current proposal: global first, so type-specific logic runs closest to the action.
- **Do we keep per-type `-create` helpers**, or replace them with a single `org-gtd-create-item` that takes type + topic + config? Leaning toward the latter as an additive step, with the old helpers kept as aliases.
- **Where do disposition runners live** — new `org-gtd-archive.el`, or inside `organize-core`? Leaning toward extracting, because archive-bound types share nontrivial logic.

## Risks

- **Silent semantic drift during per-type migration.** The per-type modules have subtle differences (`someday` clears timestamp props defensively; `tickler` takes a `config-override` alist; `trash` skips the standard split entirely). Each migration must preserve observable behavior. Mitigation: migrate one type per commit, with regression tests per type before and after.
- **Hook misuse.** Users will try to gate behavior in hooks ("return nil to abort"). Mitigation: documentation says hooks observe, and `:organize-fn` is where decisions live. Failing loudly on non-nil returns is tempting but hostile; logging a debug message is probably enough.
- **Reviewer bandwidth vs. the filter-inheritance work in flight.** The two changes touch different files (skip/view-language vs. organize-core/types), so technical collision is minimal. The real concern is reviewer attention. Mitigation: land filter-inheritance first, then sequence the migration steps above behind it.
- **Customization API confusion.** `org-gtd-user-types` and `org-gtd-customize-type` both exist. Mitigation: docstring on `user-types` points at `customize-type` as the preferred path; both are supported indefinitely.

## Appendix: Worked example — "Watching" (user-defined type)

A user wants to track TV shows they're in the middle of, with a "check on this date" timestamp and a Slack notification when one comes due.

```elisp
(org-gtd-define-type 'watching
  :org-gtd "Watching"
  :state "WAIT"
  :properties '((:when
                 :org-property "ORG_GTD_TIMESTAMP"
                 :type repeating-timestamp
                 :required t
                 :prompt "Check back on: "))
  :disposition 'list
  :supports '(reactivate)
  :hooks '(:after-file (my/notify-slack-on-watching)))

(defun my/notify-slack-on-watching (pom)
  (org-with-point-at pom
    (my/slack-send (format "Now watching: %s" (org-get-heading t t t t)))))
```

The user now has:

- A working `org-gtd-watching` command (autogenerated by `define-type`).
- Inclusion in the view DSL for free (`((type . watching) (when . past))`).
- Skip-predicate support for free.
- Reactivate semantics (save-state on conversion *into* watching, reactivate on conversion *out of* it).
- One hook, visually attached to the type declaration.

No core code changed.
