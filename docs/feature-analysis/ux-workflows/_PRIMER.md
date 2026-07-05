# org-gtd Interaction Primer — for UX-workflow authors

> **Why you're reading this.** You are designing the *complete* user experience for one unimplemented org-gtd feature (see `_TEMPLATE.md`). To make that feature feel **native** rather than bolted-on, you must design it out of org-gtd's existing interaction vocabulary — its buffers, transients, keys, and idioms. This primer is that vocabulary, distilled from the source. Cite the anchors here (`IMPL-*`, command names, functions) in your doc's §5 "Fit with org-gtd."
>
> Golden rule: **prefer reusing a surface below over inventing a new one.** When you must invent, invent in the same idiom.

> **Version baseline — read this.** We are designing for **v5**. The reliable *released* surface is **org-gtd 4.6.1** (latest tag); HEAD is ~76 unreleased commits beyond it. Both are fair to build on — they differ only in how freely you may **rework** them:
> - **[R] Released (4.6.1)** — stable and depended-upon. You *may* propose reworking it, but only with a strong reason: it clearly **improves UX** and/or **improves fidelity to GTD**. No casual churn.
> - **[U] Unreleased / in-flight (post-4.6.1 HEAD)** — real and usable for v5, and **not frozen**. Designing these features is an active chance to refine it.
> - **[X] Experimental / removed** — tried and currently not present; resurrect and reshape freely if a feature needs it.
>
> **Bonus mandate (the maintainer wants this):** where your feature's UX reveals a way to **refine, clarify, or improve the UX of the type system / extension model**, capture it — that improvement is in-scope for v5, not a distraction (see the template's "Type / extension-UX opportunities").

---

## 1. The pipeline (the spine everything hangs off)

**capture → clarify → organize → file → engage → reflect.** Three *actions* (clarify, organize, file), around which an **observation hook layer** fires (six stages — an **[U] unreleased** extension seam; see §4). The pipeline itself is released and load-bearing; every item flows through it, and your feature almost certainly attaches to a point on it.

```
capture → [inbox.org] → process-loop → clarify (WIP buffer) → organize (transient) → file (refile/archive) → engage / reflect
                                          │before/after-clarify │before/after-organize │before/after-file
```

Naming convention mirrors the pipeline: commands are `org-gtd-<step>-<action>` (`org-gtd-process-inbox`, `org-gtd-clarify-item`, `org-gtd-reflect-stuck-projects`). Match it.

---

## 2. The interaction surfaces (your palette)

### Capture
- `org-gtd-capture` wraps `org-capture`; templates live in the **`org-gtd-capture-templates` defcustom** (users add their own). Everything lands in `org-gtd-inbox-path` (`inbox.org`). A finalizer stamps `ORG_GTD_CAPTURED_AT` on each captured heading.
- Idiom: generated files carry a **guidance header** — a `#+begin_comment` block explaining the file's purpose in-place. Reuse this if your feature creates a file.

### Process loop
- `org-gtd-process-inbox` walks inbox headings **one at a time**, passing *itself* as a **continuation** so that finishing organize on item N automatically pulls item N+1. There is **no "next item" key** — completing the disposition *is* "next." `C-c C-k` aborts the whole session.
- Takeaway: any "walk a list one item at a time" feature should reuse the **continuation** mechanism (`org-gtd-clarify--continuation`), not build its own loop.

### Clarify + the WIP buffer (the workhorse editing surface)
- A **WIP buffer** is a real, file-backed org buffer (temp file under `…/org-gtd/`, auto-save on → crash-safe) mirroring the single heading being worked. `org-gtd-clarify-mode` is derived from `org-mode`.
- Lifecycle: **open** (`org-gtd-clarify-item`; `C-u` = *update in place*, no refile) → **edit** (free org editing) → **organize** (`C-c C-o` → transient) → **file/close** (cuts the original from source, cleans the temp file, **restores the saved window configuration**, runs the continuation).
- Header-line advertises the keys live: *"Clarify item. `C-c C-o` to file, `C-c d` to duplicate, `C-c C-k` to cancel."*
- WIP infra (`org-gtd-wip--get-buffer` etc.) is **generic** — reuse it for any "pull an item into an isolated editing surface" feature.

### Organize transient (disposition dispatch)
- `org-gtd-organize` (a `transient-define-prefix`) is the menu of what an item can *become*. Groups: **Options** (`-n` update-in-place, shown only when clarifying from inbox), **Actionable** (`q` quick, `s` next-action, `d` delegate, `c` calendar, `h` habit), **Project** (`p` new, `a` add-to-existing), **Non-actionable** (`i` tickler, `y` someday, `k` knowledge, `t` trash).
- Each key → a thin autoloaded command `org-gtd-<name>` that calls `(org-gtd--dispatch 'type)`. The router `org-gtd--dispatch` handles clarify-session vs. standalone.
- ⚠️ **The transient layout is hand-authored, NOT generated from the type registry.** Adding a disposition means editing **three places kept in sync by hand**: the type entry, the transient layout, and the `org-gtd-clarify-organize-help-content` help text. If your feature adds a disposition, budget for that friction — and note it: this hand-sync is a prime **type-UX improvement** target (e.g. generate the transient from the registry).

### Engage + agenda
- `org-gtd-engage` **[R]** renders the daily 4-block view (schedule → ticklers-due → delegated-due → all-next-actions) via the view DSL. `org-gtd-engage-tagged` prompts for a tag. `org-gtd-show-all-next` is the flat list.
- These are just `org-gtd-view-show` calls — see §3.

### Reflect
- A large family of read-only review commands: `org-gtd-reflect-{area-of-focus, someday-maybe, someday-review, stuck-*, missed-*, completed-*, upcoming-delegated}`, `org-gtd-reactivate`. Each is another `org-gtd-view-show` call. `org-gtd-reflect-someday-review` is notable: it's a **guided, one-at-a-time session engine** (defer/clarify/quit, stats) — the template for any "walk me through my X" feature.

### Command center (the hub)
- `org-gtd-command-center` transient **[R] (released 4.6.1)** is the top-level menu: **Engage** (`e`/`@`/`n`), **Reflect** (`a`/`y`/`d`/`r`/`R`), **Review System** submenus (`S` stuck, `M` missed). If your feature needs a home in the menus, this is where it hangs.

---

## 3. The view DSL (read this even if your feature isn't "a view")

> **[R] released 4.6.1.** The view DSL, its filter families, `org-gtd-view-show`, and prefix-format are all released and stable. What is *net-new [U]* is **interactive management** of views (create/edit/save/delete) — that is the flagship gap, not the DSL itself.

The DSL is org-gtd's answer to "how do we compose agenda displays with control org-agenda alone can't give us." It's central to the **view-manager flagship exemplar**, and its *prefix-format* capability is why org-ql/org-super-agenda were rejected.

### Grammar (in one example)
A view spec is an **alist**; multi-block via a `blocks` key. Verbatim from `org-gtd-engage`:
```elisp
`((name . "GTD Engage View")
  (prefix . (project area-of-focus "—"))     ; fallback CHAIN, not a format string
  (prefix-width . 12)
  (blocks . (((name . "Today's Schedule")   (block-type . calendar-day))
             ((name . "Tickler items ready today") (type . tickler) (when . today))
             ((name . "Delegated to check in on") (type . delegated) (when . today))
             ((name . "All actions ready")   (type . next-action)))))
```
Filter families you can expose as inputs: **type** (`next-action delegated calendar tickler someday project habit reference` + computed `stuck-* active-project completed-project …`), **time** (`(when . past/today/future)` or comparison `(when . (< "7d"))`, duration regex `[+-]?[0-9]+[mhdwMy]`), **structural** (`todo`, `done`, `not-done`), **metadata** (`area-of-focus`, `who`, `tags`, `priority`, `effort`, `clocked`, `last-clocked-out`), and a **`native` escape hatch** that passes a block straight to org-agenda. The authoritative field list is `org-gtd-view-lang--known-filter-keys`; per-type smart defaults are `org-gtd-view-lang--type-defaults`.

### Prefix-format = the reason the DSL exists
`(prefix . (project area-of-focus "—"))` resolves **per item, structurally**: try the parent project's headline (cookies stripped), else the area-of-focus (CATEGORY, falling back through project membership), else a literal. This computed, per-item resolution is exactly what a static org-agenda match string / org-ql query **cannot** express — hence the bespoke DSL. Respect this: any view feature must preserve prefix control.

### How views live today (the gap your flagship dramatizes)
- **There is no view registry, no persistence, no interactive create/edit/delete.** `org-gtd-view-show` is the sole runtime driver: hand it an alist, it builds `org-agenda-custom-commands` on the fly and renders — then throws it away.
- A view exists only as an **elisp literal inside a `defun`** or a one-off call. The single user-facing extension is the `org-gtd-reflect-missed-custom-views` defcustom (you hand-write alist literals into it).
- **Creating a view today = edit `init.el` and re-eval.** So *any* "manage my X interactively" feature is a **net-new UI pattern** for org-gtd — build it as a transient over `known-filter-keys`, using `org-gtd-view-show` for **live preview**, and introduce a persisted `defcustom` store (name → spec) with CRUD. Nothing in the architecture blocks this; it simply doesn't exist yet.

---

## 4. Extension model — what exists, and how freely you may reshape it

Accuracy matters most here, because much of this is **in-flight**. Tags: **[R]** released in 4.6.1 · **[U]** unreleased (post-4.6.1 HEAD) · **[X]** experimental/removed. All of it is buildable-on for v5; the tag tells you the *rework bar*, not whether you may use it.

- **The type registry.** `org-gtd-types` is a static alist of the fixed GTD categories. **[R]** `org-gtd-user-types` merges user additions at lookup — the released extension seam. **[U]** `org-gtd-customize-type` (edit a type live) and the richer per-type slots `:organize-fn` / `:organize-project-fn` (replace organize behavior) and per-type `:hooks` exist only on HEAD. **[X]** `org-gtd-define-type` (a registration entry point for a genuinely new top-level type) was a **post-4.6.1 v5 experiment — added, then removed**; it is *not* in any v4 release and *not* on HEAD today. **You may resurrect and reshape it for v5** if a feature needs a new type — and if you do, treat "what should `define-type` feel like?" as part of your design.
  - So a new GTD category is **not forbidden** (we're building v5). But GTD orthodoxy usually treats sub-categorization as *list-work*: prefer a **[R] someday sub-list** (`org-gtd-someday-lists`) or a **[U] customized existing type** before minting a new type. When a new type is genuinely the right call, justify it on **UX / GTD-fidelity** grounds.
- **Hooks.** **[R]** the classic `org-gtd-organize-hooks` (a user list run during organize; default `org-set-tags-command`) is the *released* automation seam. **[U]** the six-stage observation system (`:before/after-clarify|organize|file`, dispatched by `org-gtd-hooks-run`, in `org-gtd-hooks.el`) is unreleased — usable for v5, not frozen. Its HEAD semantics: hooks **observe** (return value ignored, cannot gate/abort; errors logged, never fatal); to *alter* an outcome you replace an **[U]** `:organize-fn` slot. Design rule: "warn / annotate / log" → a hook; "alter the outcome" → a slot. Say which released or unreleased seam you're leaning on — and if the model is awkward for your feature, that's a type-UX opportunity to note.
- **Property descriptors.** A type captures fields via `:properties` descriptors — `:semantic-name`, `:org-property`, `:type` (text / timestamp / repeating-timestamp), `:required`, `:prompt`, `:default`, `:input-fn`. The descriptor mechanism is **[R]**; editing it live via `customize-type` is **[U]**. This is how you add a captured field (e.g. `ENERGY`, `stakeholder`) with little or no new UI.
- **Continuation chaining [R]** (`org-gtd-clarify--continuation`) + **WIP infra [R]** — the reuse path for any multi-item guided flow.

**How to use this:** design on the v5 foundation (released + unreleased), but be explicit in your doc about which surfaces you lean on and their tag — and wherever the current model (especially the [U] parts) is awkward, propose the improvement. That's the bonus mandate, not scope creep.

---

## 5. Voice & idioms (get these right or it reads as foreign)

- **Keys:** `C-c C-o` = "open the disposition menu / file this"; `C-c C-k` = abort/cancel (universal); `C-u` prefix = "update in place / skip refile." Uppercase = "exact/forceful" variant of a lowercase command (`C-c d` vs `C-c D`).
- **Transients:** named `org-gtd-<verb>`, built with `transient-define-prefix`, single-letter **mnemonic** keys grouped by GTD semantics, each row = letter + plain-English label. Mnemonics in play: `s`=next-action `d`=delegate `c`=calendar `p`=project `i`=tickler `y`=someday `k`=knowledge `t`=trash `q`=quick `h`=habit `a`=add-to-project.
- **No default keybindings** anywhere except `org-gtd-clarify-mode-map`. Users bind commands/transients themselves. **Never assume a global keymap** — your feature is reached by `M-x`, a transient entry, or a menu, not a key you invent.
- **Errors teach, they don't crash:** fail-soft (log via `message`), guidance headers, header-line hints, help buffers. Match this tone in your §6 failure modes.
- **Window discipline:** flows snapshot and **restore** window configuration around themselves (clarify does this). If your feature takes over the frame, plan to restore.

---

## 6. Design checklist (answer these in your doc)

Before writing §3 (walkthrough) and §5 (fit), make sure you can answer:

1. **Which surface does it extend?** (capture / process-loop / clarify+WIP / organize-transient / view DSL / engage-agenda / reflect / command-center / type-registry / hooks) — name it, cite the anchor.
2. **What's the release tag of each surface you lean on?** [R] released 4.6.1 vs [U] unreleased HEAD. Building on [U] is fine for v5; reworking an [R] surface needs a **UX- or GTD-fidelity** justification.
3. **Does it want a new type?** We're building v5, so that's allowed — but prefer a someday **sub-list** or a **customized type** unless a new type genuinely improves UX/GTD-fidelity. If you do want one, sketch what the (currently [X]) `define-type` should look like — you're helping design it.
4. **Observe or alter?** "warn / annotate / log" rides a hook; "alter the outcome" needs an `:organize-fn` slot [U]. Say which, and whether a released seam suffices.
5. **Type / extension-UX opportunity?** Does designing this reveal a way to refine or clarify the type system / extension model? Capture it — explicitly in-scope for v5.
6. **How is it reached and discovered?** (no default keys — so: `M-x` name, a transient entry in `org-gtd-command-center`, or an in-flow prompt.)
7. **What's the full lifecycle?** create → preview → edit → save/recall → delete. If any verb is "edit init.el and re-eval," that's the gap your feature closes — design the friendly replacement.
8. **Does it touch the organize transient?** Note the three-places-in-sync friction (itself a prime type-UX improvement target: generate the transient from the registry).
9. **Where does it restore the user to** when done or aborted?

---

*Sources: distilled 2026-07-04 from `org-gtd-organize.el`, `-organize-core.el`, `-clarify.el`, `-wip.el`, `-process.el`, `-capture.el`, `-types.el`, `-hooks.el`, `-view-language.el`, `-agenda.el`, `-engage.el`, `-command-center.el`, `-reflect.el`, and `doc/diataxis/{reference,explanation}/`. Anchors are current as of this date; verify a named function still exists before relying on it in an implementation plan. Release tags ([R]/[U]/[X]) were verified against tag 4.6.1 vs HEAD on 2026-07-04 (HEAD ≈ 76 commits ahead of 4.6.1) — re-verify before an implementation plan, since the [U] surface moves.*
