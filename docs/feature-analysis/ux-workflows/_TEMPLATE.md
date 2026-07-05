# UX Workflow — «Feature name»

`«REC-id | NEW-id»` · «one-line: the user need» · cluster: `«surface-cluster»`

> **What this doc is.** The *complete* user experience for this feature in org-gtd — what the user does, sees, and presses across the feature's whole lifecycle. Not the mechanism that backs it. "They have a DSL for views" is not a UX; "they can create, preview, edit, save, reload, and delete a view without writing elisp" is. Design for someone whose GTD habits were built on **folders and paper**: now that it's digital, how do we make each action at least as friendly as the physical act it replaces?

> **How to fill this in.** Every section below is required. If one genuinely doesn't apply, keep the heading and write a single line saying why. Be concrete: real command names, real keystrokes, real buffer contents. **Show, don't tell** — a mock beats a paragraph. Match org-gtd's existing idioms (see `_PRIMER.md`) so the feature feels native, not bolted on. Keep the whole doc tight (aim ~1–2 pages); depth goes into the walkthrough and the sketch, not into prose.

---

## 1. The need (what & why)

- In GTD terms, what problem does this solve, and for whom? (1–3 sentences.)
- When in the user's practice do they hit it?
- Source: `«REC-id»` (deliverable #3 entry) · book citation · or net-new rationale.

## 2. Entry points & discovery

- **Invoke** — every way the user *starts* this: command name(s), transient entry + key, agenda/engage action, capture path, keybinding.
- **Discover** — how a user learns it exists *without* reading the manual: a menu affordance, a prompt inside a flow they already do, a visible surface. A feature nobody can find has no UX.

## 3. Full-lifecycle walkthrough

The heart of the doc. Walk the *entire* experience step by step, keystroke-by-keystroke where it matters. Write the **primary path** first as a numbered sequence, then note variations. Cover each verb that applies; drop a verb only with a one-line justification.

- **Create / start** — …
- **See / preview** — what's on screen; does the user get *live* feedback as they work?
- **Edit / reconfigure** — how they change it after the fact.
- **Save / name / recall** — does it persist? how do they get back to it later?
- **Delete / undo / back out** — how they abandon or remove it, safely.
- **Repeat / recur** (if applicable) — …

## 4. Interaction sketch

- **Mock** — ASCII of the transient menu / buffer / prompt sequence, showing the keys.
- **Keymap** — table of `key → action`.
- **Live preview** (if any, à la the DSL-view manager) — show a before/after of what updates on screen as the user changes an input.

## 5. Fit with org-gtd

- **Extends** — which existing surface(s): organize transient · clarify + WIP buffer · process loop · view DSL · engage + agenda · capture · type registry · hooks. Cite `IMPL-*` and command names.
- **Shared surface / cluster** — which *other* features should share this UX (name them), and where this must feel **identical** to an existing flow (consistency constraints). NB: `gap-implementation-strategies.md` §3/§10 clusters are *build-route hypotheses* — confirm or break them here from a UX standpoint; do not assume they hold.
- **Reuse vs. new** — what's reused as-is, what is genuinely new UI.
- **Release tag of what you lean on** — `[R]` released 4.6.1 vs `[U]` unreleased HEAD (see `_PRIMER.md`). We build for **v5**, so `[U]` is fair game; but if you'd rework an `[R]` surface, justify it by **UX** or **GTD-fidelity** improvement.

### Type / extension-UX opportunities

Does designing this feature reveal a way to **refine, clarify, or improve** the type system or extension model — the registry, generating the organize transient from it, the hook/slot model, `define-type`, property descriptors? Capture it here. This is **explicitly in-scope for v5**, not scope creep; the maintainer wants these features to leave the type UX better than they found it. Write "none identified" if truly none.

## 6. Edge cases & failure modes

- **Empty state** — no items / views / contacts / history yet.
- **Bad input** — invalid entries, conflicts, duplicates, very large data sets.
- **When it goes wrong** — what the user *sees*. org-gtd favors teaching errors and guidance, not stack traces — stay in that voice.

## 7. Open questions & maintainer decisions

- UX forks left unresolved; anything needing the maintainer's call before build.

## 8. Provenance & links

- `REC-id` · deliverable-#3 status (Not-implemented / Partial / …) · `gap-implementation-strategies.md` §-ref (the **build route** — a hint, not a UX spec) · related `WF-*` workflows · sibling `REC-*`/`NEW-*` in the same cluster.
