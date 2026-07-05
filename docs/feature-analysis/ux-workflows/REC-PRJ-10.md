# UX Workflow — Verb-starter checklist for naming projects

`REC-PRJ-10` · *"When I make a project I blank on how to phrase it — give me outcome-verb starters so the name states a finish line."* · cluster: `D — scaffolded project creation`

> The complete experience of being *helped to name a project by its outcome* at the moment it's created. Not the checklist storage engine (that's REC-CHK-01) — the felt experience of turning "Website" into "Roll out the redesigned marketing website."

---

## 1. The need (what & why)

- GTD insists a **project name states the *successful outcome*, not the topic** — the wording should let you know you're done. Beginners (and everyone under load) name projects as nouns ("Taxes", "Basement") that never signal completion. Allen's remedy is a **verb starter list** (Finalize, Resolve, Roll out, Implement, Research, Design, Organize, Update, Maximize, Install, Ensure, Submit, Handle, Look into…) you scan to phrase the outcome.
- **When they hit it:** at the exact instant they choose the `p` (new project) disposition while clarifying — the one moment org-gtd asks "what is this project?" and stamps a heading.
- Source: `REC-PRJ-10` (Not-implemented; `tool/should`; falls out of V-10). Book: *GDT* project-naming / "outcome verbs" list.

## 2. Entry points & discovery

- **Invoke** — no new command in the common path. Pressing **`p`** in the organize transient ([R] `org-gtd-organize` → new project) triggers a **naming step** *before* the project template is stamped. It offers the verbs from a bundled checklist as completion candidates. Re-run on demand via **`M-x org-gtd-project-name-helper`** (also proposed on `org-gtd-clarify-mode-map` as **`C-c C-n`**, [U]).
- **Discover** — surfaced *inside a flow the user already does*: the naming prompt appears automatically the first time you organize-as-project, with a header-line teach line ("Name the outcome — press `TAB` for verb starters, `RET` to keep as-is"). No manual reading required. The verb list itself is browsable/editable as a named list in the CHK-01 manager (Cluster E), where it shows as "Project verbs."

## 3. Full-lifecycle walkthrough

Naming is **per-project** (transient act on one heading); the *verb list* it draws from is a persisted named object (owned by CHK-01/E). Lifecycle below covers both layers.

- **Create / start** — In the WIP buffer with heading `Website`, press `C-c C-o` → `p`. Before the project skeleton is inserted, org-gtd prompts:
  `Project outcome (verb first): ` with the "Project verbs" list as `completing-read` candidates. User types `roll` → completion narrows to `Roll out`; `RET` inserts it, then continues free-typing → `Roll out the redesigned marketing website`. `RET` sets the heading; the project template stamps as normal.
- **See / preview** — The WIP buffer heading updates **live** to the chosen name the moment you accept the prompt; the standard project skeleton (sequential next-actions) then appears under it. What you see is the real org heading — no separate preview pane needed.
- **Edit / reconfigure** — The name is just heading text: edit it inline in the WIP buffer anytime, or re-invoke the helper with `C-c C-n` to re-scan verbs (it pre-fills the current heading so you augment, not clobber). Managing *which verbs are offered* happens in the CHK-01 manager (add "Publish", remove "Maximize") — never here.
- **Save / name / recall** — The project name persists as the org heading (filed on organize). The verb list persists in the `org-gtd-checklists` store as a bundled default (recallable/editable via the Cluster-E manager). No per-project naming state to recall.
- **Delete / undo / back out** — Empty input or `C-g` at the prompt = **keep the heading verbatim** and proceed; the helper is a pure opt-in scaffold that never blocks project creation. Standard undo reverts a heading edit. Disable the auto-prompt entirely via `org-gtd-project-name-helper` = `nil`.
- **Repeat / recur** — Fires once per new project; idempotent — re-running just re-offers verbs against the current name.

## 4. Interaction sketch

**Mock — from the organize transient through the naming prompt:**

```
WIP buffer (org-gtd-clarify-mode)  [U]
┌──────────────────────────────────────────────────────────┐
│ Clarify item.  C-c C-o file · C-c C-n name · C-c C-k quit │  ← header-line
│                                                            │
│ * Website                                                  │
└──────────────────────────────────────────────────────────┘
        │  C-c C-o
        ▼
 Organize  [R]
   Project
    [p] new project        [a] add to existing
        │  p
        ▼
 Minibuffer  [consumes CHK-01 "Project verbs" list]
 ┌───────────────────────────────────────────────────────┐
 │ Project outcome (verb first): roll▮                    │
 │  Roll out   Resolve   Research   Reduce                │  ← completion
 └───────────────────────────────────────────────────────┘
        │  "Roll out the redesigned marketing website" RET
        ▼
 * Roll out the redesigned marketing website     ← heading updates live
 ** NEXT ...        ← standard project skeleton stamps as usual
```

**Keymap**

| Key / input | Action |
|---|---|
| `p` (in organize transient) | New project → fire naming helper first |
| `TAB` | Complete against the verb-starter list |
| `RET` (with text) | Accept name, stamp project |
| `RET` empty / `C-g` | Keep heading as-is, proceed (opt-out) |
| `C-c C-n` (clarify-mode) [U] | Re-run naming helper on current heading |
| `M-x org-gtd-project-name-helper` | Same, standalone |

**Live preview** — the heading rewrite *is* the preview: accept the prompt and the `*` heading in the WIP buffer changes under point before the skeleton is added.

## 5. Fit with org-gtd

- **Extends** — the **organize-as-project disposition** (`p` → project template, IMPL-040) and the **clarify WIP buffer** (`org-gtd-clarify-mode`). The naming step slots into the project-creation path just before the skeleton stamp. Verb candidates are read from the **CHK-01 checklist store** (the "Project verbs" bundled list).
- **Shared surface / cluster** — **Cluster D**, sibling **REC-PRJ-06** (Natural Planning scaffold). Both intercept the *same* `p`-at-creation moment inside the WIP buffer and must read as **one optional project-shaping affordance**: identical entry (`p`), identical "scaffold you can ignore — `RET`/`C-g` skips" contract, identical header-line teaching idiom. PRJ-10 is the *naming* helper; PRJ-06 is the *planning* helper — they should compose (name first, then optional plan), not feel like two bolt-ons. This **breaks** the build-route grouping (which lumps PRJ-10 with checklist machinery): its mechanism is CHK-01's list type, but its **UX home is project creation**, not the checklist manager.
- **Reuse vs. new** — Reuses: `completing-read`, organize transient, WIP heading edit, CHK-01 named-list store. New: the pre-stamp naming hook in the `p` path, the optional `C-c C-n` binding, the `org-gtd-project-name-helper` defcustom/command.
- **Release tag** — Leans on [R] organize transient + [R] project template + [R] clarify WIP; the CHK-01 list store and the `C-c C-n` clarify-map addition are [U] (fine for v5). No [R] rework beyond adding one optional binding to `clarify-mode-map` (justified: naming lives on the WIP editing surface, the one place default keys are sanctioned).

### Type / extension-UX opportunities

Real leverage here: make the project type's **name field a property descriptor with a checklist-backed `:input-fn`.** Today project naming is implicit heading text; modeling it as a `:properties` descriptor (`:semantic-name outcome`, `:input-fn (checklist-completion "Project verbs")`) means **any type could declare a named checklist as the completion source for a field** — verb starters for projects, channel list for delegate (Cluster C DEL-05), energy levels for X-04. That turns "a checklist" and "a field's allowed values / prompts" into the *same* mechanism, and gives `define-type`/`customize-type` a clean, uniform way to attach guided input. Capture this: descriptors should be able to reference a named checklist by name.

## 6. Edge cases & failure modes

- **Empty state** — verb list missing/emptied in the manager → the prompt degrades to a plain free-text "Project outcome:" with no candidates; project creation never blocks.
- **Bad input** — user ignores verbs and types a noun; org-gtd does **not** lint or reject (that consent-triage behavior is Cluster B / CLA-10's job, not this scaffold's). Duplicate/near-duplicate names are allowed. Very long verb lists just page in completion.
- **When it goes wrong** — helper errors (e.g. corrupt list) are caught and logged via `message` ("Verb helper unavailable — name freely"); the `p` disposition proceeds with a plain prompt. Fail-soft, in the teaching voice.

## 7. Open questions & maintainer decisions

- **Auto-fire vs. opt-in-per-invocation:** prompt automatically on every `p`, or only when `org-gtd-project-name-helper` is enabled (default on/off)? Recommend default-on, one `RET` to skip.
- **Compose order with PRJ-06:** name → then Natural Planning, or fold naming into PRJ-06's "vision/outcome" phase so there's a single scaffold? (Cluster-D synthesis call.)
- **Does the heading also drive an `OUTCOME`/vision property** (shared with PRJ-06's V-04 schema), or stay heading-only for v5?

## 8. Provenance & links

- `REC-PRJ-10` · Not-implemented · `gap-implementation-strategies.md` §3.2 (checklist-type reshape; build route) + §1 authored-content corpus · gated by `REC-CHK-01` (V-10) which supplies the list data-model · sibling `REC-PRJ-06` (Cluster D) · consumes Cluster E (`REC-CHK-01`/`NEW-VIEW-MANAGER`) list store · related workflows `organize.feature` WF (project extraction / naming by outcome).
