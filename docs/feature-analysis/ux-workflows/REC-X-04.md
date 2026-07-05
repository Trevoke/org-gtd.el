# UX Workflow — Optional ENERGY attribute

`REC-X-04` · "Match the next action to how much gas I have left in the tank." · cluster: `F — optional classification metadata`

> The **complete** UX for an optional per-action energy level in org-gtd v5: how a user turns it on, tags an item with it, filters by it in engage, and turns it back off — all through existing property/DSL idioms, no bespoke buffer.

---

## 1. The need (what & why)

- GTD's third limiting factor when choosing an action is **energy available** (`engage.feature` §"Limiting factor 3"): the doctrine says keep "a batch of simple little tasks so that when energy is depleted the user can still be good for something." org-gtd today ships **context** (tags), **time** (effort/IMPL-090), and **priority** (IMPL-068) — but **zero energy references** (REC-ENG-03 confirms; energy half of ENG-05 was moved here). This closes that gap.
- Hit at **engage time**, when a fried user scanning next-actions wants "show me only the low-energy stuff." Attached earlier, at **organize/clarify**.
- Source: `REC-X-04` (Not-implemented, `tool/may`) → **V-07: optional energy tag/property, near-term, off by default.**

## 2. Entry points & discovery

- **Invoke (opt-in):** `M-x customize-variable RET org-gtd-energy-levels` — a defcustom, **default `nil` (feature off)**. Set it to e.g. `'("High" "Medium" "Low")` to enable everything below. Nothing appears in any prompt until this is non-nil.
- **Invoke (attach):** inside the clarify WIP buffer, `C-c e` → `org-gtd-set-energy` (completing-read over the levels). Also offered as an **optional, skippable** property prompt after any actionable disposition when the feature is on.
- **Invoke (consume):** `M-x org-gtd-engage-by-energy` (prompts a level, renders the engage next-actions block filtered) and the DSL filter key `(energy . "Low")` for anyone writing view specs / `NEW-VIEW-MANAGER` views.
- **Discover:** the `C-c e` binding rides the clarify-mode header-line hint (the one place org-gtd owns a keymap) — *only when the feature is on*; and an `n`-adjacent entry in the `org-gtd-command-center` Engage group ("energy → filter next actions"). Off by default means an untouched install never sees it — correct for a `may`.

## 3. Full-lifecycle walkthrough

**Primary path (feature enabled):**
1. User clarifies an inbox item, presses `C-c C-o`, picks `s` (single next action). Item files as normal.
2. Because `org-gtd-energy-levels` is non-nil, a **final optional prompt** appears: `Energy (RET to skip): [High|Medium|Low]`. User types `L`+RET → `ENERGY: Low` property stamped on the heading. Pressing RET alone skips — no property written, item is untagged (the common case; energy is sparse by design).
3. Later, tired, the user runs `M-x org-gtd-engage-by-energy`, picks `Low`. Engage renders its usual 4-block view but the **all-next-actions** block is filtered to `ENERGY=Low` items, same prefix-format (`project — area`) as vanilla engage.

- **Create / start** — enable via `org-gtd-energy-levels`; attach via the skippable prompt or `C-c e` in WIP.
- **See / preview** — the ENERGY property is visible in the WIP buffer's property drawer; in agenda output it can be surfaced through the existing `effort`-style column. `org-gtd-engage-by-energy` is the live consume view.
- **Edit / reconfigure** — re-run `C-c e` on the item (in WIP or via `org-gtd-clarify-item` with `C-u` update-in-place) to change the level; pick the blank entry to clear it. Levels themselves are re-editable in customize.
- **Save / name / recall** — persistence is **org-native**: the value lives in the heading's `ENERGY` property, no side store. "Recall" = filter for it any time via engage/DSL.
- **Delete / undo / back out** — set energy to empty to drop the property from one item; set `org-gtd-energy-levels` back to `nil` to retire the whole feature (existing `ENERGY` props stay as inert org properties, harmless).
- **Repeat / recur** — n/a for the attribute itself; a habit/repeater carries its `ENERGY` prop across recurrences for free (it's just a property).

## 4. Interaction sketch

**Attach — the optional tail prompt after disposition (feature on):**
```
┌─ minibuffer ───────────────────────────────────────────────┐
│ Energy (RET to skip): Low                                    │
│  High    Medium    Low                                       │
└─────────────────────────────────────────────────────────────┘
        ↑ completing-read over org-gtd-energy-levels; empty ⇒ no property
```

**Consume — engage filtered by energy:**
```
M-x org-gtd-engage-by-energy RET  →  Energy: Low RET

GTD Engage — Energy: Low
  Today's Schedule        (unchanged)
  Tickler / Delegated     (unchanged)
  All actions ready ▾ ENERGY=Low
    proj-taxes  — Finance    Shred last year's receipts
    (errands)   — Home       Refill stapler
```

**Keymap**

| Key / command | Context | Action |
|---|---|---|
| `C-c e` (`org-gtd-set-energy`) | clarify WIP buffer | set/change/clear ENERGY on the item |
| `RET` at energy prompt | post-disposition | skip — leave item untagged |
| `M-x org-gtd-engage-by-energy` | anywhere | prompt a level, render filtered engage |
| `(energy . "Low")` | view DSL spec | filter block to that level |
| command-center `n`-group entry | command center | reach `org-gtd-engage-by-energy` |

**Live preview:** none bespoke — the "preview" is `org-gtd-engage-by-energy` itself (reuses `org-gtd-view-show`, the primer's live-render path). Re-invoking with a different level re-renders; no separate preview pane needed for a single-field filter.

## 5. Fit with org-gtd

- **Extends** — (1) the **type property-descriptor** mechanism `[R]` (`:properties` with `:org-property`/`:prompt`/`:input-fn`, IMPL-034/`org-gtd-types.el`) to define `ENERGY`; (2) the **view DSL** `[R]` — adds `energy` to `org-gtd-view-lang--known-filter-keys` with a predicate modeled exactly on `org-gtd-pred--effort-matches` (IMPL-090); (3) **engage** `[R]` — `org-gtd-engage-by-energy` mirrors `org-gtd-engage-tagged`; (4) clarify-mode keymap `[R]` for `C-c e`.
- **Shared surface / cluster (F):** the shared contract is **"optional classification attached at organize, consumed via existing surfaces, off by default, no bespoke buffer."** My sibling `REC-NXT-05` attaches via the **someday-list-prompt** idiom; I attach via the **property-prompt** idiom. Where we must feel **identical**: (a) the *opt-in gate* (one defcustom, default empty = invisible), (b) the *attach moment* is a single **skippable** minibuffer prompt riding the disposition flow — never a new buffer, never a required field, and (c) the *consume moment* is a **DSL filter key + one engage/reflect entry point**. If NXT-05's skip affordance is `RET`, mine is `RET`; if its entry command is `org-gtd-<x>`, mine matches the naming. The cluster's promise: attaching optional metadata reads the same regardless of which attribute.
- **Reuse vs. new** — reused: property descriptor, DSL predicate shape, engage renderer, clarify header-line. Genuinely new: the `energy` filter key + predicate, `org-gtd-set-energy`, `org-gtd-engage-by-energy`, the `org-gtd-energy-levels` defcustom.
- **Release tag** — everything leaned on is `[R]` (4.6.1). **No `[R]` rework** and, deliberately, **no organize-transient change** — energy is not a disposition, so it dodges the three-places-in-sync friction entirely. Confirms the build-route agreement noted in `_CLUSTERS.md` §7.

### Type / extension-UX opportunities

1. **The generic `property` filter key is reserved but unimplemented.** `known-filter-keys` lists `property`, yet there is no `org-gtd-pred--property-*` wiring for an arbitrary `(property . ("ENERGY" . "Low"))`. Rather than hardcode `energy`, v5 should **implement the generic property filter** and make `energy` sugar over it. That single improvement lets *any* user property (energy, stakeholder, location) be DSL-filterable — a real extension-model win, and it makes REC-X-04 a ~10-line feature.
2. **Optional/off-by-default property descriptors have no expression today.** Every `:properties` entry is unconditionally prompted. Energy wants a descriptor that is **present only when a defcustom enables it** and **`:required nil` + skippable**. Proposing a `:when-enabled` / `:optional` descriptor slot (or gating a descriptor on a predicate) is a reusable type-UX primitive the whole "optional metadata" cluster F needs — worth formalizing in v5.

## 6. Edge cases & failure modes

- **Empty state / feature off:** `org-gtd-energy-levels` nil ⇒ no prompt, no key, no command visible. `org-gtd-engage-by-energy` invoked anyway ⇒ teaching message: `"Energy levels are off. Set org-gtd-energy-levels to enable."` (no crash).
- **Bad input:** completing-read is `REQUIRE-MATCH` over the configured levels, so an item can't hold a level the user later removed from the list; filtering for an orphaned old value simply returns no items (not an error). Empty string ⇒ property removed, not stored as `""`.
- **Large data:** energy is a plain property predicate — same cost as the effort filter; no new indexing.
- **When it goes wrong:** stays in org-gtd's fail-soft voice — messages, not stack traces; window config restored by the clarify/engage flows it rides.

## 7. Open questions & maintainer decisions

- **Levels: strings vs. GTD's canonical trio.** Ship a default of `'("High" "Medium" "Low")` (still gated behind opt-in), or leave strictly `nil` and force the user to author their own? (Leaning: default trio, feature still off until set — but the gate is "non-nil," so a default value *is* "on." Needs a `boolean + list` split or a sentinel.)
- **ENERGY as a tag vs. property.** Build route names "property / tag." Property chosen (structured, DSL-native, invisible in headline). Confirm we don't also want a `:energy_low:` tag for users who filter by org tags directly.
- **Generic `property` filter** (opportunity #1): implement now as the backing, or hardcode `energy` and defer the generic key? Affects sibling reuse.

## 8. Provenance & links

`REC-X-04` · deliverable-#3 **Not-implemented** (zero energy refs, re-verified) · build route: `gap-implementation-strategies.md` §7 schema-addition + "Org-native" §193 (ENERGY property, DSL-filterable) + IMPL-034 type registry · related: `engage.feature` "Limiting factor 3 — energy available", REC-ENG-03/05 (energy half moved here) · sibling: `REC-NXT-05` (cluster F, someday-list-prompt idiom) · maps to roadmap **V-07**.
