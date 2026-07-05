# UX Workflow — REC-CAP-08

`REC-CAP-08` · "I just finished a call — capture the loose ends before they evaporate" · cluster: `G — activity-boundary harvest`

> The complete UX for the capture prompt that fires at the close of a *clocked* activity (a call, a work session), offering to sweep whatever came out of it into `inbox.org`.

---

## 1. The need (what & why)

- GTD's collect habit is weakest exactly at **activity boundaries**: a call ends, three commitments were made, and they live only in your head until the next thing overwrites them. The book prescribes capturing *at the moment of closure*, before attention moves on.
- The user hits this every time they **clock out** of a call/meeting/focused session (`org-clock-out`). The tool is already tracking that boundary — it just never asks the collect question.
- Source: `REC-CAP-08` (Not-implemented; `tool/may`; DA94-34, no prior disposition). Org-native backing: `org-clock-out-hook`.

## 2. Entry points & discovery

- **Invoke** — not a command the user types; it is a **prompt that fires itself** on the `org-clock-out` boundary when the closed activity qualifies (see §6 for the predicate). It can also be raised manually with `M-x org-gtd-capture-harvest` on the heading at point (for people who don't clock, or want the sweep on demand).
- **Arm** — one opt-in defcustom, `org-gtd-capture-boundary-harvest` (default `nil`), plus `org-gtd-capture-boundary-min-duration` (default `2m`) so trivial clock-outs stay silent. Setting the first to `t` wires the hook.
- **Discover** — it surfaces *inside a flow the user already does* (clocking out), so it teaches itself the first time it fires. The command-center footer notes "Boundary harvest: off — `M-x customize-variable org-gtd-capture-boundary-harvest`." No default key (org-gtd binds none outside clarify-mode).

## 3. Full-lifecycle walkthrough

**Primary path — harvesting after a clocked call:**

1. User runs a call under a clocked heading `* Call with Sam`, then `org-clock-out` (however they normally do it).
2. The boundary fires. Because the clock ran ≥ `min-duration`, org-gtd raises the shared **harvest transient** (§4) in a small popup at the bottom of the frame — the clock-out itself has already completed underneath it.
3. User presses `c`. An `org-gtd-capture` inbox capture opens ("*Send Sam the Q3 numbers*"), `C-c C-c` finalizes it to `inbox.org`. Control **returns to the transient** (loop), mode-line inbox count ticks `+1`.
4. User presses `c` again → "*Book the follow-up*" → finalize. `+1`.
5. Nothing more came of it → `s` (skip / done). Transient closes, window config is restored, user is exactly where they clocked out.

- **Create / start** — armed once via the defcustom; each harvest instance is *born from the boundary*, not created by hand.
- **See / preview** — the transient shows **what closed and for how long** ("Call with Sam · clocked 23m"), and the **live inbox count climbs** as items are captured (reusing the mode-line inbox-count machinery, IMPL-132).
- **Edit / reconfigure** — captured items are ordinary inbox headings; they are edited/refined later in the normal **process-inbox → clarify** loop. The *feature's* behavior is reconfigured through the two defcustoms (which activities trigger, the duration floor).
- **Save / name / recall** — persistence *is* the inbox. Harvested items sit in `inbox.org` and are **recalled** by the next `org-gtd-process-inbox`. No separate store.
- **Delete / undo / back out** — `s` (nothing came of it) or `C-c C-k` dismisses with zero writes. `!` says "never ask for *this* heading again" (stamps a property so recurring meetings you never harvest stop nagging). Disarm globally by setting the defcustom back to `nil`.
- **Repeat / recur** — fires automatically on every qualifying clock-out; the `!` per-heading opt-out and the duration floor keep it from becoming noise.

## 4. Interaction sketch

**Mock — the harvest transient (shared Cluster-G surface):**

```
┌─ Harvest ─────────────────────────────────────────────┐
│ Call with Sam · clocked 23m — did anything come of it? │
│                                                        │
│  Capture                                               │
│   c   Capture an item → inbox        (captured: 2)     │
│   RET Capture, then ask again                          │
│                                                        │
│  Close                                                 │
│   s   Nothing came of it / done                        │
│   !   Never ask for this heading                       │
│   C-c C-k  Dismiss                                     │
└────────────────────────────────────────────────────────┘
        │  press c
        ▼
[org-gtd-capture: inbox]  * Send Sam the Q3 numbers
                          :ORG_GTD_CAPTURED_AT: [2026-07-04 ...]
        C-c C-c  → lands in inbox.org, returns to Harvest (captured: 3)
```

**Keymap:**

| key       | action                                              |
|-----------|-----------------------------------------------------|
| `c` / `RET` | open inbox capture; on finalize, loop back here    |
| `s`       | close — nothing (or nothing more) came of it        |
| `!`       | stamp this heading so it never prompts again         |
| `C-c C-k` | dismiss (universal abort)                            |

**Live preview** — the `(captured: N)` counter and the global mode-line inbox count update on each finalize, so the user *sees* the harvest accumulating before deciding they're done.

## 5. Fit with org-gtd

- **Extends** — the **capture pipeline**: `org-gtd-capture` / the `org-gtd-capture-templates` defcustom, finalizing to `org-gtd-inbox-path` with the `ORG_GTD_CAPTURED_AT` stamp. Trigger is org-native `org-clock-out-hook`. Reuses the **transient idiom** (`transient-define-prefix`) and the mode-line inbox-count refresh (IMPL-132).
- **Shared surface / cluster** — Cluster **G** with `REC-AGE-03`. The **harvest transient in §4 is the shared surface**: same "did anything come of it?" framing, same `c/s/!` keys, same inbox landing. Only the *trigger* differs — CAP-08 fires on a clock boundary, AGE-03 on a meeting object reaching "occurred," but both call the identical `org-gtd-capture-harvest` prefix. This confirms the build-route divergence note (a §9 hook and a §8 object sharing one moment): from the UX seat they must be **indistinguishable** past the trigger.
- **Reuse vs. new** — reused as-is: capture templates, inbox filing, the `ORG_GTD_CAPTURED_AT` finalizer, the mode-line counter, the transient look. Genuinely new: the harvest transient itself and the clock-out arming logic (both shared with AGE-03).
- **Observe or alter?** — pure **observe/prompt**: it neither gates nor rewrites the clock-out (which completes normally underneath). This is the "warn/annotate/prompt" side of the hook rule, so it rides a hook rather than an `:organize-fn` slot.
- **Release tag** — capture pipeline, templates, inbox finalizer, transient idiom, mode-line count: **[R]** 4.6.1. `org-clock-out-hook`: org-native. The six-stage observation hooks (IMPL-136): **[U]** — but see below, they don't actually fit. No **[R]** surface is reworked.

### Type / extension-UX opportunities

The **[U]** six-stage hook system (IMPL-136) is scoped to *organize* stages only — there is **no seam for lifecycle/activity boundaries** (clock-out, meeting-occurred), so CAP-08 and AGE-03 would each wire raw org hooks independently and duplicate the "raise the harvest transient" glue. **Opportunity:** introduce a small **`org-gtd-harvest-triggers` registry** — a first-class list of boundary events (`clock-out`, `meeting-occurred`, `manual`) that map to the one harvest prefix. Both siblings *register* against it instead of hand-wiring hooks; users can add their own boundary (e.g. "on TODO→DONE of a `:call:` item") declaratively. That generalizes org-gtd's hook vocabulary beyond the organize pipeline and is the cleaner extension model this feature exposes.

## 6. Edge cases & failure modes

- **Empty state** — user presses `s` immediately (nothing came of it): transient closes, no writes, no inbox churn. This is the common case and must be one keystroke.
- **Bad input / noise** — every tiny clock-out would nag; the `min-duration` floor and per-heading `!` opt-out are the guardrails. A recurring standup you never harvest is silenced after one `!`.
- **Nested / interrupted clocks** — clocking into a *new* task auto-clocks-out the old one; the harvest prompt must fire for the **closed** task without blocking the **new** clock-in. If a capture is aborted mid-way (`C-c C-k` inside `org-capture`), the harvest transient stays open (nothing lost); dismissing the transient never rolls back already-finalized items.
- **When it goes wrong** — the hook is fail-soft: any error raising the transient is logged via `message` ("org-gtd: could not open harvest prompt"), the clock-out is never disturbed. Voice stays teaching, not stack-trace.

## 7. Open questions & maintainer decisions

- **Transient vs. minibuffer y/n at the boundary.** A transient gives the shared keymap and the live counter, but pops a window at an unexpected moment. Fallback: degrade to a `y-or-n-p` when a defcustom prefers minimal interruption. Maintainer call on the default.
- **Duration floor default** — `2m` proposed (mirrors the 2-minute motif); confirm.
- **Should `!` (never-ask) live as a property on the heading or in a defcustom exclusion list?** Property travels with recurring templates; list is easier to audit. Cluster-G-wide decision (AGE-03 needs the same).
- **Multi-item capture ergonomics** — loop-on-`c` vs. a single multi-line capture that splits into headings. Loop is simpler and reuses one template.

## 8. Provenance & links

- `REC-CAP-08` · Not-implemented · `gap-implementation-strategies.md`: org-native backing `org-clock-out-hook` (§ "Org-native"), hook-surface seam IMPL-136 (§9), dispatch table row "host on IMPL-136 stages; org-native `org-clock-out-hook`."
- Cluster **G** shared contract (`_CLUSTERS.md`): sibling **`REC-AGE-03`** (meeting lifecycle object) — same harvest moment, same inbox landing, different trigger.
- Related surfaces: `org-gtd-capture` / `org-gtd-capture-templates` / `org-gtd-inbox-path` (capture pipeline), mode-line inbox count (IMPL-132), process-inbox recall loop.
- No matching `WF-*` gherkin (no clock-boundary scenario exists in `workflows/`; net-new moment).
