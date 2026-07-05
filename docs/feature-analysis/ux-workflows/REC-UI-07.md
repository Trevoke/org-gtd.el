# UX Workflow — Verb-first action entry

`REC-UI-07` · "I just want to type *what I'm doing* — 'Call the dentist' — and have it land in the right place without learning org-gtd's taxonomy." · cluster: `singleton (alt front-door to organize dispatch)`

---

## 1. The need (what & why)

GTD next actions are, by discipline, phrased as physical verbs ("Call…", "Draft…", "Buy…", "Read…"). The disposition an item wants is usually *encoded in that verb*: "Buy" is an errand, "Call" is a context-bound next action, "Brainstorm" implies a multi-step project. Today the user must translate their natural verb into org-gtd's abstract taxonomy (`s` next-action, `p` project, `d` delegate…) every single time. Verb-first entry lets the leading verb *drive the routing*, so a beginner who thinks in plain-language actions never has to learn the mnemonic map, and a veteran gets one-keystroke dispatch on the 80% of items whose verb is unambiguous.

- **When they hit it:** at the organize step, on every actionable item — the highest-frequency decision in the whole system.
- **Source:** `REC-UI-07` (Not-implemented) · DA94-26 (verb-menu entry with verb-driven routing: "get/buy → errands") · Book: next actions are physical, visible, verb-led.

## 2. Entry points & discovery

- **Invoke:**
  - New key `v` **"Enter by verb…"** on the `org-gtd-organize` transient [R] (Options group), sitting alongside the mnemonic dispositions as a peer front-door.
  - Standalone autoloaded command `M-x org-gtd-organize-by-verb`, usable inside the clarify WIP buffer or on any heading (mirrors how `org-gtd-organize` is both a transient and a callable).
- **Discover:** the `v` row is visible every time the organize transient opens — the user sees "Enter by verb…" next to the taxonomy and tries it. On first parse of a heading whose leading word is a known verb, the transient header-line hints: *"Leading verb 'Call' detected — press `v` to route by verb."* No new keybinding is assumed (org-gtd binds nothing globally; `v` lives only inside the transient).

## 3. Full-lifecycle walkthrough

The persistent object here is the **verb route** (a verb → disposition[+context] mapping). The action item is transient; the routes are what you create, recall, and delete.

**Primary path (the natural case — verb already typed):**
1. Clarify pulls "Call the dentist about the crown" into the WIP buffer [R]; user presses `C-c C-o` → organize transient.
2. org-gtd has already parsed the leading verb **"Call"** and pre-highlighted the `v` row: *"v  Enter by verb… (detected: Call → Next action @calls)"*.
3. User presses `v`. A one-line confirm prompt appears with the resolved route filled in (see mock). It is **not** a blank prompt — it echoes what the verb resolved to.
4. `RET` accepts → org-gtd dispatches the *same* command the taxonomy key would (`org-gtd-next-action` via `org-gtd--dispatch` [R]), stamps the `@calls` context tag, files the item, restores the window config, and the process-loop continuation [R] pulls the next inbox item. Zero taxonomy learned.

**Create / start (verb not yet in the heading):** `M-x org-gtd-organize-by-verb` on a bare heading opens `completing-read` over the known verb set (Call, Email, Buy, Draft, Read, Brainstorm, Ask, File…). Picking "Buy" routes to Next action + `@errands` and optionally prepends "Buy " to the headline if absent.

**See / preview:** as the user types/cycles the verb in the prompt, the tail of the minibuffer live-updates the resolved target: `Buy → Next action @errands`, `Brainstorm → Project (multi-step)`, `Ask Sam to → Delegate`. The disposition is previewed *before* commit — the user never files blind.

**Edit / reconfigure (this item):** `TAB` from the confirm prompt drops through to the full organize transient with the item intact, so any verb decision is overridable to the canonical taxonomy in one key. Re-typing the verb re-routes live.

**Save / name / recall (the route):** an **unknown verb** (say "Photograph") triggers a teaching prompt: *"No route for 'Photograph'. Route it once, or remember it?"* — pick a disposition, then `y` remembers it by appending to the `org-gtd-verb-routes` defcustom (verb → `(disposition . context-tag)`). Next time, "Photograph" resolves automatically. Recall is implicit: saved routes rank first in completion, MRU-ordered.

**Delete / undo / back out:** `C-c C-k` at any prompt aborts back to the organize transient (nothing filed). To remove a route, `M-x org-gtd-verb-routes-edit` (thin `customize`-backed list editor) or edit the defcustom; a route removed simply reverts that verb to the "unknown → teaching prompt" path — nothing breaks.

**Repeat / recur:** not applicable to a single dispatch; the *routes* recur implicitly by persisting.

## 4. Interaction sketch

```
 Clarify WIP buffer ─ header-line:
 ┌──────────────────────────────────────────────────────────────┐
 │ Clarify item. C-c C-o to organize · leading verb "Call" ↦ v   │
 │                                                              │
 │  * Call the dentist about the crown                          │
 └──────────────────────────────────────────────────────────────┘
        │  C-c C-o
        ▼
 ┌── Organize ─────────────────────────────────────────────────┐
 │ [ v  Enter by verb…   (detected: Call → Next action @calls) ]│  ← new row [R-reworked]
 │  Actionable:  q Quick   s Next action                        │
 │               d Delegate  c Calendar  h Habit                │
 │  Project:     p New   a Add to existing                      │
 │  Non-action:  i Tickler  y Someday  k Knowledge  t Trash     │
 └─────────────────────────────────────────────────────────────┘
        │  v
        ▼
   Route by verb:  Call ▸  Next action  @calls
   [RET accept · e edit verb · TAB full menu · C-c C-k cancel]
        │  RET
        ▼
   → org-gtd-next-action  (tags: :@calls:)  → filed → next inbox item
```

Unknown-verb branch:
```
 Route by verb: Photograph
   ⚠ No route for “Photograph”.
   [d disposition once · y remember this verb · C-c C-k cancel]
```

**Keymap (within the verb prompt / transient):**

| key | action |
|-----|--------|
| `v` | (in organize transient) open verb-first entry, pre-filled with detected verb |
| `RET` | accept resolved route → dispatch that disposition, apply context tag, file |
| `e` | edit / retype the verb (re-routes live) |
| `TAB` | fall through to the full `org-gtd-organize` transient, item intact |
| `d` | (unknown verb) pick a disposition for this item only, ephemeral |
| `y` | (unknown verb) remember verb → append to `org-gtd-verb-routes` |
| `C-c C-k` | abort back to organize transient; nothing filed |

**Live preview:** minibuffer tail rewrites on each keystroke — `Read` → `Next action @read` (or Read/Review list if `REC-NXT-05` present) → retype `Rev` → `Review` → `Next action`. The route resolves visibly before any commit.

## 5. Fit with org-gtd

- **Extends:** the **organize transient** dispatch (`org-gtd-organize`, `org-gtd--dispatch`) — verb-first is a *thin alternate router into the same disposition commands*, plus context-tagging via the classic `org-gtd-organize-hooks`/`org-set-tags` idiom. Reads the WIP heading [R]; reuses the process-loop continuation [R].
- **Shared surface / cluster:** singleton — shares no lifecycle. But it **must feel identical** to the organize transient it parallels: same `org-gtd-<name>` commands, same resulting properties/tags, same `C-c C-k`/`TAB` back-out semantics, so a verb-routed next action is byte-for-byte indistinguishable from a `s`-routed one. Its `completing-read` verb prompt should match the delegate flow's who-prompt idiom, and its "remember this verb" save should echo (lightly) Cluster E's name→spec persistence, without becoming a full CRUD manager.
- **Reuse vs. new:** *reused* — every disposition command, WIP buffer, continuation, tag mechanism. *New* — the `org-gtd-verb-routes` defcustom, the leading-verb parser, and the confirm/preview prompt.
- **Release tag:** leans on `[R]` organize transient + dispatch + WIP + continuation. Adding the `v` row **reworks an [R] surface** — justified on **UX**: it lowers the taxonomy-learning barrier (a novice types "Buy milk" and never learns that Buy ⇒ single action in errands context), and on **GTD-fidelity** (it rewards verb-led phrasing, which the book mandates). Optional route target *Read/Review list* is `[U]` (depends on `REC-NXT-05`).

### Type / extension-UX opportunities

Strong one. The verb router is a *second* hand-maintained dispatch map living beside the already-hand-synced trio (type entry + transient layout + help text — the primer's "three places in sync by hand"). Rather than add a fourth, fold verb aliases **into the type registry** as a per-type `:verbs` / `:aliases` slot (e.g. `next-action :verbs (call email draft …)`, with optional `:context` per alias). Then the organize transient, the help content, *and* the verb router all **generate from one source**. This turns REC-UI-07 into the forcing function for "generate the organize transient from the registry" — the exact type-UX refactor the primer flags as highest-value. If pursued, the resurrected `[X]` `define-type` should accept `:verbs` as a first-class slot.

## 6. Edge cases & failure modes

- **Empty state:** no `org-gtd-verb-routes` customization → ship a sensible default map (Call/Email/Text→next-action @calls·@agenda; Buy/Get/Pick-up→@errands; Draft/Write/Code→@computer; Brainstorm/Plan/Outline→project; Ask/Delegate→delegate; File/Note→knowledge; Read/Review→next-action/@read). Works out of the box.
- **Ambiguous verb** ("Review" could be a quick glance or a project kickoff): route to the *safest single-step* default (next-action) and let `TAB`→full menu correct it; never guess into a project silently.
- **No leading verb / non-verb heading:** the `v` row shows "(no verb detected)"; pressing it opens the completing-read verb picker instead of a pre-fill. No error.
- **Unknown verb:** the teaching prompt (mock above) — offer route-once or remember; never dead-ends.
- **Bad/duplicate route in defcustom:** last-write-wins on lookup; `org-gtd-verb-routes-edit` flags a verb mapped twice with a `message` warning, in the teaching-error voice — no crash.
- **When it goes wrong:** every failure resolves to "fall through to the organize transient you already know," so verb-first can never trap the user.

## 7. Open questions & maintainer decisions

- **Does verb entry apply an org tag as "context"?** org-gtd has no first-class context object; verbs would stamp plain org tags (`@calls`). Confirm that's acceptable, or scope contexts out and route on disposition only.
- **`v` on the transient vs. a separate capture-time front-door** — is organize-time the only home, or should verb entry also front-run capture?
- **Registry `:verbs` slot now or later** — ship REC-UI-07 with a standalone `org-gtd-verb-routes` defcustom, or block on the registry-generated-transient refactor? (Recommend: ship standalone, design the slot as the migration target.)
- **Read/Review verb target** — depends on whether `REC-NXT-05` (Read/Review someday sub-list) ships in v5.

## 8. Provenance & links

`REC-UI-07` · deliverable-#3 status **Not-implemented** · `tool/may` · source **DA94-26** (verb-menu entry, "get/buy → errands"); no `gap-implementation-strategies.md` build-route entry (net-new) · related `WF-11` organize buckets · sibling surfaces: `org-gtd-organize` transient (parallel dispatch), `REC-NXT-05` (Read/Review route target), `REC-X-04` (`ENERGY` — another optional organize-time attribute) · cluster: **Singleton** (`_CLUSTERS.md` §Singletons).
