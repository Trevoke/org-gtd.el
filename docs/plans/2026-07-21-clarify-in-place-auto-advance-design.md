# Clarify-in-place → auto-advance for the overdue-calendar review — Design

**Status:** Designed 2026-07-21. Follow-up to PR #298 (overdue-calendar review).
**Target:** v5 (`org-gtd-5`), on branch `feature/overdue-calendar-review`.

## 1. Goal

Make the `c` (clarify) disposition of `org-gtd-reflect-missed-calendar-review`
behave like the inbox loop: pressing `c` drops the current overdue item into the
full clarify → organize flow, and **when the user finishes organizing it, the
review automatically advances to the next overdue item.** No premature advance
(the current shipped behavior parks the walk and makes the user press `s`); no
separate orphan clarify buffer.

## 2. Why it isn't free today

`org-gtd-clarify-item` always opens a *separate* WIP buffer keyed by the item's
own clarify-id. In that buffer `org-gtd-walk--active` is nil (it is buffer-local +
permanent-local to the review's *surface* buffer). So when the user organizes
there, `org-gtd-organize--call` (org-gtd-organize-core.el) takes its one-off
branch — it never sees an active walk, so it never calls `org-gtd-walk-advance`.

The inbox walk gets auto-advance precisely because its **surface buffer *is* the
clarify buffer**: `org-gtd-inbox-walk--render-marker` renders each item as an
editable clarify buffer *in the walk surface*, so `org-gtd-walk--active`
(permanent-local, survives the `org-gtd-clarify-mode` switch) is still present
when organize runs, and `org-gtd-organize--call`'s `walk-active` branch fires
`org-gtd-walk-advance`.

## 3. Approach — reuse the inbox in-place-clarify pattern

The review walk becomes a **hybrid**: each item normally renders as the read-only
disposition console; the `c` disposition transforms that *same surface buffer*
into an in-place editable clarify buffer for the current item. Organize then
advances the walk via the existing engine seam, and the next item renders back as
the console.

### 3.1 `--clarify-in-place (marker)` (new)

Mirrors `org-gtd-inbox-walk--render-marker` (deliberately *not* DRY-extracted yet
— see §7). In the surface buffer:

1. capture `old-id` = current `org-gtd-clarify--clarify-id` or the surface key;
2. `new-id` = `org-gtd-id-get-create marker` (lazy id stamp on the source);
3. `inhibit-read-only` t; `erase-buffer`;
4. `org-gtd--without-kill-merge` around
   `org-gtd-clarify--initialize-buffer-contents marker surface` (copies the
   subtree in, strips org-gtd state props);
5. `org-gtd-wip--rekey old-id new-id` (so the WIP registry + org-id resolution
   track the surface as this item's clarify buffer — same rationale as the inbox
   twin's long comment: do *not* suppress `org-id-track-globally` here);
6. `(unless (derived-mode-p 'org-gtd-clarify-mode) (org-gtd-clarify-mode))`
   (`org-gtd-walk--active` survives — permanent-local);
7. set buffer-locals: `org-gtd-clarify--clarify-id` = new-id,
   `--source-heading-marker` = marker, `--skip-refile` = nil,
   `--window-config` = the window config captured **before** `c` reconfigured
   windows (so organize's post-advance and cancel can behave sanely);
8. `set-buffer-modified-p nil`;
9. install a **buffer-local `C-c C-k` override** (see §3.3);
10. `org-gtd-clarify-setup-windows surface` (same clarify UX as normal, incl. the
    horizons reference window).

`c` (`org-gtd-reflect-missed-calendar-review-clarify`) resolves the current
item's marker and calls `--clarify-in-place`; it does **not** advance or bump.

### 3.2 Console `--render`: undo the clarify staging before drawing

`--render` runs on every settle/advance. After a `c` + organize, the surface
arrives still keyed under the item's clarify-id and (mode already about to switch)
carrying clarify state + a horizons window. So at the **top** of `--render`,
before drawing the console:

- if `org-gtd-clarify--clarify-id` is set, `org-gtd-wip--rekey` the surface back
  to `--surface-key` (so `:on-finish`/`-quit` cleanup by the surface key still
  finds it — no leak);
- `org-gtd-clarify--cleanup-horizons-view` (tear down the reference window the
  clarify staging opened; the console is not a clarify view);

then draw the console as today (switch to console mode — which clears the
non-permanent clarify buffer-locals via `kill-all-local-variables` — set
read-only, header-line, `pop-to-buffer`). This path is a no-op for items that
were never clarified (clarify-id nil, no horizons window).

### 3.3 Cancel (`C-c C-k`) semantics — **decision: return to the console**

Default `org-gtd-clarify-stop` dispatches (on `org-gtd-walk--active`) to the
inbox-flavored `org-gtd-clarify--stop-walk`, which *abandons the whole walk*
(`org-gtd-walk-quit`) plus inbox-specific queue/horizons teardown. That is wrong
for a per-item escape hatch: cancelling one clarify should **not** end the whole
review.

So the in-place clarify installs a buffer-local keymap (parent =
`org-gtd-clarify-mode-map`) that rebinds `C-c C-k` to
`--cancel-clarify`, which re-renders the **console for the current item** (rekey
back, cleanup horizons, redraw read-only via the walk's own render) without
advancing or quitting. The review continues; the item stays overdue until
disposed. (Alternative considered — inbox-consistent "cancel = quit the review" —
rejected: surprising, and its inbox cleanup path assumes a duplicate queue /
horizons state this walk doesn't own.)

### 3.4 Organize completion — already handled

No new code: `org-gtd-organize--call`'s `walk-active` branch does the source cut,
`org-gtd-walk-advance`, and `org-gtd-save-buffers` for us; an `org-gtd-error`
stays on the current item; an unexpected `:render` error releases the scope lock
and re-signals.

## 4. End-to-end flow

1. Console shows overdue item N (read-only).
2. `c` → `--clarify-in-place` → surface becomes the editable clarify buffer for N,
   walk still active.
3. User organizes N (picks a type in the `org-gtd-organize` transient).
4. `org-gtd-organize--call` (walk-active) cuts the source, `org-gtd-walk-advance`.
5. Advance → settle → `--render` for item N+1 → rekey-back + horizons-cleanup +
   console redraw. Review continues at N+1.
6. On the last item, advance finishes the walk (`:on-finish` cleans up the
   surface + reports the tally).
7. If instead the user cancels (`C-c C-k`) at step 3 → `--cancel-clarify` redraws
   the console for N; review continues.

## 5. Counters

Clarify still bumps no counter on entry (it is not a resolution). When organize
completes it is a genuine disposition, but the engine advance path doesn't know
our counter keys. Keep it simple for v1: clarify does not increment `:reviewed`
or any action counter (the item's disposition was recorded by the organize flow
itself, outside our tally). Documented as a known limitation; revisit if a
"clarified N" counter is wanted.

## 6. Testing

- **Happy path**: overdue item → start review → `c` → assert surface is now
  `org-gtd-clarify-mode`, editable, holds the item, `org-gtd-walk--active` still
  set. Then drive organize-to-completion the way the inbox/organize tests do
  (invoke the organize path for a concrete type, e.g. single-action) and assert
  the walk **advanced**: for a 2-item walk the console now shows item 2; for a
  1-item walk the walk finished (surface cleaned up) and the item is now the
  organized type (no longer overdue calendar).
- **Cancel path**: `c` → `--cancel-clarify` → assert surface is back in console
  mode (read-only, disposition header-line) for the *same* item, walk still
  active, cursor unchanged.
- **Rekey/cleanup**: after `c` + organize + finish, no leaked WIP surface buffer
  (cleanup by surface key still works); no leftover horizons window.
- **Regression**: full suite stays green (esp. inbox-walk + someday-review +
  the existing 29 missed-calendar tests, including the park/guard tests, which
  must be updated to the new advance-on-organize behavior where they assert the
  old park semantics).

## 7. Deferred

- **DRY**: extract the shared in-place-clarify render (`inbox-walk--render-marker`
  and this module's `--clarify-in-place`) into one helper (likely in
  `org-gtd-clarify.el`). Not now — refactoring the inbox render risks the inbox
  test suite; do it once this second consumer is proven.
- A first-class "clarified" counter.
- Generalizing `org-gtd-clarify-stop` to be walk-spec-aware instead of the
  buffer-local `C-c C-k` override (cleaner, but engine/clarify surgery).
