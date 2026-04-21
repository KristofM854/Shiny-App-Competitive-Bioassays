# Guided Tour — B.0 Diagnosis Note

**Date:** 2026-04-20
**Branch:** `claude/implement-documentation-plan-uIPrF`
**Source:** `server_common.R`, `observeEvent(input$start_tour, ...)`

## What was checked

Static read of the current `onbeforechange` callback (server_common.R:381-391):

```js
function(targetElement) {
  var $pane = $(targetElement).closest('.tab-pane');
  if ($pane.length && !$pane.hasClass('active')) {
    var targetTab = $pane.data('value');
    if (targetTab) {
      Shiny.setInputValue('tour_set_tab', targetTab,
                          {priority: 'event'});
    }
  }
}
```

…paired with the companion `observeEvent(input$tour_set_tab, ...)` (server_common.R:397-404)
that calls `updateTabsetPanel()` to swap the active wizard tab.

## Finding (matches plan B.0 Cause 1)

`Shiny.setInputValue` is asynchronous: the value rides the websocket → R fires the
observer → R sends `updateTabsetPanel` back to the client → Shiny applies it →
only then does the new `.tab-pane` become `.active` and pick up `display: block`.

intro.js does not wait for any of that. Right after `onbeforechange` returns, it
reads the target element's bounding rect and positions the highlight + tooltip.
For the very first step on a new tab the rect comes from a still-hidden pane
(width 0 / off-screen), which is exactly why the tooltip lands on the wrong
area or is nowhere near the intended element.

The first step (`#language_toggle_section`) lives outside the tabsetPanel, so
that one looks fine — masking the bug until the tour crosses a tab boundary.

## Live console.log step skipped

I would normally instrument the callback with
`console.log(targetElement.id, $pane.hasClass('active'))` and step through with
DevTools open to confirm. R is not available in this sandbox, so the app cannot
be launched here. The static evidence above (async `Shiny.setInputValue`,
`event`-priority observer, no synchronous tab activation on the JS side) is
sufficient to confirm Cause 1, and B.1's fix (`readCallback("switchTabs")`)
addresses it directly: the rintrojs helper calls `jQuery.fn.tab('show')`
synchronously inside the `onbeforechange` callback, so the pane is `.active`
before intro.js measures geometry.

## Other plan causes

- **Cause 2/5** (conditional panels): the assay-branched step list already
  only includes the panel for the active assay, so the hidden conditional
  panel never appears in `tour_steps`. Will revisit under B.4 if behavior
  diverges after B.1 lands.
- **Cause 3** (starting tab): `updateTabsetPanel(... "tab_config")` immediately
  precedes `introjs(...)` — same async issue as Cause 1; the first non-config
  step in a session that starts on a different tab races the same way.
  Resolved indirectly by B.1 (synchronous switch on every `onbeforechange`).
- **Cause 4** (zigzag matrix order): cosmetic, addressed by B.3.
