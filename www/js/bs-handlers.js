// bs-handlers.js — custom Shiny message handlers for Bioassay Suite

// ---------------------------------------------------------------------------
// Plate-preview row alignment
// Measures the pixel position of the first visible rHandsontable data cell and
// the first well cell in the mini preview grid, then adjusts paddingTop of
// .bs-plate-preview until row A of both panels sits on the same baseline.
// ---------------------------------------------------------------------------
function bsAlignPlatePreview() {
  var previewBox = document.querySelector('#layout_preview_plate .bs-plate-preview');
  if (!previewBox) return;

  // Reset so each call starts from the CSS baseline of 10 px.
  previewBox.style.paddingTop = '';

  requestAnimationFrame(function() {
    // First visible Handsontable data row cell (any of the three matrices).
    var htCells = document.querySelectorAll(
      '.bs-matrix-viewport .htCore tbody tr:first-child td:first-child'
    );
    var htCell = null;
    for (var i = 0; i < htCells.length; i++) {
      var r = htCells[i].getBoundingClientRect();
      if (r.width > 0 && r.height > 0) { htCell = htCells[i]; break; }
    }

    // Grid children layout:
    // [0]       corner cell
    // [1..12]   column labels (12 cells)
    // [13]      row-A label
    // [14]      A1 well  ← this is what we align to row A of the ht
    var previewGrid = document.querySelector(
      '#layout_preview_plate .bs-plate-grid-wrap'
    );
    if (!htCell || !previewGrid) return;

    var firstWell = previewGrid.children[14];
    if (!firstWell) return;

    var htRect   = htCell.getBoundingClientRect();
    var wellRect = firstWell.getBoundingClientRect();
    var delta    = htRect.top - wellRect.top;

    if (Math.abs(delta) < 2) return; // already aligned

    var currentPad = parseFloat(getComputedStyle(previewBox).paddingTop) || 10;
    previewBox.style.paddingTop = Math.max(4, currentPad + delta) + 'px';
  });
}

// Re-run whenever the preview output re-renders.
$(document).on('shiny:value', function(e) {
  if (e.name === 'layout_preview_plate') setTimeout(bsAlignPlatePreview, 120);
});

// Re-run when assay type or active layer changes.
// active_layer switches which conditionalPanel (and rHandsontable) is visible,
// changing the measured first-row top without re-rendering layout_preview_plate.
$(document).on('shiny:inputchanged', function(e) {
  if (e.name === 'assay_type' || e.name === 'active_layer') {
    setTimeout(bsAlignPlatePreview, 250);
  }
});

// Final pass once Shiny becomes idle (catches deferred renders).
$(document).on('shiny:idle', function() { bsAlignPlatePreview(); });

// ---------------------------------------------------------------------------

Shiny.addCustomMessageHandler("bs:copy_to_clipboard", function(text) {
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).catch(function() {
      fallbackCopy(text);
    });
  } else {
    fallbackCopy(text);
  }
});

function fallbackCopy(text) {
  var el = document.createElement("textarea");
  el.value = text;
  el.style.position = "fixed";
  el.style.opacity = "0";
  document.body.appendChild(el);
  el.select();
  try { document.execCommand("copy"); } catch (e) {}
  document.body.removeChild(el);
}
