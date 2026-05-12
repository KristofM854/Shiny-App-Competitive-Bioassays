// bs-handlers.js — custom Shiny message handlers for Bioassay Suite

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
