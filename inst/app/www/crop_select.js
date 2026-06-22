/**
 * crop_select.js — rubber-band selection on a rendered PDF page image.
 *
 * Usage (from R via session$sendCustomMessage):
 *   session$sendCustomMessage("initCropSelect", list(
 *     containerId = ns("page_container"),   // id of the wrapping div
 *     inputId     = ns("crop_box")          // Shiny input to set
 *   ))
 *
 * The Shiny input is set to a list {x1, y1, x2, y2} where each value is
 * a fraction [0, 1] of the image dimensions.  R converts to PDF points.
 *
 * Set `priority: 'event'` so re-drawing the same box still fires.
 */
(function () {
  "use strict";

  function initCropSelect(container, inputId) {
    var isDragging = false;
    var startFracX = 0, startFracY = 0;

    // Create overlay rectangle (absolutely positioned inside container)
    var sel = document.createElement("div");
    sel.className = "crop-selection";
    sel.style.cssText =
      "position:absolute;border:2px dashed #0d6efd;background:rgba(13,110,253,0.08);" +
      "pointer-events:none;display:none;box-sizing:border-box;";
    container.appendChild(sel);

    function fracPos(e) {
      var rect = container.getBoundingClientRect();
      return {
        x: Math.max(0, Math.min(1, (e.clientX - rect.left)  / rect.width)),
        y: Math.max(0, Math.min(1, (e.clientY - rect.top)   / rect.height))
      };
    }

    function drawSel(x1, y1, x2, y2) {
      sel.style.left   = (Math.min(x1, x2) * 100) + "%";
      sel.style.top    = (Math.min(y1, y2) * 100) + "%";
      sel.style.width  = (Math.abs(x2 - x1) * 100) + "%";
      sel.style.height = (Math.abs(y2 - y1) * 100) + "%";
    }

    container.addEventListener("mousedown", function (e) {
      if (e.button !== 0) return;
      var p = fracPos(e);
      startFracX = p.x;
      startFracY = p.y;
      isDragging = true;
      sel.style.display = "block";
      drawSel(p.x, p.y, p.x, p.y);
      e.preventDefault();
    });

    document.addEventListener("mousemove", function (e) {
      if (!isDragging) return;
      var p = fracPos(e);
      drawSel(startFracX, startFracY, p.x, p.y);
    });

    document.addEventListener("mouseup", function (e) {
      if (!isDragging) return;
      isDragging = false;
      var p = fracPos(e);
      var x1 = Math.min(startFracX, p.x);
      var x2 = Math.max(startFracX, p.x);
      var y1 = Math.min(startFracY, p.y);
      var y2 = Math.max(startFracY, p.y);
      // Only report non-trivial selections (> 1% in each dimension)
      if ((x2 - x1) > 0.01 && (y2 - y1) > 0.01) {
        Shiny.setInputValue(inputId, { x1: x1, y1: y1, x2: x2, y2: y2 },
                            { priority: "event" });
      }
    });
  }

  Shiny.addCustomMessageHandler("initCropSelect", function (msg) {
    function attach(container) {
      // Guard: skip if already initialised for this container
      if (container.dataset.cropInit === "1") return;
      container.dataset.cropInit = "1";
      initCropSelect(container, msg.inputId);
    }

    var container = document.getElementById(msg.containerId);
    if (!container) {
      // The uiOutput may not be in the DOM yet — retry after Shiny renders
      setTimeout(function () {
        var c2 = document.getElementById(msg.containerId);
        if (c2) attach(c2);
      }, 600);
      return;
    }
    attach(container);
  });

  // Allow R to clear the selection overlay
  Shiny.addCustomMessageHandler("clearCropSelect", function (msg) {
    var container = document.getElementById(msg.containerId);
    if (!container) return;
    var sel = container.querySelector(".crop-selection");
    if (sel) sel.style.display = "none";
  });
})();
