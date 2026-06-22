#' Propose `row_role` from label keywords and styling cues (FR-ED-05, PRD §7.5)
#'
#' Heuristically classifies each row as one of:
#' `header` | `line` | `subtotal` | `total` | `ratio`
#'
#' The result is a **proposal** — the user confirms or overrides in the edit
#' grid (FR-ED-05).  Engine logic stays Shiny-free; these are plain functions.
#'
#' Detection priority (highest wins):
#' 1. `ratio`   — EPS/DPS/per-share/ratio keywords, or row has `%` values
#' 2. `total`   — "total" keyword (standalone or terminal), "net total"
#' 3. `subtotal`— "net …", "gross …", "profit …/loss …", "result", "surplus",
#'                "subtotal", etc.
#' 4. `header`  — no numeric value present (all value cells are NA/blank/dash),
#'                or label matches known section-heading patterns
#' 5. `line`    — default
#'
#' @name detect_row_role
NULL

# ---------------------------------------------------------------------------
# Keyword patterns (case-insensitive, tested against trimmed label)
# ---------------------------------------------------------------------------

# ratio: per-share rows, percentage summaries
.RATIO_RE <- paste0(
  "\\beps\\b|\\bdps\\b",
  "|\\bper\\s+share\\b",
  "|\\bper\\s+ordinary\\s+share\\b",
  "|\\bearnings\\s+per\\b",
  "|\\bdividend\\s+per\\b",
  "|\\bcapital\\s+adequacy\\b",
  "|\\breturn\\s+on\\b",
  "|\\byield\\b",
  "|\\bratio\\b",
  "|\\bcost[-\\s]to[-\\s]income\\b"
)

# total: terminal or standalone "total" (but exclude subtotal / "not total")
.TOTAL_RE <- paste0(
  "^total\\b",          # starts with "total"
  "|\\btotal$",         # ends with "total"
  "|^net\\s+total\\b",
  "|^total\\s+equity\\b",
  "|^total\\s+assets\\b",
  "|^total\\s+liabilities\\b",
  "|^total\\s+comprehensive\\b",
  "|^grand\\s+total\\b"
)

# subtotal: section-level aggregates — more specific than "total"
.SUBTOTAL_RE <- paste0(
  "^net\\b(?!\\s+total)",                # "net …" but not "net total"
  "|^gross\\b",
  "|^operating\\s+(profit|loss|income|result)\\b",
  "|profit\\s+before\\b",
  "|profit\\s+after\\b",
  "|loss\\s+before\\b",
  "|loss\\s+after\\b",
  "|profit[/\\(]loss\\b",
  "|loss[/\\(]profit\\b",
  "|\\bsubto\\s*tal\\b",
  "|^sub-total\\b",
  "|\\bresult$",
  "|\\bsurplus$",
  "|\\bdeficit$",
  "|income\\s+before\\b",
  "|income\\s+after\\b",
  "|\\bebitda\\b",
  "|\\bebit\\b",
  "|^insurance\\s+service\\s+result\\b",  # CIC corpus
  "|^insurance\\s+finance\\b",
  "|^net\\s+insurance\\b"
)

# header: all-caps section headings, or known section labels
.HEADER_RE <- paste0(
  "^[A-Z][A-Z\\s&/()'-]+$",   # ALL CAPS label (section heading)
  "|^assets$",
  "|^liabilities$",
  "|^equity$",
  "|^income$",
  "|^expenses$",
  "|^revenue$",
  "|^cash\\s+flows?\\b",
  "|^notes?\\s+to\\b"
)

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Propose `row_role` for a single row
#'
#' @param label       The row label string (as extracted from the PDF).
#' @param value_texts Character vector of raw value-cell texts for this row
#'   (used to detect ratio rows from `%` values when the label is ambiguous).
#'   Pass `NULL` or `character(0)` if unavailable.
#' @param has_bold    `TRUE` if the PDF's styling marks this row as bold
#'   (available for native PDFs via `pdftools::pdf_data` font_size heuristic).
#' @param indentation Non-negative integer: how many leading spaces/indent
#'   units the label has.  Deeply indented rows are rarely totals/headers.
#' @return A character scalar: one of
#'   `"header"`, `"line"`, `"subtotal"`, `"total"`, `"ratio"`.
#' @export
detect_row_role <- function(label,
                             value_texts  = NULL,
                             has_bold     = FALSE,
                             indentation  = 0L) {
  lbl <- trimws(as.character(label))
  lbl_lo <- tolower(lbl)

  # ---- 1. ratio -------------------------------------------------------
  if (grepl(.RATIO_RE, lbl_lo, perl = TRUE)) return("ratio")

  # Also ratio if any value cell carries a % suffix
  if (!is.null(value_texts) && length(value_texts) > 0L) {
    pct_vals <- trimws(as.character(value_texts))
    if (any(grepl("%\\s*$", pct_vals, perl = TRUE))) return("ratio")
  }

  # ---- 2. total -------------------------------------------------------
  if (grepl(.TOTAL_RE, lbl_lo, perl = TRUE)) return("total")

  # Bold text + a "total"-like label is a strong total signal
  if (has_bold && grepl("\\btotal\\b", lbl_lo, perl = TRUE)) return("total")

  # ---- 3. subtotal ----------------------------------------------------
  if (grepl(.SUBTOTAL_RE, lbl_lo, perl = TRUE)) return("subtotal")

  # Bold but not matching total/subtotal keywords → likely a section subtotal
  # (Only apply when the row actually has numeric content)
  if (has_bold && !is.null(value_texts) && length(value_texts) > 0L) {
    has_num <- any(grepl("[0-9]", trimws(as.character(value_texts))))
    if (has_num) return("subtotal")
  }

  # ---- 4. header ------------------------------------------------------
  # A row with no numeric values is a header/section label
  no_numeric_values <- is.null(value_texts) || length(value_texts) == 0L ||
    all(trimws(as.character(value_texts)) %in%
          c("", "-", "\u2013", "\u2014", "NA", NA_character_))

  if (no_numeric_values) return("header")

  # ALL-CAPS or known section-heading pattern (even if it has some values)
  if (grepl(.HEADER_RE, lbl, perl = TRUE)) return("header")

  # ---- 5. default: line -----------------------------------------------
  "line"
}

#' Propose `row_role` for a data.table of rows (vectorised)
#'
#' A convenience wrapper that applies [detect_row_role()] row-by-row over a
#' `data.table` or `data.frame`.  Adds a `row_role` column in place.
#'
#' @param dt            A `data.table` containing at least a `label` column.
#' @param value_cols    Character vector of column names holding raw value-cell
#'   texts.  If `NULL`, `detect_row_role` runs without value-text context.
#' @param bold_col      Name of a logical column indicating bold styling, or
#'   `NULL`.
#' @param indent_col    Name of an integer column with indentation level, or
#'   `NULL`.
#' @return The input `dt` (modified in place) with a `row_role` column added
#'   (existing values overwritten).
#' @export
detect_row_roles <- function(dt, value_cols = NULL,
                              bold_col = NULL, indent_col = NULL) {
  stopifnot(data.table::is.data.table(dt), "label" %in% names(dt))

  # Use data.table::set() to avoid the cedta() gate that fires when [,:=]
  # is called from outside a data.table-aware package context.
  roles <- mapply(
    function(lbl, row_idx) {
      vtexts <- if (!is.null(value_cols)) {
        unlist(dt[row_idx, value_cols, with = FALSE])
      } else {
        NULL
      }
      bold <- if (!is.null(bold_col) && bold_col %in% names(dt)) {
        dt[[bold_col]][[row_idx]]
      } else {
        FALSE
      }
      ind <- if (!is.null(indent_col) && indent_col %in% names(dt)) {
        dt[[indent_col]][[row_idx]]
      } else {
        0L
      }
      detect_row_role(lbl, vtexts, bold, ind)
    },
    lbl     = dt[["label"]],
    row_idx = seq_len(nrow(dt)),
    SIMPLIFY = TRUE
  )
  data.table::set(dt, j = "row_role", value = as.character(roles))

  invisible(dt)
}
