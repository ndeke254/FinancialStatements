#' Shared geometric table reconstruction from word bounding boxes (FR-EX-02)
#'
#' Implements the core of PRD §7.3:
#' 1. Cluster tokens into **row bands** using Y-gap detection.
#' 2. Identify **value column bands** from numeric token X-midpoints; column
#'    bands are seeded from the first numeric row's positions (handles 12-column
#'    KCB case).
#' 3. Build a **wide raw matrix**: one row per row band, one column per
#'    detected value column, plus a `label` column for the leftmost text.
#'
#' Used by both the native engine (M2) and the OCR engine (M4, which feeds
#' OCR-derived tokens into the same reconstruction).
#'
#' @name engine_reconstruct
NULL

# ---------------------------------------------------------------------------
# Constants (all in PDF points, 72 pt = 1 inch)
# ---------------------------------------------------------------------------

# Fraction of median token height used as row-gap threshold
.ROW_GAP_FACTOR  <- 0.75

# Fraction of page width considered the "label zone" (left side)
.LABEL_ZONE_FRAC <- 0.40

# Minimum absolute column gap (pts) between distinct value columns
.COL_GAP_MIN_PT  <- 8L

# Regex matching tokens that look like value cells
.VALUE_TOKEN_RE  <- paste0(
  "^[0-9,]+$",                  # plain integer with commas
  "|^[0-9,]+\\.[0-9]+$",        # decimal
  "|^\\([0-9,]+\\)$",           # negative parens
  "|^\\([0-9,]+\\.[0-9]+\\)$",  # negative decimal parens
  "|^[0-9,]+%$",                # percentage
  "|^[0-9.]+%$",                # decimal percentage
  "|^[-\u2013\u2014]$"          # dash / en-dash / em-dash
)

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Reconstruct a table from word bounding-box tokens
#'
#' @param tokens     A `data.frame` (or `data.table`) with columns
#'   `x`, `y`, `width`, `height`, `text` as returned by
#'   `pdftools::pdf_data()[[page]]`.  A `space` column is ignored.
#' @param region     Named list with elements `xmin`, `xmax`, `ymin`, `ymax`
#'   (all in PDF points), or `NULL` for the full page.
#' @param page_width Page width in PDF points (`pdftools::pdf_info()$pagesize`
#'   or computed from `max(tokens$x + tokens$width)`).  Used to determine the
#'   label zone boundary when `region` is `NULL`.
#' @param method     Extraction method string to embed in the result
#'   (`"native"` or `"ocr"`).
#' @return A named list (the "extraction result"):
#'   \describe{
#'     \item{`table_dt`}{`data.table` with columns `line_item_order` (int),
#'       `label` (chr), `col_1` … `col_N` (chr).}
#'     \item{`n_value_cols`}{Number of detected value columns.}
#'     \item{`col_x_bands`}{Numeric vector of column band X-centres (pts).}
#'     \item{`method`}{The method string passed in.}
#'     \item{`status`}{`"extracted"` on success, `"failed"` otherwise.}
#'   }
#' @export
reconstruct_table <- function(tokens, region = NULL,
                               page_width = NULL, method = "native") {
  tokens <- data.table::as.data.table(tokens)

  # ---- 0. Filter to region --------------------------------------------------
  if (!is.null(region)) {
    tokens <- tokens[
      x >= region$xmin & (x + width)  <= region$xmax &
      y >= region$ymin & (y + height) <= region$ymax
    ]
  }

  if (nrow(tokens) == 0L) {
    return(.failed_result(method))
  }

  # Drop blank tokens — use $ to avoid shadowing by base::text()
  tokens <- tokens[nzchar(trimws(tokens$text))]
  if (nrow(tokens) == 0L) {
    return(.failed_result(method))
  }

  # ---- 1. Row-band clustering -----------------------------------------------
  tokens[, y_center := y + height / 2]
  data.table::setorderv(tokens, c("y_center", "x"))

  med_h         <- stats::median(tokens$height, na.rm = TRUE)
  row_gap_thr   <- max(med_h * .ROW_GAP_FACTOR, 2.0)

  tokens[, row_gap_here := c(0, diff(y_center))]
  tokens[, row_band     := cumsum(row_gap_here > row_gap_thr) + 1L]

  # ---- 2. Detect value column bands -----------------------------------------
  tokens[, x_center := x + width / 2]

  # Determine label zone right edge
  if (is.null(page_width)) {
    page_width <- max(tokens[, x + width]) + 1
  }
  label_zone_right <- page_width * .LABEL_ZONE_FRAC

  # Use tokens that look like values AND are in the value zone (right of label)
  value_tok <- tokens[
    x_center > label_zone_right & grepl(.VALUE_TOKEN_RE, text, perl = TRUE)
  ]

  # Fallback: if we found no value tokens with strict pattern, loosen to
  # all tokens right of the label zone
  if (nrow(value_tok) == 0L) {
    value_tok <- tokens[x_center > label_zone_right]
  }

  col_x_bands <- .detect_col_bands(value_tok$x_center)

  if (length(col_x_bands) == 0L) {
    return(.failed_result(method))
  }

  # ---- 3. Build wide matrix -------------------------------------------------
  # Assign each token to a column band (label or col_k)
  tokens[, col_band := .assign_col_band(x_center, col_x_bands, label_zone_right)]

  # Within each (row_band, col_band), concatenate tokens left-to-right
  cell_texts <- tokens[, .(
    cell_text = paste(text[order(x)], collapse = " ")
  ), by = .(row_band, col_band)]

  # Pivot to wide
  wide <- data.table::dcast(
    cell_texts,
    row_band ~ col_band,
    value.var = "cell_text",
    fill = ""
  )

  # Rename columns: "label" + "col_1", "col_2", ...
  value_col_ids <- sort(setdiff(
    names(wide),
    c("row_band", "label", "0")   # "0" = label band id
  ))

  # band id 0 → "label"; numeric ids → "col_k"
  if ("0" %in% names(wide)) {
    data.table::setnames(wide, "0", "label")
  } else if (!"label" %in% names(wide)) {
    wide[, label := ""]
  }

  # Rename numeric band ids to col_1, col_2, ...
  n_val_cols <- length(value_col_ids)
  if (n_val_cols > 0L) {
    new_names <- paste0("col_", seq_len(n_val_cols))
    data.table::setnames(wide, value_col_ids, new_names)
  }

  # Add line_item_order and clean up
  data.table::setorderv(wide, "row_band")
  wide[, line_item_order := .I]
  wide[, row_band        := NULL]

  # Ensure label column exists and is first
  if (!"label" %in% names(wide)) wide[, label := ""]
  move_cols <- c("line_item_order", "label",
                 paste0("col_", seq_len(n_val_cols)))
  extra     <- setdiff(names(wide), move_cols)
  data.table::setcolorder(wide, c(move_cols, extra))

  list(
    table_dt     = wide,
    n_value_cols = n_val_cols,
    col_x_bands  = col_x_bands,
    method       = method,
    status       = "extracted"
  )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Detect column band X-centres via 1-D gap clustering
.detect_col_bands <- function(x_centers) {
  if (length(x_centers) == 0L) return(numeric(0))

  x_sorted <- sort(x_centers)
  gaps      <- c(Inf, diff(x_sorted))   # first gap is infinite (starts new band)

  # Adaptive gap threshold: max of (.COL_GAP_MIN_PT, half the median gap)
  med_gap   <- stats::median(diff(x_sorted))
  gap_thr   <- max(.COL_GAP_MIN_PT, if (is.na(med_gap)) .COL_GAP_MIN_PT
                                     else med_gap * 0.5)

  band_id   <- cumsum(gaps > gap_thr)
  # Band centres = mean x within each band
  dt        <- data.table::data.table(x = x_sorted, band = band_id)
  dt[, .(center = mean(x)), by = band][order(band)][["center"]]
}

# Assign each token's x_center to the nearest column band
# Returns integer vector of band indices (0 = label column)
.assign_col_band <- function(x_centers, col_band_centers, label_zone_right) {
  half_gap <- if (length(col_band_centers) > 1L) {
    min(diff(sort(col_band_centers))) / 2
  } else {
    .COL_GAP_MIN_PT * 2
  }

  vapply(x_centers, function(xc) {
    if (xc <= label_zone_right) return(0L)
    dists <- abs(col_band_centers - xc)
    nearest <- which.min(dists)
    if (dists[[nearest]] <= half_gap * 2) as.integer(nearest) else 0L
  }, integer(1L))
}

# Return a failed extraction result with empty table
.failed_result <- function(method) {
  list(
    table_dt     = data.table::data.table(
      line_item_order = integer(0),
      label           = character(0)
    ),
    n_value_cols = 0L,
    col_x_bands  = numeric(0),
    method       = method,
    status       = "failed"
  )
}
