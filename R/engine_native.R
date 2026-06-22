#' Native PDF extraction engine (FR-EX-02) and strategy dispatcher
#'
#' @description
#' The **native engine** calls `pdftools::pdf_data()` to obtain word-level
#' bounding boxes, then passes the tokens to [reconstruct_table()] for
#' geometric row/column clustering.  No Java; no `tabulapdf`.
#'
#' The **`extract()` dispatcher** (PRD §7.1 strategy interface) selects the
#' right engine based on the `method` argument or — when `method` is `NULL` —
#' calls [classify_page()] to determine it automatically.  The output shape
#' is identical regardless of which engine runs, so callers never need to
#' branch on method.
#'
#' @name engine_native
NULL

# ---------------------------------------------------------------------------
# Strategy dispatcher — public entry point
# ---------------------------------------------------------------------------

#' Extract a table region from a PDF page
#'
#' Selects the extraction engine automatically (via [classify_page()]) or
#' uses the `method` argument to override.
#'
#' Records `extraction_method` and `extraction_status` on the result
#' (FR-EX-07).
#'
#' @param pdf_path Path to the PDF file.
#' @param page     Integer page number (1-based).
#' @param region   Named list `list(xmin, xmax, ymin, ymax)` in PDF points,
#'   or `NULL` for the full page.
#' @param method   One of `"native"`, `"ocr"`, `"ai"`, or `NULL` (auto-detect
#'   via [classify_page()]).
#' @return An **extraction result** list with elements:
#'   \describe{
#'     \item{`table_dt`}{`data.table`: `line_item_order`, `label`, `col_1`…`col_N`.}
#'     \item{`n_value_cols`}{Number of detected value columns (integer).}
#'     \item{`col_x_bands`}{Numeric vector of column X-centre positions.}
#'     \item{`method`}{The method that was actually used.}
#'     \item{`status`}{ `"extracted"`, `"failed"`, or `"not_available"`.}
#'     \item{`page`}{The page number.}
#'     \item{`region`}{The region crop box (or `NULL`).}
#'   }
#' @export
extract <- function(pdf_path, page, region = NULL, method = NULL) {
  stopifnot(file.exists(pdf_path), is.numeric(page), length(page) == 1L)

  page <- as.integer(page)

  if (is.null(method)) {
    method <- classify_page(pdf_path, page)
  }

  result <- switch(method,
    "native" = engine_native_extract(pdf_path, page, region),
    "ocr"    = engine_ocr_extract(pdf_path, page, region),
    "ai"     = engine_ai_extract(pdf_path, page, region),
    stop("Unknown extraction method: '", method, "'")
  )

  # Attach page + region for FR-EX-07 traceability
  result$page   <- page
  result$region <- region
  result
}

# ---------------------------------------------------------------------------
# Native engine
# ---------------------------------------------------------------------------

#' Extract a page region using the native pdftools text layer (FR-EX-02)
#'
#' Loads word bounding boxes with `pdftools::pdf_data()`, applies an optional
#' region crop, then calls [reconstruct_table()] for geometric clustering.
#'
#' @param pdf_path Path to the PDF file.
#' @param page     Integer page number (1-based).
#' @param region   Named list `list(xmin, xmax, ymin, ymax)` or `NULL`.
#' @return An extraction result list (see [extract()]).
#' @export
engine_native_extract <- function(pdf_path, page, region = NULL) {
  page   <- as.integer(page)
  tokens <- tryCatch(
    pdftools::pdf_data(pdf_path)[[page]],
    error = function(e) {
      message("engine_native_extract: pdf_data error on page ", page,
              ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(tokens) || nrow(tokens) == 0L) {
    return(.native_failed())
  }

  tokens <- data.table::as.data.table(tokens)

  # Page width — needed for label-zone heuristic
  page_width <- tryCatch({
    info <- pdftools::pdf_info(pdf_path)
    # pdf_info returns a list; pagesize is a named numeric vector
    pw <- info[["pagesize"]]
    if (!is.null(pw) && length(pw) >= 1L) pw[[1L]] else NULL
  }, error = function(e) NULL)

  if (is.null(page_width)) {
    page_width <- max(tokens[, x + width]) + 1
  }

  reconstruct_table(tokens, region = region,
                    page_width = page_width, method = "native")
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.native_failed <- function() {
  list(
    table_dt     = data.table::data.table(
      line_item_order = integer(0),
      label           = character(0)
    ),
    n_value_cols = 0L,
    col_x_bands  = numeric(0),
    method       = "native",
    status       = "failed",
    page         = NA_integer_,
    region       = NULL
  )
}
