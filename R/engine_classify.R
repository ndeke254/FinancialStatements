#' Classify a PDF page as "native" or "scanned" (FR-EX-01)
#'
#' Uses `pdftools::pdf_data()` to retrieve word-level bounding boxes.
#' A page is **native** when its text layer is dense enough to reconstruct
#' a table geometrically.  It is **scanned** when the text layer is absent or
#' too sparse (e.g. a JPEG-embedded scan with no embedded text, as in ABSA).
#'
#' Classification is **per page** (PRD §7.2) — a multi-page range may contain
#' a mix; the caller must classify each page independently.
#'
#' @name engine_classify
NULL

# ---------------------------------------------------------------------------
# Thresholds
# ---------------------------------------------------------------------------

# Minimum number of meaningful tokens to call a page "native"
.NATIVE_MIN_TOKENS <- 15L

# Minimum total printable characters across all tokens
.NATIVE_MIN_CHARS <- 40L

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Classify a single PDF page
#'
#' @param pdf_path Path to the PDF file.
#' @param page     Integer page number (1-based).
#' @return `"native"` or `"scanned"`.
#' @export
classify_page <- function(pdf_path, page) {
  stopifnot(file.exists(pdf_path), is.numeric(page), length(page) == 1L)

  page_data <- tryCatch(
    pdftools::pdf_data(pdf_path)[[as.integer(page)]],
    error = function(e) NULL
  )

  classify_tokens(page_data)
}

#' Classify a pre-loaded token data.frame (exported for testing)
#'
#' Separates the classification logic from the file I/O so it can be
#' exercised in unit tests without a real PDF.
#'
#' @param tokens A `data.frame` as returned by `pdftools::pdf_data()[[page]]`,
#'   with at minimum a `text` column.  `NULL` or zero-row input → `"scanned"`.
#' @return `"native"` or `"scanned"`.
#' @export
classify_tokens <- function(tokens) {
  if (is.null(tokens) || !is.data.frame(tokens) || nrow(tokens) == 0L) {
    return("scanned")
  }

  texts <- trimws(as.character(tokens[["text"]]))

  # Count tokens with meaningful content (not whitespace)
  meaningful <- texts[nzchar(texts)]
  n_tokens   <- length(meaningful)

  # Count printable characters across all meaningful tokens
  n_chars <- sum(nchar(meaningful))

  if (n_tokens >= .NATIVE_MIN_TOKENS && n_chars >= .NATIVE_MIN_CHARS) {
    "native"
  } else {
    "scanned"
  }
}
