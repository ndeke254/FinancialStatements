#' OCR extraction engine — stub for M4 (FR-EX-03)
#'
#' @description
#' In **M4** this will:
#' 1. Render the PDF page to a high-resolution raster with
#'    `pdftools::pdf_render_page()` (≥ 300 DPI).
#' 2. Run `tesseract::ocr_data()` to obtain word bounding boxes.
#' 3. Feed the result into [reconstruct_table()] (shared with the native path).
#'
#' In **v1 / M2** this stub returns `status = "not_available"` so the
#' dispatcher can fall back gracefully and the caller can surface an
#' informative message to the user.
#'
#' The function signature is identical to [engine_native_extract()] so the
#' strategy interface in [extract()] requires no branching changes in M4.
#'
#' @param pdf_path Path to the PDF file.
#' @param page     Integer page number (1-based).
#' @param region   Named list `list(xmin, xmax, ymin, ymax)` or `NULL`.
#' @return An extraction result list with `status = "not_available"`.
#' @export
engine_ocr_extract <- function(pdf_path, page, region = NULL) {
  message("engine_ocr_extract: OCR engine is not implemented in M2. ",
          "Implement in M4 (tesseract path).")

  list(
    table_dt     = data.table::data.table(
      line_item_order = integer(0),
      label           = character(0)
    ),
    n_value_cols = 0L,
    col_x_bands  = numeric(0),
    method       = "ocr",
    status       = "not_available",
    page         = as.integer(page),
    region       = region
  )
}
