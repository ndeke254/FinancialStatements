#' AI/vision extraction engine — stub for v2 (FR-EX-04)
#'
#' @description
#' In **v2** this will:
#' 1. Render the PDF page to a raster image.
#' 2. Submit the image to a vision model.
#' 3. Parse the structured JSON response into the standard extraction result.
#'
#' In **v1** this stub is **disabled** and always returns
#' `status = "not_available"`.  The strategy interface in [extract()] is
#' designed so that wiring the real implementation in v2 requires only
#' filling in this function body — no changes elsewhere (FR-EX-04).
#'
#' @param pdf_path Path to the PDF file.
#' @param page     Integer page number (1-based).
#' @param region   Named list `list(xmin, xmax, ymin, ymax)` or `NULL`.
#' @return An extraction result list with `status = "not_available"`.
#' @export
engine_ai_extract <- function(pdf_path, page, region = NULL) {
  message("engine_ai_extract: AI engine is stubbed and disabled in v1. ",
          "Wire a real vision model in v2.")

  list(
    table_dt     = data.table::data.table(
      line_item_order = integer(0),
      label           = character(0)
    ),
    n_value_cols = 0L,
    col_x_bands  = numeric(0),
    method       = "ai",
    status       = "not_available",
    page         = as.integer(page),
    region       = region
  )
}
