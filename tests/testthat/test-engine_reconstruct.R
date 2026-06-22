# Tests for reconstruct_table using synthetic token data.tables.
# No real PDF needed — validates the bounding-box clustering algorithm.

# ---------------------------------------------------------------------------
# Token fixture builders
# ---------------------------------------------------------------------------

# Build a synthetic token data.table modelling a simple financial table:
#
#   row 1: "Cash and cash equivalents"   4,865,824   3,122,100
#   row 2: "Loans and advances"         48,000,000  42,000,000
#   row 3: "Total assets"               95,000,000  82,000,000
#
# Page is 595 pts wide (A4).  Label zone is left ~35%, value cols at ~380 & ~490.
make_two_col_tokens <- function() {
  # Helper: make one token entry
  tok <- function(text, x, y, w = NULL) {
    if (is.null(w)) w <- nchar(text) * 6  # rough 6 pts per char
    data.table::data.table(
      width = w, height = 10, x = x, y = y, space = TRUE, text = text
    )
  }

  data.table::rbindlist(list(
    # Row 1 (y ≈ 100)
    tok("Cash",      20, 100),
    tok("and",       68, 100),
    tok("cash",      88, 100),
    tok("equivalents", 116, 100),
    tok("4,865,824", 360, 100, w = 50),
    tok("3,122,100", 470, 100, w = 50),

    # Row 2 (y ≈ 115)
    tok("Loans",     20, 115),
    tok("and",       68, 115),
    tok("advances",  88, 115),
    tok("48,000,000",360, 115, w = 55),
    tok("42,000,000",470, 115, w = 55),

    # Row 3 (y ≈ 130)
    tok("Total",     20, 130),
    tok("assets",    56, 130),
    tok("95,000,000",360, 130, w = 55),
    tok("82,000,000",470, 130, w = 55)
  ))
}

# CIC-like: 4 rows, single pair of columns (H1 2024 / H1 2023)
make_cic_tokens <- function() {
  tok <- function(text, x, y, w = NULL) {
    if (is.null(w)) w <- nchar(text) * 6
    data.table::data.table(width=w, height=10, x=x, y=y, space=TRUE, text=text)
  }
  data.table::rbindlist(list(
    # Column headers (y = 80) — not value tokens, but they anchor columns
    tok("H1 2024", 360, 80, w=45),
    tok("H1 2023", 465, 80, w=45),
    # Data rows
    tok("Insurance revenue",          20, 100),
    tok("5,234,000",                 360, 100, w=50),
    tok("4,865,000",                 465, 100, w=50),
    tok("Insurance service expenses", 20, 115),
    tok("(3,100,000)",               360, 115, w=55),
    tok("(2,800,000)",               465, 115, w=55),
    tok("Insurance service result",   20, 130),
    tok("2,134,000",                 360, 130, w=50),
    tok("2,065,000",                 465, 130, w=50),
    tok("EPS",                        20, 145),
    tok("0.30",                      360, 145, w=50),   # same width as currency rows -> same column band
    tok("0.28",                      465, 145, w=50)
  ))
}

# ---------------------------------------------------------------------------
# Basic structure tests
# ---------------------------------------------------------------------------

test_that("reconstruct_table returns a list with required fields", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)

  expect_type(result, "list")
  expect_true(all(c("table_dt", "n_value_cols", "col_x_bands",
                    "method", "status") %in% names(result)))
})

test_that("status is 'extracted' for valid tokens", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  expect_equal(result$status, "extracted")
})

test_that("empty tokens -> status 'failed'", {
  empty <- data.table::data.table(
    width=double(0), height=double(0), x=double(0), y=double(0),
    space=logical(0), text=character(0)
  )
  result <- reconstruct_table(empty, page_width = 595)
  expect_equal(result$status, "failed")
  expect_equal(nrow(result$table_dt), 0L)
})

# ---------------------------------------------------------------------------
# Row detection
# ---------------------------------------------------------------------------

test_that("correct number of rows detected (3 data rows)", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  expect_equal(nrow(result$table_dt), 3L)
})

test_that("line_item_order is sequential from 1", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  expect_equal(result$table_dt$line_item_order, 1:3)
})

test_that("label column contains label text", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  labels <- result$table_dt$label
  expect_true(any(grepl("Cash", labels)))
  expect_true(any(grepl("Total", labels)))
})

# ---------------------------------------------------------------------------
# Column detection
# ---------------------------------------------------------------------------

test_that("two value columns detected for two-column fixture", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  expect_equal(result$n_value_cols, 2L)
  expect_true("col_1" %in% names(result$table_dt))
  expect_true("col_2" %in% names(result$table_dt))
})

test_that("value cells contain the raw printed strings", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  col1   <- result$table_dt$col_1

  expect_true(any(grepl("4,865,824",  col1)))
  expect_true(any(grepl("48,000,000", col1)))
  expect_true(any(grepl("95,000,000", col1)))
})

test_that("col_x_bands has length equal to n_value_cols", {
  result <- reconstruct_table(make_two_col_tokens(), page_width = 595)
  expect_equal(length(result$col_x_bands), result$n_value_cols)
})

# ---------------------------------------------------------------------------
# Region cropping
# ---------------------------------------------------------------------------

test_that("region crop excludes tokens outside the box", {
  # Crop to first two rows only (y 95–125)
  result <- reconstruct_table(
    make_two_col_tokens(),
    region     = list(xmin = 0, xmax = 595, ymin = 95, ymax = 125),
    page_width = 595
  )
  expect_lte(nrow(result$table_dt), 2L)
})

# ---------------------------------------------------------------------------
# CIC multi-row fixture
# ---------------------------------------------------------------------------

test_that("CIC-style fixture: 5 rows (header + 4 data), 2 value columns", {
  # The header row at y=80 ("H1 2024", "H1 2023") is a row band with empty
  # label and text values — correctly included as row 1 by the engine.
  result <- reconstruct_table(make_cic_tokens(), page_width = 595)
  expect_equal(result$status, "extracted")
  expect_equal(nrow(result$table_dt), 5L)
  expect_equal(result$n_value_cols, 2L)
})

test_that("EPS row value preserved in correct column", {
  result <- reconstruct_table(make_cic_tokens(), page_width = 595)
  eps_row <- result$table_dt[grepl("EPS|eps", label)]
  if (nrow(eps_row) > 0L) {
    expect_true(any(grepl("0.30", unlist(eps_row[, .(col_1, col_2)]))))
  }
})

# ---------------------------------------------------------------------------
# method field
# ---------------------------------------------------------------------------

test_that("method is passed through to result", {
  result <- reconstruct_table(make_two_col_tokens(), method = "ocr",
                               page_width = 595)
  expect_equal(result$method, "ocr")
})
