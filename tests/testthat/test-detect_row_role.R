# Tests for detect_row_role — keyword, value-context, and corpus edge cases.
# All tests are pure unit tests; no file I/O.

# ---------------------------------------------------------------------------
# Ratio rows
# ---------------------------------------------------------------------------

test_that("EPS label -> ratio (CIC corpus)", {
  expect_equal(detect_row_role("EPS"), "ratio")
})

test_that("DPS label -> ratio", {
  expect_equal(detect_row_role("Dividend per share"), "ratio")
})

test_that("'Earnings per share (basic)' -> ratio", {
  expect_equal(detect_row_role("Earnings per share (basic)"), "ratio")
})

test_that("ratio keyword in label -> ratio", {
  expect_equal(detect_row_role("Capital adequacy ratio"), "ratio")
})

test_that("row with % value cells -> ratio even without keyword", {
  expect_equal(
    detect_row_role("Return on equity", value_texts = c("15.3%", "14.1%")),
    "ratio"
  )
})

# ---------------------------------------------------------------------------
# Total rows
# ---------------------------------------------------------------------------

test_that("'Total assets' -> total (KCB corpus)", {
  expect_equal(detect_row_role("Total assets"), "total")
})

test_that("'Total equity and liabilities' -> total", {
  expect_equal(detect_row_role("Total equity and liabilities",
                                value_texts = c("95,000", "82,000")),
               "total")
})

test_that("'TOTAL' (all caps) -> total", {
  expect_equal(detect_row_role("TOTAL", value_texts = c("1,000")), "total")
})

test_that("bold Total with numeric values -> total", {
  expect_equal(
    detect_row_role("Total", value_texts = c("10,000"), has_bold = TRUE),
    "total"
  )
})

# ---------------------------------------------------------------------------
# Subtotal rows
# ---------------------------------------------------------------------------

test_that("'Net interest income' -> subtotal", {
  expect_equal(detect_row_role("Net interest income",
                                value_texts = c("5,000")), "subtotal")
})

test_that("'Gross profit' -> subtotal", {
  expect_equal(detect_row_role("Gross profit",
                                value_texts = c("20,000")), "subtotal")
})

test_that("'Profit before tax' -> subtotal (KCB P&L)", {
  expect_equal(detect_row_role("Profit before tax",
                                value_texts = c("8,500", "7,200")),
               "subtotal")
})

test_that("'Insurance service result' -> subtotal (CIC corpus)", {
  expect_equal(detect_row_role("Insurance service result",
                                value_texts = c("1,200")),
               "subtotal")
})

test_that("'Operating profit/(loss)' -> subtotal", {
  expect_equal(detect_row_role("Operating profit/(loss)",
                                value_texts = c("3,000")),
               "subtotal")
})

# ---------------------------------------------------------------------------
# Header rows
# ---------------------------------------------------------------------------

test_that("label with no numeric values -> header", {
  expect_equal(
    detect_row_role("ASSETS", value_texts = c("", "-", NA)),
    "header"
  )
})

test_that("blank label with no values -> header", {
  expect_equal(detect_row_role("", value_texts = character(0)), "header")
})

test_that("NULL value_texts -> header (no data context)", {
  expect_equal(detect_row_role("Cash and cash equivalents",
                                value_texts = NULL),
               "header")
})

test_that("ALL CAPS section label -> header even with some values", {
  # Not a great real-world case but tests the regex path
  expect_equal(detect_row_role("ASSETS"), "header")
})

# ---------------------------------------------------------------------------
# Line rows (default)
# ---------------------------------------------------------------------------

test_that("plain balance-sheet line item -> line", {
  expect_equal(
    detect_row_role("Cash and cash equivalents",
                    value_texts = c("4,865,824", "3,122,100")),
    "line"
  )
})

test_that("numbered item '1. Loans and advances' -> line", {
  expect_equal(
    detect_row_role("1. Loans and advances",
                    value_texts = c("500,000")),
    "line"
  )
})

test_that("indented item with numeric -> line", {
  expect_equal(
    detect_row_role("  Government securities",
                    value_texts = c("12,000", "10,500"),
                    indentation = 2L),
    "line"
  )
})

# ---------------------------------------------------------------------------
# detect_row_roles (vectorised) over a data.table
# ---------------------------------------------------------------------------

test_that("detect_row_roles adds row_role column to data.table", {
  dt <- data.table::data.table(
    label    = c("ASSETS", "Cash", "Total assets", "EPS"),
    col_1    = c("",       "500",  "5,000",        "0.50"),
    col_2    = c("",       "400",  "4,500",        "0.45")
  )

  detect_row_roles(dt, value_cols = c("col_1", "col_2"))

  expect_true("row_role" %in% names(dt))
  expect_equal(dt$row_role, c("header", "line", "total", "ratio"))
})
