# Tests for engine_native.R — strategy dispatcher and stub responses.
# engine_native_extract tests require a real PDF and are skipped without one.

# ---------------------------------------------------------------------------
# Strategy dispatcher — extract()
# ---------------------------------------------------------------------------

test_that("extract() with method='ai' returns not_available (FR-EX-04)", {
  # No real PDF needed — just checking the stub returns the right status
  # We use a temp file path to get past the file.exists() check
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("dummy", f)  # not a real PDF — engine_ai stub doesn't read it
    result <- suppressMessages(extract(f, page = 1L, method = "ai"))
    expect_equal(result$status, "not_available")
    expect_equal(result$method, "ai")
  })
})

test_that("extract() with method='ocr' returns not_available (M4 stub)", {
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("dummy", f)
    result <- suppressMessages(extract(f, page = 1L, method = "ocr"))
    expect_equal(result$status, "not_available")
    expect_equal(result$method, "ocr")
  })
})

test_that("extract() attaches page and region to result (FR-EX-07)", {
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("dummy", f)
    rgn <- list(xmin = 0, xmax = 400, ymin = 50, ymax = 300)
    result <- suppressMessages(extract(f, page = 2L, region = rgn, method = "ai"))
    expect_equal(result$page,   2L)
    expect_equal(result$region, rgn)
  })
})

test_that("extract() with unknown method errors", {
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("dummy", f)
    expect_error(extract(f, 1L, method = "magic"), "Unknown extraction method")
  })
})

test_that("extract() errors on non-existent file", {
  expect_error(extract("/tmp/does_not_exist_fin.pdf", 1L))
})

# ---------------------------------------------------------------------------
# Stub shape tests — OCR and AI stubs return correct structure
# ---------------------------------------------------------------------------

test_that("engine_ocr_extract has correct result shape", {
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("dummy", f)
    result <- suppressMessages(engine_ocr_extract(f, 1L))
    expect_true(all(c("table_dt", "n_value_cols", "col_x_bands",
                      "method", "status") %in% names(result)))
    expect_equal(result$method, "ocr")
    expect_equal(result$status, "not_available")
  })
})

test_that("engine_ai_extract has correct result shape", {
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("dummy", f)
    result <- suppressMessages(engine_ai_extract(f, 1L))
    expect_true(all(c("table_dt", "n_value_cols", "col_x_bands",
                      "method", "status") %in% names(result)))
    expect_equal(result$method, "ai")
  })
})

# ---------------------------------------------------------------------------
# engine_native_extract — skipped without a real PDF fixture
# ---------------------------------------------------------------------------

test_that("engine_native_extract: fails gracefully on unreadable file", {
  withr::with_tempfile("f", fileext = ".pdf", {
    writeLines("not a pdf", f)   # malformed — pdftools will error
    result <- engine_native_extract(f, 1L)
    # Should return failed status, not throw
    expect_equal(result$status, "failed")
    expect_equal(nrow(result$table_dt), 0L)
  })
})

# Real-PDF tests: add fixtures to tests/testthat/fixtures/ and un-skip.
test_that("engine_native_extract works on CIC fixture", {
  skip("Requires CIC fixture PDF — add to tests/testthat/fixtures/")
  path   <- testthat::test_path("fixtures", "CIC_H1_2024.pdf")
  result <- engine_native_extract(path, page = 1L)
  expect_equal(result$status, "extracted")
  expect_gte(result$n_value_cols, 2L)
})

test_that("engine_native_extract works on KCB fixture (12 columns)", {
  skip("Requires KCB fixture PDF — add to tests/testthat/fixtures/")
  path   <- testthat::test_path("fixtures", "KCB_Q1_2024.pdf")
  result <- engine_native_extract(path, page = 1L)
  expect_equal(result$status, "extracted")
  expect_gte(result$n_value_cols, 10L)   # KCB has 12 value columns
})
