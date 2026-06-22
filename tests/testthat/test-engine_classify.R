# Tests for classify_tokens (pure unit tests — no real PDF needed).
# classify_page is tested separately via mocking when pdftools is available.

# ---------------------------------------------------------------------------
# classify_tokens — the testable core logic
# ---------------------------------------------------------------------------

# Helper: build a synthetic token data.frame
make_tokens <- function(texts) {
  data.frame(
    width  = rep(20, length(texts)),
    height = rep(10, length(texts)),
    x      = seq_len(length(texts)) * 25,
    y      = rep(100, length(texts)),
    space  = TRUE,
    text   = texts,
    stringsAsFactors = FALSE
  )
}

test_that("NULL tokens -> scanned", {
  expect_equal(classify_tokens(NULL), "scanned")
})

test_that("zero-row data.frame -> scanned (ABSA-like empty page)", {
  expect_equal(classify_tokens(data.frame(text = character(0))), "scanned")
})

test_that("very few tokens -> scanned (watermark / near-empty page)", {
  sparse <- make_tokens(c("CONFIDENTIAL", "2024"))   # 2 tokens
  expect_equal(classify_tokens(sparse), "scanned")
})

test_that("dense token table -> native", {
  # 20 meaningful tokens covering a typical financial table row
  texts <- c(
    "Cash", "and", "cash", "equivalents",
    "4,865,824", "3,122,100",
    "Loans", "and", "advances",
    "48,000,000", "42,000,000",
    "Total", "assets",
    "95,000,000", "82,000,000",
    "Total", "equity",
    "28,000,000", "25,000,000",
    "EPS"
  )
  expect_equal(classify_tokens(make_tokens(texts)), "native")
})

test_that("tokens with only whitespace -> scanned", {
  blank <- make_tokens(c("   ", "\t", "  "))
  expect_equal(classify_tokens(blank), "scanned")
})

test_that("exactly at threshold is native", {
  # .NATIVE_MIN_TOKENS = 15, .NATIVE_MIN_CHARS = 40
  # 15 single-char tokens = 15 chars — not enough chars even if enough tokens
  texts_short <- rep("a", 15)
  # 15 tokens but only 15 chars — still scanned because < 40 chars
  expect_equal(classify_tokens(make_tokens(texts_short)), "scanned")

  # 15 tokens with sufficient characters
  texts_long <- c(rep("CashEquiv", 5), rep("4,865,824", 5), rep("Total", 5))
  # 15 tokens, 5*9 + 5*9 + 5*5 = 115 chars — should be native
  expect_equal(classify_tokens(make_tokens(texts_long)), "native")
})

# ---------------------------------------------------------------------------
# classify_page — integration test (skipped without real PDF)
# ---------------------------------------------------------------------------

test_that("classify_page errors on non-existent file", {
  expect_error(classify_page("/tmp/does_not_exist_fin_extract.pdf", 1L))
})
