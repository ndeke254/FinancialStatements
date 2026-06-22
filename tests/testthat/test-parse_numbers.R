# Tests for parse_number / parse_numbers — implements §7.4 table exactly
# Edge cases drawn from the sample corpus (KCB, CIC, ABSA).

# ---------------------------------------------------------------------------
# §7.4 table — exact mapping
# ---------------------------------------------------------------------------

test_that("plain integer with thousands comma -> currency KES'000", {
  r <- parse_number("1,234")
  expect_equal(r$value,      1234)
  expect_equal(r$value_type, "currency")
  expect_equal(r$unit,       "KES'000")
  expect_equal(r$value_text, "1,234")
})

test_that("parenthesised negative -> negative currency (KCB corpus)", {
  r <- parse_number("(10,367,887)")
  expect_equal(r$value,      -10367887)
  expect_equal(r$value_type, "currency")
  expect_equal(r$unit,       "KES'000")
  expect_equal(r$value_text, "(10,367,887)")
})

test_that("dash -> NA (KCB: dashes mean N/A, not 0)", {
  r <- parse_number("-")
  expect_true(is.na(r$value))
  expect_true(is.na(r$value_type))
  expect_true(is.na(r$unit))
  expect_equal(r$value_text, "-")
})

test_that("blank string -> NA", {
  r <- parse_number("")
  expect_true(is.na(r$value))
  expect_true(is.na(r$value_type))
})

test_that("percentage -> 13.1 / percent / %", {
  r <- parse_number("13.1%")
  expect_equal(r$value,      13.1)
  expect_equal(r$value_type, "percent")
  expect_equal(r$unit,       "%")
  expect_equal(r$value_text, "13.1%")
})

test_that("EPS/DPS decimal with is_per_share -> per_share / KES (CIC corpus)", {
  r <- parse_number("0.30", is_per_share = TRUE)
  expect_equal(r$value,      0.30)
  expect_equal(r$value_type, "per_share")
  expect_equal(r$unit,       "KES")
  expect_equal(r$value_text, "0.30")
})

test_that("EPS/DPS without flag -> treated as currency (no false positive)", {
  r <- parse_number("0.30", is_per_share = FALSE)
  expect_equal(r$value_type, "currency")
})

# ---------------------------------------------------------------------------
# Extra corpus edge cases
# ---------------------------------------------------------------------------

test_that("en-dash variant (KCB: dashes = N/A)", {
  r <- parse_number("\u2013")
  expect_true(is.na(r$value))
})

test_that("em-dash variant -> NA", {
  r <- parse_number("\u2014")
  expect_true(is.na(r$value))
})

test_that("large CIC cash figure (4,865,824) parses correctly", {
  r <- parse_number("4,865,824")
  expect_equal(r$value, 4865824)
  expect_equal(r$value_type, "currency")
})

test_that("negative parentheses with decimal", {
  r <- parse_number("(1,234.56)")
  expect_equal(r$value, -1234.56)
  expect_equal(r$value_type, "currency")
})

test_that("percentage with trailing space -> percent", {
  r <- parse_number("25.4% ")
  expect_equal(r$value,      25.4)
  expect_equal(r$value_type, "percent")
  expect_equal(r$unit,       "%")
})

test_that("zero value parses as currency", {
  r <- parse_number("0")
  expect_equal(r$value, 0)
  expect_equal(r$value_type, "currency")
})

test_that("default_unit is respected", {
  r <- parse_number("5,000", default_unit = "USD'000")
  expect_equal(r$unit, "USD'000")
})

test_that("unrecognised text -> NA value", {
  r <- parse_number("abc")
  expect_true(is.na(r$value))
  expect_equal(r$value_text, "abc")
})

test_that("NA input -> NA (not error)", {
  expect_no_error(r <- parse_number(NA))
  expect_true(is.na(r$value))
})

# ---------------------------------------------------------------------------
# Vectorised parse_numbers
# ---------------------------------------------------------------------------

test_that("parse_numbers returns data.table with correct nrow", {
  texts  <- c("1,000", "(500)", "-", "10%", "0.50")
  result <- parse_numbers(texts,
                           is_per_share = c(FALSE, FALSE, FALSE, FALSE, TRUE))

  expect_true(data.table::is.data.table(result))
  expect_equal(nrow(result), 5L)
  expect_equal(result$value,      c(1000, -500, NA, 10, 0.50))
  expect_equal(result$value_type, c("currency", "currency", NA,
                                     "percent", "per_share"))
  expect_equal(result$unit,       c("KES'000", "KES'000", NA, "%", "KES"))
})

test_that("parse_numbers recycles is_per_share scalar", {
  result <- parse_numbers(c("0.25", "0.30"), is_per_share = TRUE)
  expect_true(all(result$value_type == "per_share"))
})

test_that("parse_numbers preserves value_text for audit", {
  texts  <- c("(99,999)", "–", "7.5%")
  result <- parse_numbers(texts)
  expect_equal(result$value_text, texts)
})
