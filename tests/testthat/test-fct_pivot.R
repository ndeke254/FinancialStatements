# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Two-column long table: KCB Kenya Q1 2024 + KCB Tanzania Q1 2024
# Three line items, mixed row_role, mixed value_type (KCB corpus edge cases)
make_fixture_long <- function() {
  data.table::data.table(
    section         = c("Assets", "Assets", "Assets",
                        "Assets", "Assets", "Assets"),
    line_item       = c("Cash", "Total assets", "EPS",
                        "Cash", "Total assets", "EPS"),
    line_item_order = c(1L, 2L, 3L, 1L, 2L, 3L),
    row_role        = c("line", "total", "ratio",
                        "line", "total", "ratio"),
    value_type      = c("currency", "currency", "per_share",
                        "currency", "currency", "per_share"),
    unit            = c("KES'000", "KES'000", "KES",
                        "KES'000", "KES'000", "KES"),
    currency        = "KES",
    entity          = c("KCB Kenya", "KCB Kenya", "KCB Kenya",
                        "KCB Tanzania", "KCB Tanzania", "KCB Tanzania"),
    period_label    = c("31 Mar 2024", "31 Mar 2024", "31 Mar 2024",
                        "31 Mar 2024", "31 Mar 2024", "31 Mar 2024"),
    period_end      = as.Date(c("2024-03-31", "2024-03-31", "2024-03-31",
                                "2024-03-31", "2024-03-31", "2024-03-31")),
    period_type     = "instant",
    audit_status    = c("unaudited", "unaudited", "unaudited",
                        "audited", "audited", "audited"),
    value           = c(4865824, 95000000, 5.30,
                        1200000, 40000000, 2.10),
    value_text      = c("4,865,824", "95,000,000", "5.30",
                        "1,200,000", "40,000,000", "2.10")
  )
}

# ---------------------------------------------------------------------------
# long_to_wide tests
# ---------------------------------------------------------------------------

test_that("long_to_wide produces one column per entity__period_label", {
  long <- make_fixture_long()
  wide <- long_to_wide(long)

  expect_true(data.table::is.data.table(wide))
  expect_true("KCB Kenya__31 Mar 2024"    %in% names(wide))
  expect_true("KCB Tanzania__31 Mar 2024" %in% names(wide))
})

test_that("long_to_wide includes _text__ columns for every value column", {
  wide <- long_to_wide(make_fixture_long())

  expect_true("_text__KCB Kenya__31 Mar 2024"    %in% names(wide))
  expect_true("_text__KCB Tanzania__31 Mar 2024" %in% names(wide))
})

test_that("long_to_wide attaches col_meta attribute with per-column metadata", {
  wide     <- long_to_wide(make_fixture_long())
  col_meta <- attr(wide, "col_meta")

  expect_true(data.table::is.data.table(col_meta))
  expect_true(all(c("col_key", "entity", "period_label",
                     "period_end", "period_type", "audit_status") %in%
                   names(col_meta)))
  expect_equal(nrow(col_meta), 2L)   # one per entity__period_label
})

test_that("long_to_wide preserves row order by line_item_order", {
  wide <- long_to_wide(make_fixture_long())
  expect_equal(wide$line_item_order, c(1L, 2L, 3L))
})

test_that("long_to_wide: EPS row has per_share value_type in wide form", {
  wide <- long_to_wide(make_fixture_long())
  eps_row <- wide[line_item == "EPS"]
  expect_equal(eps_row$value_type, "per_share")
  expect_equal(eps_row$unit, "KES")
})

test_that("long_to_wide: dashes (NA values) round-trip correctly", {
  long <- make_fixture_long()
  long[entity == "KCB Tanzania" & line_item == "Cash",
       `:=`(value = NA_real_, value_text = "-")]

  wide <- long_to_wide(long)
  na_val <- wide[line_item == "Cash"][["KCB Tanzania__31 Mar 2024"]]
  na_txt <- wide[line_item == "Cash"][["_text__KCB Tanzania__31 Mar 2024"]]

  expect_true(is.na(na_val))
  expect_equal(na_txt, "-")
})

# ---------------------------------------------------------------------------
# wide_to_long tests
# ---------------------------------------------------------------------------

test_that("wide_to_long round-trips long_to_wide losslessly", {
  long <- make_fixture_long()
  wide <- long_to_wide(long)

  long2 <- wide_to_long(
    wide_dt     = wide,
    filing_meta = list(
      company           = "KCB Group Plc",
      statement_type    = "balance_sheet",
      fiscal_year       = 2024L,
      currency          = "KES",
      source_file       = "test.pdf",
      source_page       = "1",
      extraction_method = "native",
      extraction_status = "confirmed",
      extracted_at      = as.POSIXct("2024-06-01 10:00:00"),
      edited_by         = "admin"
    )
  )

  # Same row count as original
  expect_equal(nrow(long2), nrow(long))

  # Values are preserved (join on entity + period_end + line_item)
  data.table::setkeyv(long,  c("entity", "period_end", "line_item"))
  data.table::setkeyv(long2, c("entity", "period_end", "line_item"))

  expect_equal(long2$value,      long$value)
  expect_equal(long2$value_text, long$value_text)
  expect_equal(long2$value_type, long$value_type)
  expect_equal(long2$unit,       long$unit)
})

test_that("wide_to_long attaches filing-level metadata", {
  wide  <- long_to_wide(make_fixture_long())
  long2 <- wide_to_long(wide, filing_meta = list(
    company        = "KCB Group Plc",
    statement_type = "balance_sheet",
    fiscal_year    = 2024L,
    lake_path      = "p"
  ))

  expect_true(all(long2$company        == "KCB Group Plc"))
  expect_true(all(long2$statement_type == "balance_sheet"))
  expect_true(all(long2$fiscal_year    == 2024L))
})

test_that("wide_to_long errors when col_meta is missing", {
  wide <- long_to_wide(make_fixture_long())
  data.table::setattr(wide, "col_meta", NULL)   # strip attribute

  expect_error(wide_to_long(wide), "col_meta")
})

test_that("wide_to_long: extraction_status defaults to 'confirmed'", {
  wide  <- long_to_wide(make_fixture_long())
  long2 <- wide_to_long(wide)

  expect_true(all(long2$extraction_status == "confirmed"))
})

test_that("wide_to_long output has all required long columns", {
  wide  <- long_to_wide(make_fixture_long())
  long2 <- wide_to_long(wide)

  required <- c("entity", "period_label", "period_end", "period_type",
                 "audit_status", "section", "line_item", "line_item_order",
                 "row_role", "value", "value_text", "value_type", "unit",
                 "currency")
  expect_true(all(required %in% names(long2)))
})
