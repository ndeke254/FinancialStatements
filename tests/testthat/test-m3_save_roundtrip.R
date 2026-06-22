# M3 save round-trip test.
#
# Tests the full pipeline:
#   build_long_for_save() -> store_write() -> store_read()
# using a synthetic CIC income-statement-style fixture.
#
# Also tests overwrite semantics: saving the same slice twice leaves one entry.

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

make_cic_wide_dt <- function() {
  # Two value columns mirroring CIC H1 income statement extract:
  #   col_1 = H1 2024 (30 Jun 2024), col_2 = H1 2023 (30 Jun 2023)
  data.table::data.table(
    section          = c("",                       "",                          "",            ""),
    line_item        = c("Insurance revenue",       "Insurance service expenses", "Insurance service result", "EPS"),
    line_item_order  = 1:4,
    row_role         = c("line", "line", "subtotal", "ratio"),
    value_type       = c("currency", "currency", "currency", "per_share"),
    unit             = c("KES'000", "KES'000", "KES'000", "KES"),
    currency         = c("KES", "KES", "KES", "KES"),
    col_1            = c("5,234,000", "(3,100,000)", "2,134,000", "0.30"),
    col_2            = c("4,865,000", "(2,800,000)", "2,065,000", "0.28")
  )
}

make_cic_col_meta <- function() {
  data.table::data.table(
    col_key      = c("CIC Group__H1 2024", "CIC Group__H1 2023"),
    entity       = c("CIC Group", "CIC Group"),
    period_label = c("H1 2024", "H1 2023"),
    period_end   = as.Date(c("2024-06-30", "2023-06-30")),
    period_type  = c("duration_half_year", "duration_half_year"),
    audit_status = c("unaudited", "audited")
  )
}

make_cic_filing_meta <- function() {
  list(
    company           = "CIC Insurance Group",
    statement_type    = "income_statement",
    fiscal_year       = 2024L,
    currency          = "KES",
    source_file       = "CIC_H1_2024.pdf",
    source_page       = "1",
    extraction_method = "native",
    extraction_status = "confirmed",
    validation_passed = NA,
    extracted_at      = as.POSIXct("2026-06-22 10:00:00", tz = "UTC"),
    edited_by         = "admin"
  )
}

# ---------------------------------------------------------------------------
# build_long_for_save — unit tests
# ---------------------------------------------------------------------------

test_that("build_long_for_save returns a data.table with required columns", {
  long_dt <- build_long_for_save(
    make_cic_wide_dt(),
    make_cic_col_meta(),
    make_cic_filing_meta()
  )
  expect_true(data.table::is.data.table(long_dt))
  expect_true(all(c("company", "statement_type", "entity", "period_end",
                    "line_item", "value", "value_text", "value_type",
                    "unit", "row_role") %in% names(long_dt)))
})

test_that("build_long_for_save produces correct row count (4 rows x 2 cols = 8)", {
  long_dt <- build_long_for_save(
    make_cic_wide_dt(),
    make_cic_col_meta(),
    make_cic_filing_meta()
  )
  expect_equal(nrow(long_dt), 8L)
})

test_that("build_long_for_save preserves raw value_text", {
  long_dt <- build_long_for_save(
    make_cic_wide_dt(),
    make_cic_col_meta(),
    make_cic_filing_meta()
  )
  texts <- long_dt$value_text
  expect_true("5,234,000"   %in% texts)
  expect_true("(3,100,000)" %in% texts)
  expect_true("-"           %in% texts | "(2,800,000)" %in% texts)
})

test_that("build_long_for_save parses numeric values correctly", {
  long_dt <- build_long_for_save(
    make_cic_wide_dt(),
    make_cic_col_meta(),
    make_cic_filing_meta()
  )
  rev_row <- long_dt[line_item == "Insurance revenue" & entity == "CIC Group" &
                     period_end == as.Date("2024-06-30")]
  expect_equal(rev_row$value, 5234000)
  exp_row <- long_dt[line_item == "Insurance service expenses" &
                     period_end == as.Date("2024-06-30")]
  expect_equal(exp_row$value, -3100000)
})

test_that("build_long_for_save applies per_share for EPS row (FR-ED-06)", {
  long_dt <- build_long_for_save(
    make_cic_wide_dt(),
    make_cic_col_meta(),
    make_cic_filing_meta()
  )
  eps_rows <- long_dt[grepl("(?i)eps", line_item, perl = TRUE)]
  expect_true(nrow(eps_rows) > 0L)
  expect_true(all(eps_rows$value_type == "per_share"))
  expect_true(all(eps_rows$unit == "KES"))
  expect_equal(eps_rows[period_end == as.Date("2024-06-30")]$value, 0.30)
})

test_that("build_long_for_save sets entity and period metadata from col_meta", {
  long_dt <- build_long_for_save(
    make_cic_wide_dt(),
    make_cic_col_meta(),
    make_cic_filing_meta()
  )
  entities <- unique(long_dt$entity)
  expect_true("CIC Group" %in% entities)
  periods <- unique(long_dt$period_end)
  expect_true(as.Date("2024-06-30") %in% periods)
  expect_true(as.Date("2023-06-30") %in% periods)
})

test_that("build_long_for_save errors when col_meta row count != value col count", {
  bad_meta <- make_cic_col_meta()[1L]   # only 1 row, but wide_dt has 2 cols
  expect_error(
    build_long_for_save(make_cic_wide_dt(), bad_meta, make_cic_filing_meta()),
    "col_meta"
  )
})

# ---------------------------------------------------------------------------
# Full round-trip: store_write -> store_read
# ---------------------------------------------------------------------------

test_that("store_write + store_read round-trip preserves all rows", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    db_path  <- file.path(getwd(), "ctrl.duckdb")
    pool     <- db_pool_connect(db_path, lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    long_dt <- build_long_for_save(
      make_cic_wide_dt(),
      make_cic_col_meta(),
      make_cic_filing_meta()
    )
    store_write(long_dt, lake_dir = lake_dir, pool = pool, edited_by = "admin")

    back <- store_read(pool,
      company        = "CIC Insurance Group",
      statement_type = "income_statement"
    )
    expect_equal(nrow(back), 8L)
    expect_true("Insurance revenue" %in% back$line_item)
  })
})

test_that("overwrite: saving the same slice twice leaves exactly one entry", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    db_path  <- file.path(getwd(), "ctrl.duckdb")
    pool     <- db_pool_connect(db_path, lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    long_dt <- build_long_for_save(
      make_cic_wide_dt(),
      make_cic_col_meta(),
      make_cic_filing_meta()
    )
    # Save twice
    store_write(long_dt, lake_dir = lake_dir, pool = pool, edited_by = "admin")
    store_write(long_dt, lake_dir = lake_dir, pool = pool, edited_by = "admin")

    back <- store_read(pool,
      company        = "CIC Insurance Group",
      statement_type = "income_statement"
    )
    # Must be 8, not 16
    expect_equal(nrow(back), 8L)

    # Catalog also has exactly one entry per entity+period
    cat_entries <- catalog_list(pool, company = "CIC Insurance Group")
    expect_equal(nrow(cat_entries), 2L)   # 2 periods (H1 2024 + H1 2023)
  })
})

test_that("round-trip preserves EPS as per_share/KES", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    db_path  <- file.path(getwd(), "ctrl.duckdb")
    pool     <- db_pool_connect(db_path, lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    long_dt <- build_long_for_save(
      make_cic_wide_dt(),
      make_cic_col_meta(),
      make_cic_filing_meta()
    )
    store_write(long_dt, lake_dir = lake_dir, pool = pool, edited_by = "admin")

    back <- store_read(pool,
      company        = "CIC Insurance Group",
      statement_type = "income_statement",
      row_roles      = "ratio"
    )
    expect_true(nrow(back) > 0L)
    expect_true(all(back$value_type == "per_share"))
    expect_true(all(back$unit == "KES"))
  })
})
