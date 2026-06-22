# Helper: build a minimal valid long data.table
make_long_dt <- function(company        = "KCB Group Plc",
                          statement_type = "balance_sheet",
                          entity         = "KCB Group Consolidated",
                          period_end     = as.Date("2024-03-31"),
                          period_label   = "31 Mar 2024",
                          n_rows         = 3L) {
  data.table::data.table(
    company           = company,
    statement_type    = statement_type,
    fiscal_year       = 2024L,
    entity            = entity,
    period_label      = period_label,
    period_end        = period_end,
    period_type       = "instant",
    audit_status      = "unaudited",
    section           = "Assets",
    line_item         = paste0("Item ", seq_len(n_rows)),
    line_item_order   = seq_len(n_rows),
    row_role          = "line",
    value             = as.double(seq_len(n_rows) * 1000),
    value_text        = paste0(seq_len(n_rows), ",000"),
    value_type        = "currency",
    unit              = "KES'000",
    currency          = "KES",
    source_file       = "KCB_Q1_2024.pdf",
    source_page       = "1",
    extraction_method = "native",
    extraction_status = "confirmed",
    validation_passed = TRUE,
    extracted_at      = as.POSIXct("2024-06-01 10:00:00"),
    edited_by         = "admin"
  )
}

test_that("store_write creates a Parquet file in the correct partition", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    long_dt <- make_long_dt()
    lake_path <- store_write(long_dt, lake_dir, pool)

    part_dir <- file.path(lake_dir, lake_path)
    expect_true(dir.exists(part_dir))
    expect_true(length(list.files(part_dir, pattern = "\\.parquet$")) > 0L)
  })
})

test_that("store_write upserts the catalog entry", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    store_write(make_long_dt(), lake_dir, pool)

    rows <- catalog_list(pool)
    expect_equal(nrow(rows), 1L)
    expect_equal(rows$company[[1L]], "KCB Group Plc")
    expect_equal(rows$n_rows[[1L]], 3L)
  })
})

test_that("store_write overwrites existing rows for same entity+period (FR-SV-02)", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    # First write — 3 line items
    store_write(make_long_dt(n_rows = 3L), lake_dir, pool)

    # Second write — same key but 5 line items
    store_write(make_long_dt(n_rows = 5L), lake_dir, pool)

    part_dir  <- file.path(lake_dir, "company=KCB_Group_Plc",
                            "statement_type=balance_sheet")
    stored_dt <- data.table::as.data.table(
      arrow::read_parquet(file.path(part_dir, "part-0.parquet"))
    )

    # Only the 5 rows from the second write should remain
    expect_equal(nrow(stored_dt), 5L)
  })
})

test_that("store_write preserves rows for other entity/period in same partition", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    dt_a <- make_long_dt(entity = "KCB Kenya",   period_end = as.Date("2024-03-31"),
                          period_label = "Q1 2024", n_rows = 2L)
    dt_b <- make_long_dt(entity = "KCB Tanzania", period_end = as.Date("2024-03-31"),
                          period_label = "Q1 2024", n_rows = 3L)

    store_write(dt_a, lake_dir, pool)
    store_write(dt_b, lake_dir, pool)

    part_dir  <- file.path(lake_dir, "company=KCB_Group_Plc",
                            "statement_type=balance_sheet")
    stored_dt <- data.table::as.data.table(
      arrow::read_parquet(file.path(part_dir, "part-0.parquet"))
    )

    expect_equal(nrow(stored_dt), 5L)   # 2 + 3

    entities <- unique(stored_dt[["entity"]])
    expect_true("KCB Kenya"    %in% entities)
    expect_true("KCB Tanzania" %in% entities)
  })
})

test_that("store_write errors on multi-company data.table", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    bad_dt <- data.table::rbindlist(list(
      make_long_dt(company = "CompA"),
      make_long_dt(company = "CompB")
    ))

    expect_error(store_write(bad_dt, lake_dir, pool), "one company")
  })
})
