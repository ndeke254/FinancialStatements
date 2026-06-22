# Helper (mirrors make_long_dt from test-store_write, kept local)
make_long_dt_read <- function(company        = "KCB Group Plc",
                               statement_type = "balance_sheet",
                               entity         = "KCB Group Consolidated",
                               period_end     = as.Date("2024-03-31"),
                               period_label   = "31 Mar 2024",
                               fiscal_year    = 2024L,
                               audit_status   = "unaudited",
                               n_rows         = 3L) {
  data.table::data.table(
    company           = company,
    statement_type    = statement_type,
    fiscal_year       = fiscal_year,
    entity            = entity,
    period_label      = period_label,
    period_end        = period_end,
    period_type       = "instant",
    audit_status      = audit_status,
    section           = "Assets",
    line_item         = paste0("Item ", seq_len(n_rows)),
    line_item_order   = seq_len(n_rows),
    row_role          = "line",
    value             = as.double(seq_len(n_rows) * 1000),
    value_text        = paste0(seq_len(n_rows), ",000"),
    value_type        = "currency",
    unit              = "KES'000",
    currency          = "KES",
    source_file       = "test.pdf",
    source_page       = "1",
    extraction_method = "native",
    extraction_status = "confirmed",
    validation_passed = TRUE,
    extracted_at      = as.POSIXct("2024-06-01 10:00:00"),
    edited_by         = "admin"
  )
}

# Setup: write some data and return pool + lake_dir
setup_lake <- function(tmpdir) {
  lake_dir <- file.path(tmpdir, "lake")
  pool     <- db_pool_connect(file.path(tmpdir, "c.duckdb"), lake_dir)

  store_write(make_long_dt_read(entity = "KCB Kenya",    fiscal_year = 2024L,
                                 audit_status = "unaudited", n_rows = 3L),
              lake_dir, pool)
  store_write(make_long_dt_read(entity = "KCB Tanzania", fiscal_year = 2024L,
                                 audit_status = "audited",   n_rows = 2L),
              lake_dir, pool)
  store_write(make_long_dt_read(company = "CIC Insurance", statement_type = "income_statement",
                                 entity = "CIC Group", period_end = as.Date("2023-06-30"),
                                 period_label = "H1 2023", fiscal_year = 2023L,
                                 audit_status = "unaudited", n_rows = 4L),
              lake_dir, pool)

  list(pool = pool, lake_dir = lake_dir)
}

test_that("store_read returns all rows for a partition", {
  withr::with_tempdir({
    env <- setup_lake(getwd())
    on.exit(db_pool_disconnect(env$pool), add = TRUE)

    result <- store_read(env$pool, "KCB Group Plc", "balance_sheet")
    expect_equal(nrow(result), 5L)   # 3 + 2
    expect_true(data.table::is.data.table(result))
  })
})

test_that("store_read filters by entity", {
  withr::with_tempdir({
    env <- setup_lake(getwd())
    on.exit(db_pool_disconnect(env$pool), add = TRUE)

    result <- store_read(env$pool, "KCB Group Plc", "balance_sheet",
                         entity = "KCB Kenya")
    expect_equal(nrow(result), 3L)
    expect_true(all(result$entity == "KCB Kenya"))
  })
})

test_that("store_read filters by fiscal_year", {
  withr::with_tempdir({
    env <- setup_lake(getwd())
    on.exit(db_pool_disconnect(env$pool), add = TRUE)

    result <- store_read(env$pool, "KCB Group Plc", "balance_sheet",
                         fiscal_year = 2024L)
    expect_equal(nrow(result), 5L)
  })
})

test_that("store_read respects row_roles filter (FR-AN-04)", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    dt <- make_long_dt_read(n_rows = 3L)
    dt[, row_role := c("line", "subtotal", "total")]
    store_write(dt, lake_dir, pool)

    result <- store_read(pool, "KCB Group Plc", "balance_sheet",
                         row_roles = "line")
    expect_equal(nrow(result), 1L)
    expect_true(all(result$row_role == "line"))
  })
})

test_that("store_read returns zero rows for unknown company", {
  withr::with_tempdir({
    lake_dir <- file.path(getwd(), "lake")
    pool     <- db_pool_connect(file.path(getwd(), "c.duckdb"), lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    result <- store_read(pool, "Nobody Corp", "balance_sheet")
    expect_equal(nrow(result), 0L)
  })
})

test_that("store_read_line_items returns distinct section/line_item/row_role", {
  withr::with_tempdir({
    env <- setup_lake(getwd())
    on.exit(db_pool_disconnect(env$pool), add = TRUE)

    items <- store_read_line_items(env$pool, "KCB Group Plc", "balance_sheet")
    expect_true(data.table::is.data.table(items))
    expect_true(all(c("section", "line_item", "row_role") %in% names(items)))
    expect_true(nrow(items) > 0L)
  })
})
