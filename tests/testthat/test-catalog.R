test_that("catalog_upsert inserts a new row", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    id <- catalog_upsert(
      pool             = pool,
      company          = "KCB Group Plc",
      statement_type   = "balance_sheet",
      entity           = "KCB Group Consolidated",
      period_end       = "2024-03-31",
      audit_status     = "unaudited",
      source_file      = "KCB_Q1_2024.pdf",
      validation_passed = TRUE,
      n_rows           = 42L,
      extracted_at     = "2024-06-01 10:00:00",
      lake_path        = "company=KCB_Group_Plc/statement_type=balance_sheet"
    )

    expect_true(is.numeric(id))
    expect_true(id >= 1L)
  })
})

test_that("catalog_upsert overwrites on same key (no versioning — FR-SV-02)", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    key_args <- list(
      pool           = pool,
      company        = "CIC Insurance",
      statement_type = "income_statement",
      entity         = "CIC Group",
      period_end     = "2024-06-30",
      lake_path      = "company=CIC_Insurance/statement_type=income_statement"
    )

    do.call(catalog_upsert, c(key_args, list(n_rows = 10L)))
    do.call(catalog_upsert, c(key_args, list(n_rows = 15L)))  # overwrite

    rows <- catalog_list(pool, company = "CIC Insurance")
    expect_equal(nrow(rows), 1L)                 # still one row
    expect_equal(rows$n_rows[[1L]], 15L)         # updated value
  })
})

test_that("catalog_list returns all rows when no filters supplied", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    catalog_upsert(pool, "CompA", "balance_sheet", "Entity1", "2023-12-31",
                   lake_path = "x")
    catalog_upsert(pool, "CompB", "income_statement", "Entity2", "2023-12-31",
                   lake_path = "y")

    rows <- catalog_list(pool)
    expect_equal(nrow(rows), 2L)
  })
})

test_that("catalog_list filters by company", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    catalog_upsert(pool, "CompA", "balance_sheet", "Ent1", "2023-12-31",
                   lake_path = "x")
    catalog_upsert(pool, "CompB", "income_statement", "Ent2", "2023-12-31",
                   lake_path = "y")

    rows <- catalog_list(pool, company = "CompA")
    expect_equal(nrow(rows), 1L)
    expect_equal(rows$company[[1L]], "CompA")
  })
})

test_that("catalog_list filters by fiscal_year", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    catalog_upsert(pool, "Comp", "balance_sheet", "Ent", "2022-12-31",
                   lake_path = "a")
    catalog_upsert(pool, "Comp", "balance_sheet", "Ent", "2023-12-31",
                   lake_path = "b")

    rows <- catalog_list(pool, fiscal_year = 2022L)
    expect_equal(nrow(rows), 1L)
  })
})

test_that("catalog_get returns the right row", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    catalog_upsert(pool, "KCB Group Plc", "cash_flow", "KCB Kenya",
                   "2024-03-31", audit_status = "reviewed",
                   lake_path = "p")

    row <- catalog_get(pool, "KCB Group Plc", "cash_flow",
                       "KCB Kenya", "2024-03-31")

    expect_equal(nrow(row), 1L)
    expect_equal(row$audit_status[[1L]], "reviewed")
  })
})

test_that("catalog_get returns zero rows for unknown key", {
  withr::with_tempdir({
    pool <- db_pool_connect(file.path(getwd(), "c.duckdb"),
                            file.path(getwd(), "lake"))
    on.exit(db_pool_disconnect(pool), add = TRUE)

    row <- catalog_get(pool, "Nobody", "balance_sheet", "Ent", "2024-01-01")
    expect_equal(nrow(row), 0L)
  })
})
