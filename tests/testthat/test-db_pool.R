test_that("db_pool_connect creates pool and initialises schema", {
  withr::with_tempdir({
    db_path  <- file.path(getwd(), "test.duckdb")
    lake_dir <- file.path(getwd(), "lake")

    pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    expect_true(pool::dbIsValid(pool))

    # All control-plane tables must exist
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)

    tables <- DBI::dbListTables(con)
    expect_true("catalog"            %in% tables)
    expect_true("dim_company"        %in% tables)
    expect_true("dim_entity"         %in% tables)
    expect_true("dim_statement_type" %in% tables)
    expect_true("vocab_audit_status" %in% tables)
    expect_true("dim_user"           %in% tables)
  })
})

test_that("db_init_schema is idempotent — calling twice does not error", {
  withr::with_tempdir({
    db_path  <- file.path(getwd(), "test.duckdb")
    lake_dir <- file.path(getwd(), "lake")

    pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    # Second call should be silent
    expect_no_error(db_init_schema(pool, lake_dir))
  })
})

test_that("v_lake VIEW is created", {
  withr::with_tempdir({
    db_path  <- file.path(getwd(), "test.duckdb")
    lake_dir <- file.path(getwd(), "lake")

    pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)

    # Query the view — should return 0 rows on an empty lake, not error
    result <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM v_lake")
    expect_equal(result$n, 0)
  })
})

test_that("seed vocabularies are loaded", {
  withr::with_tempdir({
    db_path  <- file.path(getwd(), "test.duckdb")
    lake_dir <- file.path(getwd(), "lake")

    pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
    on.exit(db_pool_disconnect(pool), add = TRUE)

    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)

    types <- DBI::dbGetQuery(con, "SELECT name FROM dim_statement_type")
    expect_true("income_statement" %in% types$name)
    expect_true("balance_sheet"    %in% types$name)

    statuses <- DBI::dbGetQuery(con, "SELECT name FROM vocab_audit_status")
    expect_true("audited"   %in% statuses$name)
    expect_true("unaudited" %in% statuses$name)

    users <- DBI::dbGetQuery(con, "SELECT role FROM dim_user")
    expect_true("admin"    %in% users$role)
    expect_true("standard" %in% users$role)
  })
})

test_that("db_pool_disconnect closes the pool cleanly", {
  withr::with_tempdir({
    db_path  <- file.path(getwd(), "test.duckdb")
    lake_dir <- file.path(getwd(), "lake")

    pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
    db_pool_disconnect(pool)

    expect_false(pool::dbIsValid(pool))
  })
})
