# M3 role-gate tests (FR-UP-00, §3).
#
# Verifies that:
# 1. is_admin / require_admin enforce the admin-only gate.
# 2. get_user_role resolves roles correctly from dim_user.
# 3. build_long_for_save is accessible and exportable (smoke test).

# ---------------------------------------------------------------------------
# is_admin / require_admin — no pool needed
# ---------------------------------------------------------------------------

test_that("is_admin('admin') returns TRUE", {
  expect_true(is_admin("admin"))
})

test_that("is_admin('standard') returns FALSE", {
  expect_false(is_admin("standard"))
})

test_that("is_admin(NA) returns FALSE", {
  expect_false(is_admin(NA_character_))
})

test_that("is_admin(NULL) returns FALSE", {
  expect_false(is_admin(NULL))
})

test_that("require_admin stops for standard role", {
  expect_error(require_admin("standard"), "admin role required")
})

test_that("require_admin stops for NA role", {
  expect_error(require_admin(NA_character_), "admin role required")
})

test_that("require_admin returns invisibly TRUE for admin", {
  expect_invisible(require_admin("admin"))
  expect_true(require_admin("admin"))
})

# ---------------------------------------------------------------------------
# get_user_role — requires a live pool (uses temp DuckDB)
# ---------------------------------------------------------------------------

test_that("get_user_role returns 'admin' for seeded admin user", {
  withr::with_tempdir({
    pool <- db_pool_connect(
      db_path  = file.path(getwd(), "ctrl.duckdb"),
      lake_dir = file.path(getwd(), "lake")
    )
    on.exit(db_pool_disconnect(pool), add = TRUE)

    role <- get_user_role(pool, "admin")
    expect_equal(role, "admin")
  })
})

test_that("get_user_role returns 'standard' for seeded viewer user", {
  withr::with_tempdir({
    pool <- db_pool_connect(
      db_path  = file.path(getwd(), "ctrl.duckdb"),
      lake_dir = file.path(getwd(), "lake")
    )
    on.exit(db_pool_disconnect(pool), add = TRUE)

    role <- get_user_role(pool, "viewer")
    expect_equal(role, "standard")
  })
})

test_that("get_user_role returns NA for unknown user", {
  withr::with_tempdir({
    pool <- db_pool_connect(
      db_path  = file.path(getwd(), "ctrl.duckdb"),
      lake_dir = file.path(getwd(), "lake")
    )
    on.exit(db_pool_disconnect(pool), add = TRUE)

    role <- get_user_role(pool, "nonexistent_xyz")
    expect_true(is.na(role))
  })
})

test_that("get_user_role returns NA for NULL user_id", {
  withr::with_tempdir({
    pool <- db_pool_connect(
      db_path  = file.path(getwd(), "ctrl.duckdb"),
      lake_dir = file.path(getwd(), "lake")
    )
    on.exit(db_pool_disconnect(pool), add = TRUE)

    expect_true(is.na(get_user_role(pool, NULL)))
    expect_true(is.na(get_user_role(pool, "")))
    expect_true(is.na(get_user_role(pool, NA_character_)))
  })
})

# ---------------------------------------------------------------------------
# build_long_for_save — role-independent, but part of the admin save path
# ---------------------------------------------------------------------------

test_that("build_long_for_save rejects non-data.table wide_dt", {
  expect_error(
    build_long_for_save(
      data.frame(col_1 = "1,000"),
      data.table::data.table(col_key = "e__p", entity = "e",
                             period_label = "p", period_end = Sys.Date(),
                             period_type = "instant", audit_status = "unaudited"),
      list()
    )
  )
})

test_that("build_long_for_save basic smoke: 1 row x 1 col -> 1 long row", {
  wide <- data.table::data.table(
    section         = "",
    line_item       = "Cash",
    line_item_order = 1L,
    row_role        = "line",
    value_type      = "currency",
    unit            = "KES'000",
    currency        = "KES",
    col_1           = "5,000"
  )
  cm <- data.table::data.table(
    col_key      = "Consolidated__Dec 2023",
    entity       = "Consolidated",
    period_label = "Dec 2023",
    period_end   = as.Date("2023-12-31"),
    period_type  = "instant",
    audit_status = "audited"
  )
  fm <- list(
    company = "Test Co", statement_type = "balance_sheet",
    fiscal_year = 2023L, currency = "KES",
    source_file = "test.pdf", source_page = "1",
    extraction_method = "native", extraction_status = "confirmed",
    validation_passed = NA, extracted_at = Sys.time(), edited_by = "admin"
  )
  long_dt <- build_long_for_save(wide, cm, fm)
  expect_equal(nrow(long_dt), 1L)
  expect_equal(long_dt$value, 5000)
  expect_equal(long_dt$value_text, "5,000")
  expect_equal(long_dt$entity, "Consolidated")
})

# ---------------------------------------------------------------------------
# .guess_from_filename — FR-UP-04 filename-is-untrusted helper
# ---------------------------------------------------------------------------

test_that(".guess_from_filename extracts year from filename", {
  g <- .guess_from_filename("KCB_2024_Q1.pdf")
  expect_equal(g$period, "2024")
})

test_that(".guess_from_filename extracts company hint from filename", {
  g <- .guess_from_filename("CIC_H1_2024.pdf")
  expect_equal(g$company, "CIC")
})

test_that(".guess_from_filename returns NA company when filename starts with digit", {
  g <- .guess_from_filename("2024_report.pdf")
  expect_true(is.na(g$company))
})
