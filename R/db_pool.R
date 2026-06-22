#' DuckDB connection pool management
#'
#' All DuckDB access goes through a single `pool` object (maxSize = 1, which
#' preserves the single-writer guarantee). Never open ad-hoc DuckDB connections
#' outside this module.
#'
#' @name db_pool
NULL

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Open the DuckDB pool and initialise the schema
#'
#' Call once at app start (in `app_server`). The returned pool must be closed
#' with [db_pool_disconnect()] on session end.
#'
#' @param db_path  Path to the DuckDB file (created if absent).
#' @param lake_dir Path to the Parquet lake root (created if absent).
#' @return A `pool` object.
#' @export
db_pool_connect <- function(db_path, lake_dir) {
  db_path <- normalizePath(db_path, mustWork = FALSE)
  lake_dir <- normalizePath(lake_dir, mustWork = FALSE)

  if (!dir.exists(dirname(db_path))) {
    dir.create(dirname(db_path), recursive = TRUE)
  }
  if (!dir.exists(lake_dir)) {
    dir.create(lake_dir, recursive = TRUE)
  }

  pool <- pool::dbPool(
    drv = duckdb::duckdb(),
    dbname = db_path,
    maxSize = 1L # single-writer guarantee
  )

  db_init_schema(pool, lake_dir)
  pool
}

#' Close the DuckDB pool
#'
#' @param pool A pool object returned by [db_pool_connect()].
#' @return Invisibly `NULL`.
#' @export
db_pool_disconnect <- function(pool) {
  pool::poolClose(pool)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Internal — schema bootstrap
# ---------------------------------------------------------------------------

#' Initialise DuckDB schema (idempotent)
#'
#' Creates all control-plane tables and the lake VIEW if they do not already
#' exist, then seeds vocabularies from the package's `extdata/seed/` CSVs.
#'
#' @param pool     A `pool` object.
#' @param lake_dir Absolute path to the Parquet lake root.
#' @keywords internal
db_init_schema <- function(pool, lake_dir) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  DBI::dbWithTransaction(con, {
    # -- Dimension tables -------------------------------------------------------
    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS dim_company (
        company_id   INTEGER PRIMARY KEY,
        group_name   VARCHAR NOT NULL UNIQUE,
        country      VARCHAR DEFAULT 'Kenya',
        created_at   TIMESTAMPTZ DEFAULT now()
      )
    "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS dim_entity (
        entity_id        INTEGER PRIMARY KEY,
        company_id       INTEGER NOT NULL REFERENCES dim_company(company_id),
        entity_name      VARCHAR NOT NULL,
        is_consolidated  BOOLEAN DEFAULT TRUE,
        UNIQUE (company_id, entity_name)
      )
    "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS dim_statement_type (
        type_id    INTEGER PRIMARY KEY,
        name       VARCHAR NOT NULL UNIQUE,
        is_builtin BOOLEAN DEFAULT FALSE
      )
    "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS vocab_audit_status (
        status_id  INTEGER PRIMARY KEY,
        name       VARCHAR NOT NULL UNIQUE,
        is_builtin BOOLEAN DEFAULT FALSE
      )
    "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS dim_user (
        user_id    INTEGER PRIMARY KEY,
        name       VARCHAR NOT NULL UNIQUE,
        role       VARCHAR NOT NULL CHECK (role IN ('standard', 'admin')),
        created_at TIMESTAMPTZ DEFAULT now()
      )
    "
    )

    # -- Catalog ----------------------------------------------------------------
    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS catalog (
        catalog_id       INTEGER PRIMARY KEY,
        company          VARCHAR NOT NULL,
        statement_type   VARCHAR NOT NULL,
        entity           VARCHAR NOT NULL,
        period_end       DATE    NOT NULL,
        audit_status     VARCHAR,
        source_file      VARCHAR,
        validation_passed BOOLEAN,
        n_rows           INTEGER,
        extracted_at     TIMESTAMPTZ,
        lake_path        VARCHAR NOT NULL,
        UNIQUE (company, statement_type, entity, period_end)
      )
    "
    )

    # -- Lake VIEW --------------------------------------------------------------
    # Parameterised glob so the VIEW stays correct even when lake_dir changes.
    # DROP + recreate is idempotent; the VIEW has no data of its own.
    # Ensure at least one Parquet file exists before the VIEW is resolved —
    # DuckDB requires files to be present at CREATE VIEW time.
    db_seed_lake_if_empty(lake_dir)
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_lake")
    lake_glob <- file.path(lake_dir, "**", "*.parquet")
    DBI::dbExecute(
      con,
      glue::glue(
        "
      CREATE VIEW v_lake AS
        SELECT * FROM read_parquet('{lake_glob}', hive_partitioning = true)
    "
      )
    )
  })

  # Seed vocabularies (outside the transaction — INSERT OR IGNORE is fine)
  db_seed_vocabularies(con)

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Internal — lake seed (ensures v_lake VIEW can always be created)
# ---------------------------------------------------------------------------

# DuckDB resolves a VIEW's schema at CREATE time, which requires at least one
# matching Parquet file.  On a fresh install the lake is empty, so we write a
# zero-row sentinel file.  The __seed__ partition values are never valid
# company/statement_type names, so they will never surface in real queries.
db_seed_lake_if_empty <- function(lake_dir) {
  existing <- list.files(lake_dir, pattern = "\\.parquet$", recursive = TRUE)
  if (length(existing) > 0L) {
    return(invisible(NULL))
  }

  seed_dir <- file.path(lake_dir, "company=__seed__", "statement_type=__seed__")
  dir.create(seed_dir, recursive = TRUE, showWarnings = FALSE)

  # Zero-row data.frame that matches the long-record schema (partition keys
  # are excluded — they live in the Hive directory names)
  seed <- data.frame(
    fiscal_year = integer(0),
    entity = character(0),
    period_label = character(0),
    period_end = as.Date(character(0)),
    period_type = character(0),
    audit_status = character(0),
    section = character(0),
    line_item = character(0),
    line_item_order = integer(0),
    row_role = character(0),
    value = double(0),
    value_text = character(0),
    value_type = character(0),
    unit = character(0),
    currency = character(0),
    source_file = character(0),
    source_page = character(0),
    extraction_method = character(0),
    extraction_status = character(0),
    validation_passed = logical(0),
    extracted_at = as.POSIXct(character(0)),
    edited_by = character(0),
    stringsAsFactors = FALSE
  )

  arrow::write_parquet(seed, file.path(seed_dir, "part-0.parquet"))
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Internal — vocabulary seeding
# ---------------------------------------------------------------------------

db_seed_vocabularies <- function(con) {
  seed_dir <- system.file("extdata", "seed", package = "fin.extract")
  if (!nzchar(seed_dir)) {
    return(invisible(NULL))
  } # package not installed yet

  seed_table <- function(csv_file, table_name) {
    path <- file.path(seed_dir, csv_file)
    if (!file.exists(path)) {
      return(invisible(NULL))
    }

    rows <- utils::read.csv(path, stringsAsFactors = FALSE)

    for (i in seq_len(nrow(rows))) {
      cols <- names(rows)
      vals <- rows[i, , drop = FALSE]
      # Build INSERT OR IGNORE so re-running is safe
      col_sql <- paste(cols, collapse = ", ")
      val_sql <- paste(
        vapply(
          vals,
          function(v) {
            if (is.logical(v)) {
              toupper(as.character(v))
            } else if (is.numeric(v)) {
              as.character(v)
            } else {
              paste0("'", gsub("'", "''", v), "'")
            }
          },
          character(1)
        ),
        collapse = ", "
      )
      DBI::dbExecute(
        con,
        glue::glue(
          "INSERT OR IGNORE INTO {table_name} ({col_sql}) VALUES ({val_sql})"
        )
      )
    }
  }

  seed_table("dim_statement_type.csv", "dim_statement_type")
  seed_table("vocab_audit_status.csv", "vocab_audit_status")
  seed_table("dim_user.csv", "dim_user")

  invisible(NULL)
}
