#' Catalog read/write helpers (FR-SV-01, FR-BR-01, FR-BR-02)
#'
#' The catalog table has exactly one row per confirmed grid slice:
#' `(company, statement_type, entity, period_end)`. All catalog access goes
#' through the pool — never open ad-hoc connections.
#'
#' @name catalog
NULL

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Upsert a catalog entry
#'
#' Inserts a new row or updates the existing row for the
#' `(company, statement_type, entity, period_end)` key (FR-SV-02: overwrite,
#' no versioning).
#'
#' @param pool          A pool object from [db_pool_connect()].
#' @param company       Company name (partition key).
#' @param statement_type Statement type string (partition key).
#' @param entity        Reporting entity name.
#' @param period_end    Period end date (`Date` or ISO-8601 string).
#' @param audit_status  Audit status string, or `NA`.
#' @param source_file   Original filename, or `NA`.
#' @param validation_passed `TRUE`/`FALSE`/`NA`.
#' @param n_rows        Number of long records saved.
#' @param extracted_at  Timestamp of extraction (`POSIXct` or ISO string).
#' @param lake_path     Relative path to the written Parquet partition.
#' @return Invisibly, the catalog_id of the upserted row.
#' @export
catalog_upsert <- function(
  pool,
  company,
  statement_type,
  entity,
  period_end,
  audit_status = NA_character_,
  source_file = NA_character_,
  validation_passed = NA,
  n_rows = NA_integer_,
  extracted_at = Sys.time(),
  lake_path
) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  period_end_s <- format(as.Date(period_end), "%Y-%m-%d")
  extracted_at_s <- format(as.POSIXct(extracted_at), "%Y-%m-%d %H:%M:%S")

  # DuckDB: explicit ON CONFLICT target required when table has multiple
  # UNIQUE constraints (PRIMARY KEY + business-key UNIQUE).
  DBI::dbExecute(
    con,
    glue::glue(
      "
    INSERT INTO catalog
      (company, statement_type, entity, period_end,
       audit_status, source_file, validation_passed,
       n_rows, extracted_at, lake_path)
    VALUES (
      {sql_str(company)},
      {sql_str(statement_type)},
      {sql_str(entity)},
      {sql_str(period_end_s)},
      {sql_str(audit_status)},
      {sql_str(source_file)},
      {sql_bool(validation_passed)},
      {sql_int(n_rows)},
      {sql_str(extracted_at_s)},
      {sql_str(lake_path)}
    )
    ON CONFLICT (company, statement_type, entity, period_end) DO UPDATE SET
      audit_status      = EXCLUDED.audit_status,
      source_file       = EXCLUDED.source_file,
      validation_passed = EXCLUDED.validation_passed,
      n_rows            = EXCLUDED.n_rows,
      extracted_at      = EXCLUDED.extracted_at,
      lake_path         = EXCLUDED.lake_path
  "
    )
  )

  id <- DBI::dbGetQuery(
    con,
    glue::glue(
      "
    SELECT catalog_id FROM catalog
    WHERE company        = {sql_str(company)}
      AND statement_type = {sql_str(statement_type)}
      AND entity         = {sql_str(entity)}
      AND period_end     = {sql_str(period_end_s)}
  "
    )
  )[["catalog_id"]]

  invisible(id)
}

#' List catalog entries (FR-BR-01, FR-BR-02)
#'
#' Returns a `data.frame` of catalog rows. Reads **only** the catalog table;
#' never scans the lake (FR-BR-02).
#'
#' @param pool         A pool object.
#' @param company      Filter by company name, or `NULL` for all.
#' @param type         Filter by statement type, or `NULL` for all.
#' @param fiscal_year  Filter by fiscal year integer, or `NULL` for all.
#' @param audit_status Filter by audit status string, or `NULL` for all.
#' @return A `data.frame` with columns matching the catalog schema.
#' @export
catalog_list <- function(
  pool,
  company = NULL,
  type = NULL,
  fiscal_year = NULL,
  audit_status = NULL
) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  where_clauses <- character(0)

  if (!is.null(company)) {
    where_clauses <- c(
      where_clauses,
      glue::glue("company = {sql_str(company)}")
    )
  }
  if (!is.null(type)) {
    where_clauses <- c(
      where_clauses,
      glue::glue("statement_type = {sql_str(type)}")
    )
  }
  if (!is.null(fiscal_year)) {
    where_clauses <- c(
      where_clauses,
      glue::glue("YEAR(period_end) = {as.integer(fiscal_year)}")
    )
  }
  if (!is.null(audit_status)) {
    where_clauses <- c(
      where_clauses,
      glue::glue("audit_status = {sql_str(audit_status)}")
    )
  }

  where_sql <- if (length(where_clauses)) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }

  DBI::dbGetQuery(
    con,
    glue::glue(
      "
    SELECT catalog_id, company, entity, statement_type,
           period_end, audit_status, source_file,
           validation_passed, n_rows, extracted_at, lake_path
    FROM catalog
    {where_sql}
    ORDER BY company, statement_type, period_end DESC
  "
    )
  )
}

#' Retrieve a single catalog entry
#'
#' @param pool           A pool object.
#' @param company        Company name.
#' @param statement_type Statement type.
#' @param entity         Entity name.
#' @param period_end     Period end date.
#' @return A one-row `data.frame`, or a zero-row `data.frame` if not found.
#' @export
catalog_get <- function(pool, company, statement_type, entity, period_end) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  period_end_s <- format(as.Date(period_end), "%Y-%m-%d")

  DBI::dbGetQuery(
    con,
    glue::glue(
      "
    SELECT * FROM catalog
    WHERE company        = {sql_str(company)}
      AND statement_type = {sql_str(statement_type)}
      AND entity         = {sql_str(entity)}
      AND period_end     = {sql_str(period_end_s)}
  "
    )
  )
}

# ---------------------------------------------------------------------------
# Internal SQL helpers (keep queries injection-safe for typed values)
# ---------------------------------------------------------------------------

sql_str <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("NULL")
  }
  paste0("'", gsub("'", "''", as.character(x)), "'")
}

sql_bool <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("NULL")
  }
  if (isTRUE(x)) "TRUE" else "FALSE"
}

sql_int <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("NULL")
  }
  as.character(as.integer(x))
}
