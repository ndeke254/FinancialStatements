#' Read long records from the Parquet lake via DuckDB (FR-AN-02)
#'
#' All reads go through the shared pool — the DuckDB `v_lake` VIEW handles
#' partition pruning automatically when `company` and `statement_type` filters
#' are supplied.
#'
#' @name store_read
NULL

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Read a slice of the lake as a data.table
#'
#' Queries the `v_lake` VIEW with partition and column pruning.  At minimum,
#' supply `company` and `statement_type` to target a single partition.
#'
#' @param pool           A pool object from [db_pool_connect()].
#' @param company        Company name (maps to Hive partition).
#' @param statement_type Statement type (maps to Hive partition).
#' @param entity         Entity name filter, or `NULL` for all.
#' @param fiscal_year    Integer fiscal year filter, or `NULL` for all.
#' @param period_end     Date filter (exact match), or `NULL` for all.
#' @param audit_status   Audit status filter, or `NULL` for all.
#' @param row_roles      Character vector of `row_role` values to include,
#'                       or `NULL` for all.  Useful for excluding
#'                       `header`/`subtotal`/`total` in analytical queries
#'                       (FR-AN-04).
#' @return A `data.table`, ordered by `entity`, `period_end`,
#'         `line_item_order`.
#' @export
store_read <- function(
  pool,
  company,
  statement_type,
  entity = NULL,
  fiscal_year = NULL,
  period_end = NULL,
  audit_status = NULL,
  row_roles = NULL
) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  where_clauses <- c(
    glue::glue("company        = {sql_str(company)}"),
    glue::glue("statement_type = {sql_str(statement_type)}")
  )

  if (!is.null(entity)) {
    where_clauses <- c(where_clauses, glue::glue("entity = {sql_str(entity)}"))
  }
  if (!is.null(fiscal_year)) {
    where_clauses <- c(
      where_clauses,
      glue::glue("fiscal_year = {as.integer(fiscal_year)}")
    )
  }
  if (!is.null(period_end)) {
    period_end_s <- format(as.Date(period_end), "%Y-%m-%d")
    where_clauses <- c(
      where_clauses,
      glue::glue("period_end = {sql_str(period_end_s)}")
    )
  }
  if (!is.null(audit_status)) {
    where_clauses <- c(
      where_clauses,
      glue::glue("audit_status = {sql_str(audit_status)}")
    )
  }
  if (!is.null(row_roles)) {
    role_list <- paste(
      vapply(row_roles, sql_str, character(1)),
      collapse = ", "
    )
    where_clauses <- c(where_clauses, glue::glue("row_role IN ({role_list})"))
  }

  where_sql <- paste("WHERE", paste(where_clauses, collapse = "\n    AND "))

  result <- DBI::dbGetQuery(
    con,
    glue::glue(
      "
    SELECT *
    FROM   v_lake
    {where_sql}
    ORDER BY entity, period_end, line_item_order
  "
    )
  )

  data.table::as.data.table(result)
}

#' Read the full list of available line items for a company × statement_type
#'
#' Used to populate line-item pickers in the analytics module without pulling
#' all values.
#'
#' @param pool           A pool object.
#' @param company        Company name.
#' @param statement_type Statement type.
#' @return A `data.table` with columns `section`, `line_item`, `row_role`.
#' @export
store_read_line_items <- function(pool, company, statement_type) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  data.table::as.data.table(DBI::dbGetQuery(
    con,
    glue::glue(
      "
    SELECT DISTINCT section, line_item, row_role
    FROM   v_lake
    WHERE  company        = {sql_str(company)}
      AND  statement_type = {sql_str(statement_type)}
    ORDER BY section, line_item
  "
    )
  ))
}
