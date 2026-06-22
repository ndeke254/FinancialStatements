#' Wide ↔ long pivot helpers
#'
#' @description
#' The store is **long/tidy** (one row per company × entity × period × line
#' item); the `rhandsontable` confirmation grid is **wide** (line items as rows,
#' one value-column per `entity__period_label`).  These two functions bridge
#' the shapes.
#'
#' Key design decisions:
#' - Column key is `entity__period_label` (double-underscore separator).
#' - Row key is `(section, line_item, line_item_order, row_role, value_type, unit)`.
#' - `value_text` (the raw printed string) is stored per column in a parallel
#'   set of `_text__entity__period_label` columns — needed for the grid display
#'   and round-trip audit.
#' - Only `value_type`, `unit`, and `currency` are per-line-item in the wide
#'   form (they must be uniform across columns for a given row).
#' - Per-column metadata (`period_end`, `period_type`, `audit_status`, `entity`)
#'   is returned in a separate `col_meta` attribute so `wide_to_long` can
#'   reconstruct the long form exactly.
#'
#' @name fct_pivot
NULL

# ---------------------------------------------------------------------------
# Column-key helpers
# ---------------------------------------------------------------------------

#' Build a column key string from entity + period_label
#' @keywords internal
.col_key <- function(entity, period_label) {
  paste0(entity, "__", period_label)
}

#' Parse a column key back into entity + period_label
#' @keywords internal
.parse_col_key <- function(key) {
  # key = "entity__period_label" — split on first double-underscore
  parts <- strsplit(key, "__", fixed = TRUE)[[1L]]
  list(entity = parts[[1L]], period_label = paste(parts[-1L], collapse = "__"))
}

# ---------------------------------------------------------------------------
# long_to_wide
# ---------------------------------------------------------------------------

#' Pivot long records to a wide data.table for grid display
#'
#' Columns in the wide output:
#' - `section`, `line_item`, `line_item_order`, `row_role`, `value_type`,
#'   `unit`, `currency` — row metadata.
#' - `<entity>__<period_label>` — one numeric value column per period.
#' - `_text__<entity>__<period_label>` — raw-string counterpart.
#'
#' A `col_meta` attribute (a `data.table`) records per-column metadata
#' (`col_key`, `entity`, `period_label`, `period_end`, `period_type`,
#' `audit_status`) so the round-trip through `wide_to_long` is lossless.
#'
#' @param long_dt A `data.table` in long/tidy format.
#' @return A wide `data.table` with a `col_meta` attribute.
#' @export
long_to_wide <- function(long_dt) {
  stopifnot(data.table::is.data.table(long_dt))

  # Row identity columns
  row_id_cols <- c(
    "section",
    "line_item",
    "line_item_order",
    "row_role",
    "value_type",
    "unit",
    "currency"
  )

  # Build column keys
  dt <- data.table::copy(long_dt)
  dt[, col_key := .col_key(entity, period_label)]

  # -- col_meta: one row per unique column key ---------------------------------
  col_meta <- unique(dt[, .(
    col_key,
    entity,
    period_label,
    period_end,
    period_type,
    audit_status
  )])
  data.table::setorder(col_meta, entity, period_end)

  col_keys <- col_meta[["col_key"]]

  # -- Pivot values ------------------------------------------------------------
  wide_val <- data.table::dcast(
    dt,
    formula = stats::as.formula(paste(
      paste(row_id_cols, collapse = " + "),
      "~ col_key"
    )),
    value.var = "value",
    fun.aggregate = function(x) x[[1L]] # confirmed data: one row per key
  )

  # -- Pivot value_text --------------------------------------------------------
  wide_txt <- data.table::dcast(
    dt,
    formula = stats::as.formula(paste(
      paste(row_id_cols, collapse = " + "),
      "~ col_key"
    )),
    value.var = "value_text",
    fun.aggregate = function(x) x[[1L]]
  )

  # Rename text columns with "_text__" prefix
  txt_rename <- setdiff(names(wide_txt), row_id_cols)
  data.table::setnames(wide_txt, txt_rename, paste0("_text__", txt_rename))

  # -- Merge and order ---------------------------------------------------------
  wide <- wide_val[wide_txt, on = row_id_cols]

  # Put value columns before text columns, preserve row order
  val_cols <- intersect(col_keys, names(wide))
  txt_cols <- intersect(paste0("_text__", col_keys), names(wide))
  data.table::setcolorder(wide, c(row_id_cols, val_cols, txt_cols))
  data.table::setorderv(wide, c("section", "line_item_order"))

  data.table::setattr(wide, "col_meta", col_meta)
  wide
}

# ---------------------------------------------------------------------------
# wide_to_long
# ---------------------------------------------------------------------------

#' Un-pivot a wide grid back to long records
#'
#' Inverts [long_to_wide()].  Call this on save, after the user has finished
#' editing the wide `rhandsontable` grid (FR-SV-01).
#'
#' @param wide_dt      Wide `data.table` (as produced by [long_to_wide()] or
#'                     edited in the grid).  May or may not carry the
#'                     `col_meta` attribute.
#' @param col_meta     A `data.table` of per-column metadata with columns
#'                     `col_key`, `entity`, `period_label`, `period_end`,
#'                     `period_type`, `audit_status`.  If `NULL`, the function
#'                     tries `attr(wide_dt, "col_meta")`.
#' @param filing_meta  A named list with filing-level fields:
#'                     `company`, `statement_type`, `fiscal_year`, `currency`,
#'                     `source_file`, `source_page`, `extraction_method`,
#'                     `extraction_status`, `extracted_at`, `edited_by`.
#' @return A long `data.table` ready for [store_write()].
#' @export
wide_to_long <- function(wide_dt, col_meta = NULL, filing_meta = list()) {
  stopifnot(data.table::is.data.table(wide_dt))

  if (is.null(col_meta)) {
    col_meta <- attr(wide_dt, "col_meta")
  }
  if (is.null(col_meta)) {
    stop("`col_meta` must be supplied or present as an attribute on `wide_dt`.")
  }

  row_id_cols <- c(
    "section",
    "line_item",
    "line_item_order",
    "row_role",
    "value_type",
    "unit",
    "currency"
  )

  col_keys <- col_meta[["col_key"]]

  # -- Melt values -------------------------------------------------------------
  val_cols_present <- intersect(col_keys, names(wide_dt))
  long_val <- data.table::melt(
    wide_dt,
    id.vars = row_id_cols,
    measure.vars = val_cols_present,
    variable.name = "col_key",
    value.name = "value",
    variable.factor = FALSE
  )

  # -- Melt value_text ---------------------------------------------------------
  txt_col_map <- stats::setNames(
    paste0("_text__", col_keys),
    col_keys
  )
  txt_cols_present <- intersect(txt_col_map, names(wide_dt))
  if (length(txt_cols_present)) {
    long_txt <- data.table::melt(
      wide_dt,
      id.vars = row_id_cols,
      measure.vars = txt_cols_present,
      variable.name = "col_key_txt",
      value.name = "value_text",
      variable.factor = FALSE
    )
    # Strip "_text__" prefix to align keys
    long_txt[, col_key := sub("^_text__", "", col_key_txt)]
    long_txt[, col_key_txt := NULL]

    long_val <- long_val[
      long_txt[, .(
        col_key,
        value_text,
        section,
        line_item,
        line_item_order,
        row_role,
        value_type,
        unit,
        currency
      )],
      on = c(row_id_cols, "col_key")
    ]
  } else {
    long_val[, value_text := NA_character_]
  }

  # -- Attach per-column metadata ----------------------------------------------
  long_val <- long_val[col_meta, on = "col_key", nomatch = 0L]

  # -- Attach filing-level metadata -------------------------------------------
  fm_defaults <- list(
    company = NA_character_,
    statement_type = NA_character_,
    fiscal_year = NA_integer_,
    currency = "KES",
    source_file = NA_character_,
    source_page = NA_character_,
    extraction_method = NA_character_,
    extraction_status = "confirmed",
    validation_passed = NA,
    extracted_at = Sys.time(),
    edited_by = NA_character_
  )
  fm <- utils::modifyList(fm_defaults, filing_meta)

  for (field in names(fm)) {
    long_val[[field]] <- fm[[field]]
  }

  # -- Drop pivot helper column ------------------------------------------------
  long_val[, col_key := NULL]

  # -- Order columns to match .LONG_REQUIRED_COLS order -----------------------
  all_cols <- union(.LONG_REQUIRED_COLS, names(long_val))
  extra_cols <- setdiff(names(long_val), .LONG_REQUIRED_COLS)
  data.table::setcolorder(long_val, c(.LONG_REQUIRED_COLS, extra_cols))

  data.table::setorderv(long_val, c("entity", "period_end", "line_item_order"))
  long_val[]
}
