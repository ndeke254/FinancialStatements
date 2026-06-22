#' Write long records to the Parquet lake (FR-SV-01, FR-SV-02, FR-SV-03)
#'
#' @description
#' Writes (or overwrites) a slice of the Parquet lake identified by
#' `(company, statement_type)`.  The overwrite rule from FR-SV-02 is:
#' *all rows for the matching `(entity, period_end)` in the partition are
#' replaced*.  Rows for other `(entity, period_end)` combinations in the same
#' partition are preserved.
#'
#' FR-SV-03 consistency: the catalog upsert only runs after the Parquet write
#' succeeds, so a write failure leaves the catalog stale-but-consistent (it
#' either retains the old path or has no entry yet).
#'
#' @name store_write
NULL

# ---------------------------------------------------------------------------
# Required long-record columns (subset that must be present)
# ---------------------------------------------------------------------------

.LONG_REQUIRED_COLS <- c(
  "company",
  "statement_type",
  "fiscal_year",
  "entity",
  "period_label",
  "period_end",
  "period_type",
  "audit_status",
  "section",
  "line_item",
  "line_item_order",
  "row_role",
  "value",
  "value_text",
  "value_type",
  "unit",
  "currency",
  "source_file",
  "source_page",
  "extraction_method",
  "extraction_status",
  "validation_passed",
  "extracted_at",
  "edited_by"
)

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Save a confirmed long-record data.table to the Parquet lake
#'
#' After a successful write, upserts the catalog via [catalog_upsert()].
#'
#' @param long_dt      A `data.table` in long/tidy format.  Must contain at
#'                     minimum the columns in `.LONG_REQUIRED_COLS`.
#' @param lake_dir     Absolute path to the Parquet lake root.
#' @param pool         A pool object used for the catalog upsert.
#' @param edited_by    Username of the admin performing the save.
#' @return Invisibly, the relative `lake_path` that was written.
#' @export
store_write <- function(long_dt, lake_dir, pool, edited_by = NA_character_) {
  stopifnot(data.table::is.data.table(long_dt))

  missing_cols <- setdiff(.LONG_REQUIRED_COLS, names(long_dt))
  if (length(missing_cols)) {
    stop(
      "long_dt is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  company <- unique(long_dt[["company"]])
  statement_type <- unique(long_dt[["statement_type"]])
  if (length(company) != 1L || length(statement_type) != 1L) {
    stop(
      "long_dt must contain exactly one company and one statement_type per call."
    )
  }

  # Stamp edited_by if provided
  if (!is.na(edited_by)) {
    long_dt[, edited_by := edited_by]
  }

  # -- Partition path ----------------------------------------------------------
  partition_dir <- file.path(
    lake_dir,
    paste0("company=", .hive_val(company)),
    paste0("statement_type=", .hive_val(statement_type))
  )
  if (!dir.exists(partition_dir)) {
    dir.create(partition_dir, recursive = TRUE)
  }

  # -- Overwrite semantics (FR-SV-02) -----------------------------------------
  # Load existing rows for this partition that belong to OTHER (entity, period_end)
  # slices and merge them with the new data before re-writing.
  existing <- .read_partition_if_exists(partition_dir)

  entity <- unique(long_dt[["entity"]])
  period_end <- unique(long_dt[["period_end"]])

  if (!is.null(existing) && nrow(existing) > 0L) {
    # Keep rows NOT belonging to any of the incoming (entity, period_end) pairs.
    # Use the !DT anti-join idiom; key_pairs is the set to exclude.
    key_pairs <- data.table::CJ(entity = entity, period_end = period_end)
    existing  <- existing[!key_pairs, on = c("entity", "period_end")]
    combined <- data.table::rbindlist(
      list(existing, long_dt),
      use.names = TRUE,
      fill = TRUE
    )
  } else {
    combined <- data.table::copy(long_dt)
  }

  # Keep company/statement_type as real columns in the Parquet file.
  # DuckDB's hive_partitioning=true would otherwise use the sanitised directory
  # name as the column value (e.g. "CIC_Insurance_Group" instead of the
  # original "CIC Insurance Group"), causing WHERE-clause mismatches in
  # store_read.  Storing the original values in the file takes precedence over
  # the Hive key.

  # -- Write -------------------------------------------------------------------
  part_file <- file.path(partition_dir, "part-0.parquet")
  arrow::write_parquet(combined, part_file)

  lake_path <- file.path(
    paste0("company=", .hive_val(company)),
    paste0("statement_type=", .hive_val(statement_type))
  )

  # -- Catalog upsert (only after successful write) ----------------------------
  # Aggregate per-entity/period for the catalog (one entry per confirmed grid).
  # Use the Cartesian product so every (entity, period_end) pair is upserted,
  # regardless of how many unique entities or periods exist in this write.
  pairs <- data.table::CJ(entity = entity, period_end = period_end)
  for (i in seq_len(nrow(pairs))) {
    ent <- pairs[["entity"]][[i]]
    ped <- pairs[["period_end"]][[i]]
    slice_rows <- long_dt[long_dt$entity == ent & long_dt$period_end == ped]
    if (nrow(slice_rows) == 0L) next

    catalog_upsert(
      pool = pool,
      company = company,
      statement_type = statement_type,
      entity = ent,
      period_end = ped,
      audit_status = slice_rows[["audit_status"]][[1L]],
      source_file = slice_rows[["source_file"]][[1L]],
      validation_passed = any(isTRUE(slice_rows[["validation_passed"]])),
      n_rows = nrow(slice_rows),
      extracted_at = slice_rows[["extracted_at"]][[1L]],
      lake_path = lake_path
    )
  }

  invisible(lake_path)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Sanitise a string for use as a Hive partition directory segment
.hive_val <- function(x) {
  gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
}

# Read an existing Parquet partition (returns NULL if empty / absent)
.read_partition_if_exists <- function(partition_dir) {
  files <- list.files(
    partition_dir,
    pattern = "\\.parquet$",
    full.names = TRUE,
    recursive = FALSE
  )
  if (!length(files)) {
    return(NULL)
  }
  dt <- data.table::as.data.table(arrow::read_parquet(files[[1L]]))
  if (nrow(dt) == 0L) {
    return(NULL)
  }
  dt
}
