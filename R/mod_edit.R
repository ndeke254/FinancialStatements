#' Edit module — editable grid, metadata confirmation, and save (FR-ED-01..06, FR-SV-01..03)
#'
#' Presents the extraction as a wide `rhandsontable` (FR-ED-01) for human
#' confirmation.  Nothing persists until the user clicks **Confirm & Save**
#' (FR-ED-02).
#'
#' Metadata inputs:
#' - **Filing-level** (FR-ED-04): company, currency, unit, source_page.
#' - **Per-column** (FR-ED-03): entity, period_label, period_end, period_type,
#'   audit_status — one set of inputs rendered per detected value column.
#' - **Per-row** (FR-ED-05): section, line_item, line_item_order, row_role —
#'   engine proposes row_role via [detect_row_roles()]; user edits in grid.
#' - **Per-row unit/value_type** (FR-ED-06): applied in [build_long_for_save()]
#'   from row_role at save time.
#'
#' On confirm: [build_long_for_save()] → [store_write()] → catalog upsert
#' (FR-SV-01).  Re-saving the same slice overwrites (FR-SV-02).
#'
#' @name mod_edit
NULL

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

#' Edit module UI
#'
#' @param id Module namespace id.
#' @export
mod_edit_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_pill(
    id = ns("edit_tabs"),

    # --- Tab 1: Grid ---
    bslib::nav_panel(
      "Grid",
      shiny::icon("table"),
      bslib::card_body(
        shiny::uiOutput(ns("grid_status")),
        rhandsontable::rHandsontableOutput(ns("hot"), height = "480px")
      )
    ),

    # --- Tab 2: Column metadata ---
    bslib::nav_panel(
      "Column metadata",
      shiny::icon("columns"),
      bslib::card_body(
        shiny::tags$p(
          class = "text-muted small",
          "Tag each extracted value column with the entity and period it represents."
        ),
        shiny::uiOutput(ns("col_meta_ui"))
      )
    ),

    # --- Tab 3: Filing metadata ---
    bslib::nav_panel(
      "Filing metadata",
      shiny::icon("file-lines"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1 / 2,
          shiny::textInput(ns("company"),  "Company",  placeholder = "e.g. CIC Insurance Group"),
          shiny::textInput(ns("currency"), "Currency", value = "KES"),
          shiny::textInput(ns("unit_default"), "Default unit", value = "KES'000"),
          shiny::textInput(ns("source_page"),  "Source page(s)", placeholder = "1")
        )
      )
    ),

    # --- Footer: save button (always visible) ---
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionButton(
        ns("btn_save"), "Confirm & Save",
        class = "btn-success",
        icon  = shiny::icon("floppy-disk")
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

#' Edit module server
#'
#' Admin gate enforced at the server level (FR-UP-00).
#'
#' @param id         Module namespace id.
#' @param pool       Pool object from [db_pool_connect()].
#' @param lake_dir   Path to the Parquet lake root.
#' @param upload_rv  Reactive from [mod_upload_server()].
#' @param extract_rv Reactive from [mod_extract_server()].
#' @param user_role  Reactive or character role string.
#' @param user_name  Character username stamped as `edited_by`.
#' @export
mod_edit_server <- function(id, pool, lake_dir,
                            upload_rv, extract_rv,
                            user_role, user_name = "admin") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Admin gate ------------------------------------------------------------
    shiny::observe({
      role <- if (shiny::is.reactive(user_role)) user_role() else user_role
      if (!is_admin(role)) {
        shiny::showNotification("Editing requires admin access.",
                                type = "error", duration = 5L)
      }
    })

    rv <- shiny::reactiveValues(
      wide_dt     = NULL,   # data.table shown in grid
      n_val_cols  = 0L,
      orig_texts  = NULL    # original extracted text per (row, col_k)
    )

    # -- Prepare grid from extraction result -----------------------------------
    shiny::observe({
      ex <- extract_rv()
      if (is.null(ex) || is.null(ex$result)) return()
      res <- ex$result
      if (is.null(res$table_dt) || nrow(res$table_dt) == 0L) return()

      tbl <- data.table::copy(res$table_dt)
      val_cols <- .sorted_val_cols(names(tbl))

      # Propose row_roles (FR-ED-05) — uses `label` column in tbl
      if (length(val_cols) > 0L && "label" %in% names(tbl)) {
        detect_row_roles(tbl, value_cols = val_cols)
      } else if (!("row_role" %in% names(tbl))) {
        tbl[, row_role := "line"]
      }

      # Rename label → line_item for display
      if ("label" %in% names(tbl)) {
        data.table::setnames(tbl, "label", "line_item")
      }
      tbl[, section   := ""]
      tbl[, value_type := "currency"]
      tbl[, unit       := "KES'000"]
      tbl[, currency   := "KES"]

      # Record the original extracted text for the _text__ columns on save
      orig <- data.table::copy(tbl[, c("line_item_order", val_cols), with = FALSE])
      rv$orig_texts  <- orig
      rv$n_val_cols  <- length(val_cols)

      # Column order for grid: row meta first, then value cols
      row_meta <- c("section", "line_item", "line_item_order",
                    "row_role", "value_type", "unit", "currency")
      present <- intersect(c(row_meta, val_cols), names(tbl))
      data.table::setcolorder(tbl, present)
      rv$wide_dt <- tbl
    })

    # -- Render rhandsontable --------------------------------------------------
    output$hot <- rhandsontable::renderRHandsontable({
      req(rv$wide_dt)
      df <- as.data.frame(rv$wide_dt)
      rhandsontable::rhandsontable(
        df,
        rowHeaders     = NULL,
        stretchH       = "all",
        contextMenu    = TRUE,
        useTypes       = FALSE
      ) |>
        rhandsontable::hot_col(
          "line_item_order",
          readOnly = TRUE,
          type     = "numeric"
        ) |>
        rhandsontable::hot_col(
          "row_role",
          type   = "dropdown",
          source = c("header", "line", "subtotal", "total", "ratio")
        ) |>
        rhandsontable::hot_col(
          "value_type",
          type   = "dropdown",
          source = c("currency", "per_share", "percent", "count")
        ) |>
        rhandsontable::hot_col(
          "currency",
          type   = "dropdown",
          source = c("KES", "USD", "EUR", "GBP")
        )
    })

    output$grid_status <- shiny::renderUI({
      if (is.null(rv$wide_dt)) {
        return(shiny::tags$p(
          class = "text-muted small",
          shiny::icon("info-circle"),
          " Run extraction first to populate the grid."
        ))
      }
      shiny::tags$p(
        class = "text-success small",
        shiny::icon("circle-check"),
        sprintf(" %d rows, %d value column(s). Edit values and row metadata, then save.",
                nrow(rv$wide_dt), rv$n_val_cols)
      )
    })

    # -- Per-column metadata UI (FR-ED-03) ------------------------------------
    output$col_meta_ui <- shiny::renderUI({
      n <- rv$n_val_cols
      if (n == 0L) {
        return(shiny::tags$p(class = "text-muted", "No extraction result yet."))
      }
      panels <- lapply(seq_len(n), function(k) {
        bslib::card(
          bslib::card_header(
            shiny::icon("table-columns"),
            sprintf(" Column %d (col_%d)", k, k)
          ),
          bslib::card_body(
            bslib::layout_column_wrap(
              width = 1 / 2,
              shiny::textInput(
                ns(paste0("col_entity_", k)),
                "Entity",
                placeholder = "e.g. CIC Insurance Group"
              ),
              shiny::textInput(
                ns(paste0("col_period_label_", k)),
                "Period label",
                placeholder = "e.g. H1 2024"
              ),
              shiny::dateInput(
                ns(paste0("col_period_end_", k)),
                "Period end",
                value = Sys.Date()
              ),
              shiny::selectInput(
                ns(paste0("col_period_type_", k)),
                "Period type",
                choices = c(
                  "instant", "duration_half_year", "duration_year",
                  "duration_quarter", "duration_ytd", "other"
                )
              ),
              shiny::selectInput(
                ns(paste0("col_audit_status_", k)),
                "Audit status",
                choices = c("unaudited", "audited", "reviewed",
                            "restated", "proforma")
              )
            )
          )
        )
      })
      do.call(shiny::tagList, panels)
    })

    # -- Save (FR-SV-01, FR-SV-02) --------------------------------------------
    shiny::observeEvent(input$btn_save, {
      role <- if (shiny::is.reactive(user_role)) user_role() else user_role
      if (!is_admin(role)) {
        shiny::showNotification("Save requires admin access.", type = "error")
        return()
      }
      req(rv$wide_dt, rv$n_val_cols > 0L)

      n <- rv$n_val_cols

      # Read current grid
      if (!is.null(input$hot)) {
        grid_df <- rhandsontable::hot_to_r(input$hot)
        wide_dt <- data.table::as.data.table(grid_df)
      } else {
        wide_dt <- data.table::copy(rv$wide_dt)
      }

      # Build col_meta
      col_meta <- data.table::rbindlist(lapply(seq_len(n), function(k) {
        entity       <- trimws(input[[paste0("col_entity_", k)]]       %||% "")
        period_label <- trimws(input[[paste0("col_period_label_", k)]] %||% "")
        period_end   <- input[[paste0("col_period_end_", k)]]          %||% Sys.Date()
        period_type  <- input[[paste0("col_period_type_", k)]]         %||% "instant"
        audit_status <- input[[paste0("col_audit_status_", k)]]        %||% "unaudited"

        if (!nzchar(entity))       entity       <- paste0("entity_", k)
        if (!nzchar(period_label)) period_label <- paste0("col_", k)

        data.table::data.table(
          col_key      = paste0(entity, "__", period_label),
          entity       = entity,
          period_label = period_label,
          period_end   = as.Date(period_end),
          period_type  = period_type,
          audit_status = audit_status
        )
      }))

      # Attach orig_texts as _text__ columns before building long
      val_cols <- .sorted_val_cols(names(wide_dt))
      if (!is.null(rv$orig_texts)) {
        for (k in seq_along(val_cols)) {
          vc <- val_cols[[k]]
          txt_col <- paste0("_text__", vc)
          if (!(txt_col %in% names(wide_dt))) {
            orig_col_vals <- rv$orig_texts[[vc]]
            if (!is.null(orig_col_vals)) {
              current_vals <- as.character(wide_dt[[vc]])
              # If user edited the cell, current_vals differ from orig;
              # always use current_vals as text (what the user sees)
              wide_dt[, (txt_col) := current_vals]
            }
          }
        }
      }

      # Build filing metadata
      ex      <- extract_rv()
      up      <- upload_rv()
      company <- trimws(input$company %||% up$guess_company %||% "Unknown")
      stmt_type <- if (!is.null(ex)) ex$statement_type %||% "income_statement" else "income_statement"
      fiscal_year <- if (!is.null(col_meta$period_end) && length(col_meta$period_end) > 0L) {
        as.integer(format(min(col_meta$period_end, na.rm = TRUE), "%Y"))
      } else NA_integer_

      filing_meta <- list(
        company           = company,
        statement_type    = stmt_type,
        fiscal_year       = fiscal_year,
        currency          = input$currency  %||% "KES",
        source_file       = up$filename     %||% NA_character_,
        source_page       = input$source_page %||% "1",
        extraction_method = if (!is.null(ex$result)) ex$result$method %||% "native" else "native",
        extraction_status = "confirmed",
        validation_passed = NA,
        extracted_at      = Sys.time(),
        edited_by         = user_name
      )

      # Convert and save
      tryCatch({
        long_dt <- build_long_for_save(wide_dt, col_meta, filing_meta)
        store_write(long_dt, lake_dir = lake_dir, pool = pool,
                    edited_by = user_name)
        shiny::showNotification(
          paste0("Saved \u2014 ", nrow(long_dt), " records for ",
                 company, " / ", stmt_type),
          type = "message", duration = 8L
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Save failed:", conditionMessage(e)),
          type = "error", duration = 10L
        )
      })
    })
  })
}

# ---------------------------------------------------------------------------
# Exported save-pipeline helper (testable without Shiny)
# ---------------------------------------------------------------------------

#' Build the long data.table ready for store_write from a confirmed wide grid
#'
#' Bridges the `rhandsontable` grid format (value columns named `col_1`,
#' `col_2`, ...) and the long format expected by [store_write()].
#'
#' Steps:
#' 1. Apply per-row `value_type`/`unit` from `row_role` (FR-ED-06).
#' 2. Parse value column text → numeric via [parse_numbers()].
#' 3. Attach `_text__col_key` columns (raw text for audit).
#' 4. Rename `col_k` → `col_key[k]` (entity__period_label format).
#' 5. Call [wide_to_long()].
#'
#' @param wide_dt    Wide `data.table` with row-meta columns and `col_1`,
#'                   `col_2`, ... holding character value strings.
#' @param col_meta   `data.table` with `col_key`, `entity`, `period_label`,
#'                   `period_end`, `period_type`, `audit_status`.  One row
#'                   per `col_k`, in `col_1`/`col_2`/... order.
#' @param filing_meta Named list passed to [wide_to_long()].
#' @return A long `data.table` ready for [store_write()].
#' @export
build_long_for_save <- function(wide_dt, col_meta, filing_meta = list()) {
  stopifnot(data.table::is.data.table(wide_dt))
  stopifnot(data.table::is.data.table(col_meta))

  w         <- data.table::copy(wide_dt)
  val_cols  <- .sorted_val_cols(names(w))
  col_keys  <- col_meta[["col_key"]]

  if (length(val_cols) != nrow(col_meta)) {
    stop(sprintf(
      "build_long_for_save: %d value column(s) in wide_dt but %d row(s) in col_meta.",
      length(val_cols), nrow(col_meta)
    ))
  }

  # -- FR-ED-06: per-row value_type/unit from row_role -----------------------
  if ("row_role" %in% names(w) && "line_item" %in% names(w)) {
    per_share_re <- "(?i)\\beps\\b|\\bdps\\b|per\\s+share|earnings\\s+per|dividend\\s+per"
    is_ps_row <- grepl(per_share_re, w[["line_item"]], perl = TRUE) &
      w[["row_role"]] == "ratio"
    is_ratio_other <- w[["row_role"]] == "ratio" & !is_ps_row

    if (any(is_ps_row))
      w[is_ps_row, c("value_type", "unit") := .("per_share", "KES")]
    if (any(is_ratio_other) && "value_type" %in% names(w))
      w[is_ratio_other & (is.na(value_type) | value_type == "currency"),
        c("value_type", "unit") := .("percent", "%")]
  }

  # -- Parse value columns and build _text__ columns -------------------------
  for (k in seq_along(val_cols)) {
    vc  <- val_cols[[k]]
    ck  <- col_keys[[k]]
    txt <- paste0("_text__", ck)

    raw_text <- as.character(w[[vc]])

    # Determine is_per_share per row
    is_ps <- if ("row_role" %in% names(w) && "line_item" %in% names(w)) {
      grepl(
        "(?i)\\beps\\b|\\bdps\\b|per\\s+share|earnings\\s+per|dividend\\s+per",
        w[["line_item"]], perl = TRUE
      ) & w[["row_role"]] == "ratio"
    } else {
      rep(FALSE, nrow(w))
    }

    parsed <- parse_numbers(raw_text, is_per_share = is_ps)

    # Attach _text__ first (using the raw text the user saw/edited)
    # If a _text__ column was already present (pre-attached), keep it
    if (!(txt %in% names(w))) {
      existing_txt <- if (paste0("_text__", vc) %in% names(w)) {
        as.character(w[[paste0("_text__", vc)]])
      } else {
        raw_text
      }
      w[, (txt) := existing_txt]
    }

    # Replace col_k with parsed numeric
    w[, (vc) := parsed$value]

    # Drop the old _text__col_k column if it was a temp placeholder
    old_txt <- paste0("_text__", vc)
    if (old_txt %in% names(w) && old_txt != txt) w[, (old_txt) := NULL]

    # Rename col_k → col_key
    if (vc != ck) data.table::setnames(w, vc, ck)
  }

  wide_to_long(w, col_meta = col_meta, filing_meta = filing_meta)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Return col_1..col_N column names sorted numerically
.sorted_val_cols <- function(col_names) {
  vc <- grep("^col_\\d+$", col_names, value = TRUE)
  vc[order(as.integer(sub("^col_", "", vc)))]
}
