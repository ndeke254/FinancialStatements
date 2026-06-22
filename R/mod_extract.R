#' Extract module — region demarcation + native extraction (FR-RG-01..04)
#'
#' Renders the selected page with a rubber-band crop UI (crop_select.js),
#' assigns one `statement_type` per region, then runs the native engine via
#' [extract()] inside an `ExtendedTask` (async, FR-EX-05).
#'
#' For a page range: extraction runs on `page_from`; multi-page stitching
#' (FR-RG-04) is handled by running once per page and rbind-listing the
#' `table_dt` rows with a `continuation` flag. (OCR stitching lives in M4.)
#'
#' @name mod_extract
NULL

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

#' Extract module UI
#'
#' @param id Module namespace id.
#' @export
mod_extract_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7L, 5L),

    # Left: page image + crop
    bslib::card(
      bslib::card_header(shiny::icon("crop"), " Demarcate region"),
      bslib::card_body(
        shiny::tags$p(
          class = "text-muted small mb-1",
          "Draw a rectangle on the page to define the table region.",
          " Leave blank to use the full page."
        ),
        shiny::tags$div(
          id    = ns("page_container"),
          class = "crop-container",
          shiny::uiOutput(ns("page_img_ui"))
        )
      )
    ),

    # Right: controls
    bslib::card(
      bslib::card_header(shiny::icon("sliders"), " Extraction options"),
      bslib::card_body(
        shiny::uiOutput(ns("crop_coords_ui")),
        shiny::hr(),
        shiny::selectInput(
          ns("statement_type"),
          "Statement type",
          choices = c("(loading...)" = "")
        ),
        shiny::div(
          class = "d-flex gap-2 align-items-center",
          shiny::actionButton(
            ns("add_type_toggle"), "Add type\u2026",
            class = "btn-sm btn-outline-secondary"
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] %% 2 != 0", ns("add_type_toggle")),
          shiny::div(
            class = "d-flex gap-2 mt-2",
            shiny::textInput(ns("new_type_name"), NULL,
                             placeholder = "New statement type name"),
            shiny::actionButton(ns("btn_add_type"), "Add",
                                class = "btn-sm btn-success")
          )
        ),
        shiny::hr(),
        shinybusy::add_busy_spinner(spin = "fading-circle", color = "#0d6efd",
                                    position = "bottom-right"),
        shiny::actionButton(
          ns("btn_run_extract"), "Run extraction",
          class = "btn-primary w-100",
          icon  = shiny::icon("bolt")
        ),
        shiny::uiOutput(ns("extract_status_ui"))
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

#' Extract module server
#'
#' Admin gate enforced at the server level (FR-UP-00).
#'
#' @param id        Module namespace id.
#' @param pool      Pool object from [db_pool_connect()].
#' @param upload_rv Reactive returned by [mod_upload_server()].
#' @param user_role Reactive or character role string.
#' @return A reactive list `list(result, statement_type)` where `result` is
#'   the list returned by [extract()], or `NULL` if extraction has not run.
#' @export
mod_extract_server <- function(id, pool, upload_rv, user_role) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Admin gate ------------------------------------------------------------
    shiny::observe({
      role <- if (shiny::is.reactive(user_role)) user_role() else user_role
      if (!is_admin(role)) {
        shiny::showNotification("Extraction requires admin access.",
                                type = "error", duration = 5L)
      }
    })

    rv <- shiny::reactiveValues(
      result       = NULL,
      page_wh_pts  = NULL,    # list(width, height) in PDF points
      img_src      = NULL     # URL for the page image
    )

    # -- Resource path for temp images -----------------------------------------
    # Serve temp PNG files via a static resource path (avoids base64 encoding)
    img_tmp_dir <- tempfile(pattern = "fin_extract_imgs_")
    dir.create(img_tmp_dir, recursive = TRUE)
    shiny::addResourcePath("fin-page-imgs", img_tmp_dir)

    # -- Populate statement_type choices from DuckDB ---------------------------
    shiny::observe({
      types <- tryCatch({
        con <- pool::poolCheckout(pool)
        on.exit(pool::poolReturn(con), add = TRUE)
        DBI::dbGetQuery(con, "SELECT name FROM dim_statement_type ORDER BY name")$name
      }, error = function(e) character(0))
      shiny::updateSelectInput(session, "statement_type",
                               choices = stats::setNames(types, types))
    })

    # -- Add new statement type ------------------------------------------------
    shiny::observeEvent(input$btn_add_type, {
      nm <- trimws(input$new_type_name %||% "")
      if (!nzchar(nm)) return()
      tryCatch({
        con <- pool::poolCheckout(pool)
        on.exit(pool::poolReturn(con), add = TRUE)
        DBI::dbExecute(
          con,
          glue::glue(
            "INSERT OR IGNORE INTO dim_statement_type (name, is_builtin) VALUES ({sql_str(nm)}, FALSE)"
          )
        )
        types <- DBI::dbGetQuery(con, "SELECT name FROM dim_statement_type ORDER BY name")$name
        shiny::updateSelectInput(session, "statement_type",
                                 choices = stats::setNames(types, types),
                                 selected = nm)
        shiny::updateTextInput(session, "new_type_name", value = "")
        shiny::showNotification(paste("Added:", nm), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # -- Render page preview and initialise crop JS ----------------------------
    shiny::observe({
      up <- upload_rv()
      if (is.null(up) || is.null(up$pdf_path)) return()

      # Page dimensions in PDF points
      psz <- tryCatch({
        info <- pdftools::pdf_info(up$pdf_path)
        list(width = info$pagesize[["width"]], height = info$pagesize[["height"]])
      }, error = function(e) list(width = 595, height = 842))
      rv$page_wh_pts <- psz

      # Render page to temp PNG
      tmp_name <- paste0("page_", up$page_from, "_",
                         digest::digest(up$pdf_path), ".png")
      tmp_file <- file.path(img_tmp_dir, tmp_name)
      if (!file.exists(tmp_file)) {
        tryCatch(
          pdftools::pdf_convert(
            up$pdf_path, format = "png", pages = up$page_from,
            dpi = 120L, filenames = tmp_file, verbose = FALSE
          ),
          error = function(e) NULL
        )
      }
      rv$img_src <- paste0("fin-page-imgs/", tmp_name)
    })

    # Image HTML rendered inside the crop-container div
    output$page_img_ui <- shiny::renderUI({
      req(rv$img_src)
      shiny::tagList(
        shiny::tags$img(
          src   = rv$img_src,
          style = "display:block;width:100%;pointer-events:none;",
          alt   = "PDF page"
        )
      )
    })

    # Init crop JS after image renders
    shiny::observe({
      req(rv$img_src)
      session$sendCustomMessage("initCropSelect", list(
        containerId = ns("page_container"),
        inputId     = ns("crop_box")
      ))
    })

    # Show current crop coordinates
    output$crop_coords_ui <- shiny::renderUI({
      cb <- input$crop_box
      if (is.null(cb) || is.null(rv$page_wh_pts)) {
        return(shiny::tags$p(
          class = "text-muted small",
          shiny::icon("info-circle"),
          " No region selected — full page will be used."
        ))
      }
      psz <- rv$page_wh_pts
      shiny::tags$div(
        class = "small font-monospace p-2 bg-light border rounded",
        shiny::tags$strong("Selected region (PDF pts):"), shiny::tags$br(),
        sprintf(
          "x: %.0f\u2013%.0f  \u00b7  y: %.0f\u2013%.0f",
          cb$x1 * psz$width, cb$x2 * psz$width,
          cb$y1 * psz$height, cb$y2 * psz$height
        ),
        shiny::actionLink(ns("clear_crop"), "Clear", class = "ms-2 small")
      )
    })

    shiny::observeEvent(input$clear_crop, {
      session$sendCustomMessage("clearCropSelect",
                                list(containerId = ns("page_container")))
      shiny::updateCheckboxInput(session, "crop_box", value = NULL)
    })

    # -- Async extraction (FR-EX-05) ------------------------------------------
    extract_task <- shiny::ExtendedTask$new(function(pdf_path, page, region) {
      promises::future_promise({
        extract(pdf_path, page = page, region = region, method = NULL)
      })
    })

    shiny::observeEvent(input$btn_run_extract, {
      up <- upload_rv()
      if (is.null(up) || is.null(up$pdf_path)) {
        shiny::showNotification("Upload a PDF first.", type = "warning")
        return()
      }
      role <- if (shiny::is.reactive(user_role)) user_role() else user_role
      if (!is_admin(role)) {
        shiny::showNotification("Admin access required.", type = "error")
        return()
      }

      region <- .crop_to_region(input$crop_box, rv$page_wh_pts)

      # For page range: extract first page; continuation handled below
      shinybusy::show_modal_spinner(
        spin = "fading-circle", color = "#0d6efd",
        text = "Extracting table\u2026"
      )
      extract_task$invoke(
        pdf_path = up$pdf_path,
        page     = up$page_from,
        region   = region
      )
    })

    shiny::observe({
      status <- extract_task$status()
      if (status == "success") {
        shinybusy::remove_modal_spinner()
        res <- extract_task$result()

        # For page range > 1: run remaining pages and stitch (FR-RG-04)
        up <- upload_rv()
        if (!is.null(up) && up$page_to > up$page_from &&
            !is.null(res$table_dt) && nrow(res$table_dt) > 0L) {
          res <- .stitch_pages(
            res,
            pdf_path     = up$pdf_path,
            page_from    = up$page_from + 1L,
            page_to      = up$page_to,
            region       = .crop_to_region(input$crop_box, rv$page_wh_pts)
          )
        }

        rv$result <- res
        shiny::showNotification(
          paste0("Extracted ", res$n_value_cols, " value column(s), ",
                 nrow(res$table_dt), " rows."),
          type = "message"
        )
      } else if (status == "error") {
        shinybusy::remove_modal_spinner()
        shiny::showNotification("Extraction failed. Check the PDF and try again.",
                                type = "error")
      }
    })

    output$extract_status_ui <- shiny::renderUI({
      req(rv$result)
      res <- rv$result
      bslib::card(
        class = "bg-success-subtle border-success mt-2",
        bslib::card_body(
          class = "py-2",
          shiny::icon("circle-check"),
          sprintf(
            " Extracted: %d rows, %d value columns (method: %s)",
            nrow(res$table_dt), res$n_value_cols, res$method
          )
        )
      )
    })

    # -- Return reactive -------------------------------------------------------
    shiny::reactive({
      if (is.null(rv$result)) return(NULL)
      list(
        result         = rv$result,
        statement_type = input$statement_type %||% ""
      )
    })
  })
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Convert JS fractional crop_box to PDF-point region list
.crop_to_region <- function(crop_box, page_wh_pts) {
  if (is.null(crop_box) || is.null(page_wh_pts)) return(NULL)
  list(
    xmin = crop_box$x1 * page_wh_pts$width,
    xmax = crop_box$x2 * page_wh_pts$width,
    ymin = crop_box$y1 * page_wh_pts$height,
    ymax = crop_box$y2 * page_wh_pts$height
  )
}

# Stitch continuation pages into an existing result (FR-RG-04)
.stitch_pages <- function(base_result, pdf_path, page_from, page_to, region) {
  combined <- data.table::copy(base_result$table_dt)
  max_order <- max(combined$line_item_order, 0L)

  for (pg in seq(page_from, page_to)) {
    cont_res <- tryCatch(
      extract(pdf_path, page = pg, region = region, method = NULL),
      error = function(e) NULL
    )
    if (is.null(cont_res) || nrow(cont_res$table_dt) == 0L) next
    cont_dt <- data.table::copy(cont_res$table_dt)
    cont_dt[, line_item_order := line_item_order + max_order]
    cont_dt[, continuation := TRUE]
    if (!("continuation" %in% names(combined))) combined[, continuation := FALSE]
    combined <- data.table::rbindlist(list(combined, cont_dt),
                                      use.names = TRUE, fill = TRUE)
    max_order <- max(combined$line_item_order, 0L)
  }

  base_result$table_dt <- combined
  base_result
}
