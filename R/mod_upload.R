#' Upload module — admin-only PDF ingestion (FR-UP-00 — FR-UP-04)
#'
#' Handles:
#' - Single PDF upload, stored by SHA-256 file hash (FR-UP-01).
#' - Page thumbnail / image preview (FR-UP-02).
#' - Single page or contiguous page-range selection (FR-UP-03).
#' - Pre-fills a company/period *guess* from the filename, clearly marked
#'   unconfirmed; the filename is **never** treated as authoritative (FR-UP-04).
#'
#' @name mod_upload
NULL

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

#' Upload module UI
#'
#' @param id Module namespace id.
#' @export
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::icon("file-arrow-up"), " Upload PDF"
    ),
    bslib::card_body(
      shiny::fileInput(
        ns("pdf_file"),
        label    = NULL,
        accept   = ".pdf",
        buttonLabel = shiny::icon("folder-open"),
        placeholder = "Select a PDF\u2026"
      ),
      shiny::uiOutput(ns("guess_banner")),
      bslib::layout_column_wrap(
        width = 1 / 2,
        shiny::numericInput(
          ns("page_from"), "From page",
          value = 1L, min = 1L, step = 1L
        ),
        shiny::numericInput(
          ns("page_to"), "To page",
          value = 1L, min = 1L, step = 1L
        )
      ),
      shiny::actionButton(
        ns("btn_confirm_pages"), "Confirm page selection",
        class = "btn-primary btn-sm"
      ),
      shiny::hr(),
      shiny::uiOutput(ns("page_preview_area"))
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

#' Upload module server
#'
#' Admin gate enforced at the server level (FR-UP-00).
#'
#' @param id          Module namespace id.
#' @param pool        Pool object from [db_pool_connect()].
#' @param user_role   Reactive or character role string.
#' @param uploads_dir Directory where raw PDFs are stored by hash (FR-UP-01).
#' @return A reactive list:
#'   `list(pdf_path, file_hash, filename, page_from, page_to, n_pages,
#'         guess_company, guess_period)`.
#'   `NULL` if no upload yet.
#' @export
mod_upload_server <- function(id, pool, user_role, uploads_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Server-side admin gate (FR-UP-00) -------------------------------------
    shiny::observe({
      role <- if (shiny::is.reactive(user_role)) user_role() else user_role
      if (!is_admin(role)) {
        shiny::showNotification(
          "Upload requires admin access.", type = "error", duration = 5L
        )
      }
    })

    rv <- shiny::reactiveValues(
      pdf_path     = NULL,
      file_hash    = NULL,
      filename     = NULL,
      n_pages      = NULL,
      page_from    = 1L,
      page_to      = 1L,
      guess_company = NA_character_,
      guess_period  = NA_character_,
      preview_page  = 1L
    )

    # -- Handle file upload -----------------------------------------------------
    shiny::observeEvent(input$pdf_file, {
      role <- if (shiny::is.reactive(user_role)) user_role() else user_role
      if (!is_admin(role)) return()

      f <- input$pdf_file
      if (is.null(f)) return()

      hash <- tryCatch(
        digest::digest(f$datapath, algo = "sha256", file = TRUE),
        error = function(e) digest::digest(f$name)
      )
      if (!dir.exists(uploads_dir)) dir.create(uploads_dir, recursive = TRUE)
      dest <- file.path(uploads_dir, paste0(hash, ".pdf"))
      if (!file.exists(dest)) file.copy(f$datapath, dest)

      n_pg <- tryCatch(
        pdftools::pdf_info(dest)$pages,
        error = function(e) 1L
      )

      guess <- .guess_from_filename(f$name)

      rv$pdf_path      <- dest
      rv$file_hash     <- hash
      rv$filename      <- f$name
      rv$n_pages       <- n_pg
      rv$page_from     <- 1L
      rv$page_to       <- 1L
      rv$guess_company <- guess$company
      rv$guess_period  <- guess$period
      rv$preview_page  <- 1L

      shiny::updateNumericInput(session, "page_from", value = 1L, max = n_pg)
      shiny::updateNumericInput(session, "page_to",   value = 1L, max = n_pg)
    })

    # -- Confirm page selection -------------------------------------------------
    shiny::observeEvent(input$btn_confirm_pages, {
      if (is.null(rv$n_pages)) return()
      pf <- max(1L, as.integer(input$page_from))
      pt <- min(rv$n_pages, max(pf, as.integer(input$page_to)))
      rv$page_from    <- pf
      rv$page_to      <- pt
      rv$preview_page <- pf
    })

    # -- Guess banner (FR-UP-04) ------------------------------------------------
    output$guess_banner <- shiny::renderUI({
      req(rv$filename)
      shiny::tagList(
        bslib::card(
          class = "bg-warning-subtle border-warning mb-2",
          bslib::card_body(
            shiny::tags$small(
              shiny::icon("triangle-exclamation"),
              " Unconfirmed guess from filename (not authoritative):"
            ),
            shiny::tags$div(
              shiny::tags$strong("Company: "),
              rv$guess_company %||% "(unknown)",
              shiny::tags$span(class = "guess-badge", "GUESS"),
              shiny::tags$strong(" \u00b7 Period: "),
              rv$guess_period %||% "(unknown)",
              shiny::tags$span(class = "guess-badge", "GUESS")
            )
          )
        )
      )
    })

    # -- Page preview -----------------------------------------------------------
    output$page_preview_area <- shiny::renderUI({
      req(rv$pdf_path)
      shiny::tagList(
        shiny::tags$div(
          shiny::tags$small(
            shiny::icon("images"),
            " Preview \u2014 page ", rv$preview_page,
            " of ", rv$n_pages
          ),
          shiny::tags$div(
            style = "display:flex;gap:4px;margin:4px 0;",
            shiny::actionButton(ns("prev_page"), shiny::icon("chevron-left"),
                                class = "btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("next_page"), shiny::icon("chevron-right"),
                                class = "btn-sm btn-outline-secondary")
          )
        ),
        shiny::imageOutput(ns("page_img"), width = "100%", height = "auto")
      )
    })

    shiny::observeEvent(input$prev_page, {
      if (!is.null(rv$preview_page) && rv$preview_page > 1L) {
        rv$preview_page <- rv$preview_page - 1L
      }
    })
    shiny::observeEvent(input$next_page, {
      if (!is.null(rv$preview_page) && rv$preview_page < (rv$n_pages %||% 1L)) {
        rv$preview_page <- rv$preview_page + 1L
      }
    })

    output$page_img <- shiny::renderImage({
      req(rv$pdf_path, rv$preview_page)
      tmp <- tempfile(fileext = ".png")
      pdftools::pdf_convert(
        rv$pdf_path,
        format    = "png",
        pages     = rv$preview_page,
        dpi       = 120L,
        filenames = tmp,
        verbose   = FALSE
      )
      list(src = tmp, contentType = "image/png",
           width = "100%", alt = paste("Page", rv$preview_page))
    }, deleteFile = TRUE)

    # -- Return reactive state --------------------------------------------------
    shiny::reactive({
      if (is.null(rv$pdf_path)) return(NULL)
      list(
        pdf_path     = rv$pdf_path,
        file_hash    = rv$file_hash,
        filename     = rv$filename,
        page_from    = rv$page_from,
        page_to      = rv$page_to,
        n_pages      = rv$n_pages,
        guess_company = rv$guess_company,
        guess_period  = rv$guess_period
      )
    })
  })
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract a company/period guess from a PDF filename (FR-UP-04)
#'
#' Returns named list `list(company, period)`.  Both may be `NA_character_`.
#' Never treat the result as authoritative.
#'
#' @param filename Character filename (basename).
#' @return Named list with `company` and `period` (character or NA).
#' @export
.guess_from_filename <- function(filename) {
  base <- tools::file_path_sans_ext(basename(filename))
  # Extract 4-digit year (19xx or 20xx), not adjacent to other digits
  year_m <- regmatches(base, regexpr("(?<![0-9])(19|20)\\d{2}(?![0-9])", base, perl = TRUE))
  guess_period <- if (length(year_m) > 0L) year_m[[1L]] else NA_character_
  # Company: leading alpha word(s) before first digit/separator
  co_m <- regmatches(base, regexpr("^[A-Za-z][A-Za-z ]+", base))
  guess_company <- if (length(co_m) > 0L) trimws(co_m[[1L]]) else NA_character_
  list(company = guess_company, period = guess_period)
}
