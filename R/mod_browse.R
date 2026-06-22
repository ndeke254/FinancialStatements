#' Catalog browser module (FR-BR-00 — FR-BR-03)
#'
#' Standard-user read surface.  Lists confirmed statements from the DuckDB
#' catalog with filter controls.  Reads only the catalog table — never a
#' full lake scan (FR-BR-02).  Available to standard users and admins
#' (FR-BR-00).
#'
#' @name mod_browse
NULL

#' Catalog browser UI
#'
#' @param id Module namespace id.
#' @export
mod_browse_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Saved Statements"),
    bslib::layout_column_wrap(
      width = 1 / 4,
      shiny::textInput(
        ns("filter_company"), "Company",
        placeholder = "All companies"
      ),
      shiny::selectInput(
        ns("filter_type"), "Statement type",
        choices = c(
          "All" = "",
          "income_statement", "balance_sheet", "cash_flow",
          "changes_in_equity", "other_disclosures", "combined_statement"
        )
      ),
      shiny::numericInput(
        ns("filter_year"), "Fiscal year",
        value = NA_real_, min = 1990L, max = 2100L, step = 1L
      ),
      shiny::selectInput(
        ns("filter_status"), "Audit status",
        choices = c(
          "All" = "",
          "audited", "unaudited", "reviewed", "restated", "proforma"
        )
      )
    ),
    reactable::reactableOutput(ns("catalog_tbl"))
  )
}

#' Catalog browser server
#'
#' Accessible to both standard users and admins (FR-BR-00).
#'
#' @param id        Module namespace id.
#' @param pool      Pool object from [db_pool_connect()].
#' @param user_role Reactive or plain character with the current user role.
#' @export
mod_browse_server <- function(id, pool, user_role = "standard") {
  shiny::moduleServer(id, function(input, output, session) {

    catalog_data <- shiny::reactive({
      company <- if (nzchar(input$filter_company %||% "")) input$filter_company else NULL
      type    <- if (nzchar(input$filter_type    %||% "")) input$filter_type    else NULL
      year_v  <- input$filter_year
      year    <- if (!is.null(year_v) && !is.na(year_v)) as.integer(year_v) else NULL
      status  <- if (nzchar(input$filter_status  %||% "")) input$filter_status  else NULL

      catalog_list(
        pool,
        company      = company,
        type         = type,
        fiscal_year  = year,
        audit_status = status
      )
    })

    output$catalog_tbl <- reactable::renderReactable({
      df <- catalog_data()
      reactable::reactable(
        df,
        filterable      = TRUE,
        searchable      = TRUE,
        pagination      = TRUE,
        defaultPageSize = 20L,
        columns = list(
          catalog_id        = reactable::colDef(show = FALSE),
          company           = reactable::colDef(name = "Company"),
          entity            = reactable::colDef(name = "Entity"),
          statement_type    = reactable::colDef(name = "Type"),
          period_end        = reactable::colDef(name = "Period end"),
          audit_status      = reactable::colDef(name = "Audit status"),
          source_file       = reactable::colDef(name = "Source file"),
          validation_passed = reactable::colDef(name = "Valid?"),
          n_rows            = reactable::colDef(name = "Rows"),
          extracted_at      = reactable::colDef(name = "Extracted at"),
          lake_path         = reactable::colDef(show = FALSE)
        )
      )
    })
  })
}
