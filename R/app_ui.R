#' Application UI
#'
#' @param request Internal shiny parameter.
#' @importFrom shiny NS tagList
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "Financial Statement Extraction",
      theme = bslib::bs_theme(version = 5),
      id    = "main_navbar",

      # --- Ingestion pipeline (admin only — server-enforced, UI also gated) ---
      bslib::nav_panel(
        "Upload",
        shiny::icon("file-arrow-up"),
        mod_upload_ui("upload")
      ),
      bslib::nav_panel(
        "Extract",
        shiny::icon("crop"),
        mod_extract_ui("extract")
      ),
      bslib::nav_panel(
        "Edit & Save",
        shiny::icon("table"),
        mod_edit_ui("edit")
      ),

      bslib::nav_spacer(),

      # --- Read surfaces (all users) ------------------------------------------
      bslib::nav_panel(
        "Browse",
        shiny::icon("magnifying-glass"),
        mod_browse_ui("browse")
      )
    )
  )
}

#' Add external resources (CSS, JS)
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path      = app_sys("app/www"),
      app_title = "fin.extract"
    )
  )
}
