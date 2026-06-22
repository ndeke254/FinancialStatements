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
      id = "main_navbar"
      # Panels added per milestone
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
  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "fin.extract"
    )
  )
}
