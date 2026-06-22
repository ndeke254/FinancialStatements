#' Application server logic
#'
#' @param input,output,session Internal shiny parameters.
#' @importFrom shiny reactive reactiveValues observe
#' @noRd
app_server <- function(input, output, session) {
  db_path <- golem::get_golem_options("db_path") %||%
    file.path(getwd(), "data", "control.duckdb")
  lake_dir <- golem::get_golem_options("lake_dir") %||%
    file.path(getwd(), "data", "lake")

  pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
  shiny::onSessionEnded(function() db_pool_disconnect(pool))

  # Shared reactive values available to all modules
  rv <- shiny::reactiveValues(
    current_user = NULL
  )
}

# Compact NULL-coalescing operator (avoids rlang dependency)
`%||%` <- function(a, b) if (!is.null(a)) a else b
