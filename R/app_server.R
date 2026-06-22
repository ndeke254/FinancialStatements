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
  uploads_dir <- golem::get_golem_options("uploads_dir") %||%
    file.path(getwd(), "data", "uploads")
  # v1 identity: user resolved from config or dim_user seed
  current_user_name <- golem::get_golem_options("current_user") %||% "admin"

  pool <- db_pool_connect(db_path = db_path, lake_dir = lake_dir)
  shiny::onSessionEnded(function() db_pool_disconnect(pool))

  # Resolve role once per session (decoupled from identity source — §3, v2 swap)
  user_role <- shiny::reactive({
    get_user_role(pool, current_user_name)
  })

  # --- Ingestion pipeline (admin-gated at module-server level, FR-UP-00) -----
  upload_rv  <- mod_upload_server("upload",  pool, user_role, uploads_dir)
  extract_rv <- mod_extract_server("extract", pool, upload_rv, user_role)
  mod_edit_server(
    "edit",
    pool       = pool,
    lake_dir   = lake_dir,
    upload_rv  = upload_rv,
    extract_rv = extract_rv,
    user_role  = user_role,
    user_name  = current_user_name
  )

  # --- Read surfaces (all users, FR-BR-00) -----------------------------------
  mod_browse_server("browse", pool, user_role)
}

# Compact NULL-coalescing operator (avoids rlang dependency)
`%||%` <- function(a, b) if (!is.null(a)) a else b
