#' Resolve a path inside the installed package's `inst/` directory
#'
#' Equivalent to `system.file(..., package = "fin.extract")`.  Used by
#' `app_ui` to locate `inst/app/www/` for static assets.
#'
#' @param ... Path components passed to [system.file()].
#' @return A character string with the resolved path.
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "fin.extract")
}
