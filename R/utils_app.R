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

# Make data.table operators visible to cedta() for all package code.
# Without these in NAMESPACE, [, := ] calls fail with "cedta()" errors.
#' @importFrom data.table := .SD .I .N .GRP
NULL
