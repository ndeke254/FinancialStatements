#' Authorization helpers (FR-UP-00, §3)
#'
#' Role resolution is decoupled from identity source: `get_user_role()`
#' reads from `dim_user`; the check functions (`is_admin`, `require_admin`)
#' only read the resolved role string.  Swapping the identity provider
#' (Firebase Auth v2) only changes how `user_id` is obtained before calling
#' `get_user_role()` — authorization logic stays unchanged.
#'
#' @name fct_auth
NULL

#' Resolve a user's role from the control-plane dim_user table
#'
#' @param pool    A pool object from [db_pool_connect()].
#' @param user_id Character matching `dim_user.name`.
#' @return `"admin"`, `"standard"`, or `NA_character_` if not found.
#' @export
get_user_role <- function(pool, user_id) {
  if (is.null(user_id) || length(user_id) == 0L ||
      is.na(user_id) || !nzchar(user_id)) {
    return(NA_character_)
  }
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  res <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT role FROM dim_user WHERE name = {sql_str(as.character(user_id))} LIMIT 1"
    )
  )
  if (nrow(res) == 0L) NA_character_ else res[["role"]][[1L]]
}

#' Check whether a role string is admin
#'
#' @param role Character role string from [get_user_role()].
#' @return `TRUE` / `FALSE`.
#' @export
is_admin <- function(role) {
  isTRUE(role == "admin")
}

#' Assert admin role — stops with an error if not admin (FR-UP-00)
#'
#' Call at the top of every admin-gated module server to enforce the gate
#' at the server level, independent of UI visibility.
#'
#' @param role Character role string.
#' @return Invisibly `TRUE` if admin.
#' @export
require_admin <- function(role) {
  if (!is_admin(role)) {
    stop("Access denied: admin role required.", call. = FALSE)
  }
  invisible(TRUE)
}
