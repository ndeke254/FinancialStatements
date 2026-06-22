#' Parse printed cell strings to numeric values (PRD §7.4)
#'
#' Converts the raw text in a financial-statement cell to a structured
#' `(value, value_type, unit)` triple, preserving the original string in
#' `value_text` for audit.  Implements the §7.4 table exactly:
#'
#' | Printed          | `value`     | `value_type` | `unit`    |
#' |------------------|-------------|--------------|-----------|
#' | `1,234`          | 1234        | currency     | KES'000   |
#' | `(10,367,887)`   | −10367887   | currency     | KES'000   |
#' | `−` or blank     | `NA`        | `NA`         | `NA`      |
#' | `13.1%`          | 13.1        | percent      | %         |
#' | `0.30` (EPS/DPS) | 0.30        | per_share    | KES       |
#'
#' @name parse_numbers
NULL

# ---------------------------------------------------------------------------
# Regex constants
# ---------------------------------------------------------------------------

# Dash variants that mean N/A (not zero)
.NA_STRINGS <- c("-", "\u2013", "\u2014", "n/a", "n.a.", "nil", "—", "–")

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Parse a single printed cell string
#'
#' @param text         The raw printed string (as it appears in the PDF).
#' @param is_per_share `TRUE` when the calling row is known to be an EPS/DPS
#'   row; overrides the currency interpretation for bare decimals.
#' @param default_unit Default unit for currency cells (default `"KES'000"`).
#' @return A named list: `value` (double or `NA`), `value_type` (character or
#'   `NA`), `unit` (character or `NA`), `value_text` (the original string).
#' @export
parse_number <- function(text, is_per_share = FALSE,
                          default_unit = "KES'000") {
  raw  <- as.character(text)
  trimmed <- trimws(raw)

  # ---- NA cases: blank, dash variants, explicit NA -------------------------
  if (is.na(trimmed) || trimmed == "" ||
      tolower(trimmed) %in% .NA_STRINGS) {
    return(.parse_result(NA_real_, NA_character_, NA_character_, raw))
  }

  # ---- Percentage: ends with % (possibly with trailing space) ---------------
  if (grepl("%\\s*$", trimmed)) {
    num_str <- gsub("[%,\\s]", "", trimmed)
    v <- suppressWarnings(as.double(num_str))
    return(.parse_result(v, "percent", "%", raw))
  }

  # ---- Negative in parentheses: (1,234) or (1,234,567) ----------------------
  if (grepl("^\\s*\\(.*\\)\\s*$", trimmed)) {
    num_str <- gsub("[(),\\s,]", "", trimmed)   # strip parens, commas, spaces
    num_str <- gsub(",", "", num_str)
    v <- suppressWarnings(as.double(num_str))
    neg_v <- if (is.na(v)) NA_real_ else -v
    return(.parse_result(neg_v, "currency", default_unit, raw))
  }

  # ---- Strip thousands commas; attempt numeric parse -----------------------
  num_str <- gsub(",", "", trimmed)
  v <- suppressWarnings(as.double(num_str))

  if (is.na(v)) {
    # Unrecognised pattern — keep raw text, return NA
    return(.parse_result(NA_real_, NA_character_, NA_character_, raw))
  }

  # ---- Per-share (EPS/DPS row context) -------------------------------------
  if (is_per_share) {
    return(.parse_result(v, "per_share", "KES", raw))
  }

  # ---- Plain currency -------------------------------------------------------
  .parse_result(v, "currency", default_unit, raw)
}

#' Parse a vector of printed cell strings (vectorised)
#'
#' Applies [parse_number()] to each element and returns a `data.table` with
#' one row per input element.
#'
#' @param texts        Character vector of raw printed strings.
#' @param is_per_share Scalar or logical vector (recycled) — `TRUE` for
#'   EPS/DPS rows.
#' @param default_unit Scalar or character vector (recycled) — default unit
#'   for currency cells.
#' @return A `data.table` with columns `value`, `value_type`, `unit`,
#'   `value_text` and the same length as `texts`.
#' @export
parse_numbers <- function(texts, is_per_share = FALSE,
                           default_unit = "KES'000") {
  n            <- length(texts)
  is_per_share <- rep_len(is_per_share, n)
  default_unit <- rep_len(default_unit, n)

  results <- mapply(
    parse_number,
    text         = texts,
    is_per_share = is_per_share,
    default_unit = default_unit,
    SIMPLIFY     = FALSE
  )

  data.table::rbindlist(results)
}

# ---------------------------------------------------------------------------
# Internal
# ---------------------------------------------------------------------------

.parse_result <- function(value, value_type, unit, value_text) {
  list(
    value      = value,
    value_type = value_type,
    unit       = unit,
    value_text = value_text
  )
}
