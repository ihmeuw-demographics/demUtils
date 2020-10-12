#' @title Change data.table column types
#'
#' @description Checks if `check_cols` exist in `dt` and are of type `is_fun`.
#' If any of `check_cols` are of that  type, then modifies the values using
#' `as_fun`.
#'
#' @param dt \[`data.table()`\] \cr
#' @param check_cols \[`character()`\] \cr
#'   name of columns in `dt` to check and modify.
#' @param is_fun \[`function()`\] \cr
#'   function like `is.integer()`, `is.character()` etc. to check if each of
#'   `check_cols` values evaluate to true. Default is `is.integer()`.
#' @param as_fun \[`function()`\] \cr
#'   function like `as.integer()`, `as.character()` etc. to change column values
#'   to. Default is `as.numeric()`.
#'
#' @return Modifies `dt` in place and returns `dt` invisibly.
#'
#' @examples
#' dt <- data.table::data.table(year = 1950L, age_start = 0L)
#' change_col_type(dt, c("year", "year_start", "age_start"))
#'
#' @export
change_col_type <- function(dt,
                            check_cols,
                            is_fun = is.integer,
                            as_fun = as.numeric) {

  assertive::is_data.table(dt)
  assertive::is_character(check_cols)
  assertive::is_function(is_fun)
  assertive::is_function(as_fun)

  # subset to columns in dt
  if (length(check_cols) > 0) {
    check_cols <- check_cols[check_cols %in% names(dt)]
  }

  # subset to columns of the specified type
  if (length(check_cols) > 0) {
    check_cols <- check_cols[sapply(dt[, .SD, .SD = check_cols], is_fun)]
  }

  # change column type
  if (length(check_cols) > 0) {
    dt[, (check_cols) := lapply(.SD, as_fun), .SD = check_cols]
  }
  return(invisible(dt))
}
