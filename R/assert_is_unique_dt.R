#' @title Are the rows of the input data.table unique?
#'
#' @description Checks to see if the rows of the input data.table are unique for each
#' combination of the id columns.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data.table to check rows are unique.
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`.
#'
#' @return `identify_non_unique_dt` returns a data.table with problematic rows
#'   only, includes a 'count' column identifying how many times each combination
#'   of id columns is in the dataset. `assert_is_unique_dt` returns nothing but
#'   throws an error if `identify_non_unique_dt` returns a non empty data.table.
#'
#' @seealso `assertable::assert_ids` to check that all unique combinations of
#'   specified id variables list, this is appropriate when you expect your dataset
#'   to be square.
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    value1 = 2, value2 = 4)
#' id_cols <- c("location", "year", "sex", "age_start")
#' non_unique_dt <- identify_non_unique_dt(input_dt, id_cols)
#' assert_is_unique_dt(input_dt, id_cols)
#'
#' @export
assert_is_unique_dt <- function(dt, id_cols) {

  non_unique_dt <- identify_non_unique_dt(dt, id_cols)
  is_unique_dt <- nrow(non_unique_dt) == 0

  error_msg <- "Input data rows are not unique for each combination of the id
  columns. Use `identify_non_unique_dt` to see which data is problematic."
  assertthat::assert_that(is_unique_dt, msg = error_msg)
}

#' @rdname assert_is_unique_dt
#' @export
identify_non_unique_dt <- function(dt, id_cols) {

  # Validate arguments ------------------------------------------------------

  # check `id_cols` argument
  checkmate::assert_character(id_cols)

  # check `dt` argument
  checkmate::assert_data_table(dt)
  checkmate::assert_names(names(dt), must.include = id_cols)

  # Count number of rows in each combination of `id_cols` -------------------

  non_unique_dt <- dt[duplicated(dt, by = id_cols)]
  non_unique_dt <- non_unique_dt[, id_cols, with = FALSE]

  setcolorder(non_unique_dt, id_cols)
  setkeyv(non_unique_dt, id_cols)
  return(non_unique_dt)
}
