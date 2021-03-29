#' @title Rank non-unique rows in a data.table using defined priority orders
#'
#' @param dt \[`data.table()`\]\cr
#'   Data to determine rank priority for.
#' @param rank_by_cols \[`character()`\]\cr
#'   Apply `rank_order` priorities to each unique combination of `rank_by_cols`
#'   in `dt`.
#' @param rank_order \[`list()`\]\cr
#'   Priority order to use when ranking non-unique rows. Each element of
#'   `rank_order` corresponds to a column in `dt`. Possible values for each
#'   column are '1' (ascending), '-1' (descending) or ordered factor levels when
#'   the column is not a numeric. See details for more information.
#' @param quiet \[`logical(1)`\]\cr
#'   Whether to print out detailed messages/warnings about possible issues with
#'   `rank_order`. Default is 'FALSE'.
#'
#' @return `dt` with a new 'priority' column generated using the rules specified
#'   in `rank_order`. 'priority' equal to 1 is the highest priority
#'
#' @details
#' `prioritize_dt` uses `data.table::setorderv` to order `dt` according to
#' `rank_order`. `prioritize_dt` takes three possible values to specify the
#' order of a column in `dt`.
#' 1. '1', order a numeric column in ascending order (smaller values have higher
#' priority).
#' 2. '-1', order a numeric column in descending order (larger values have
#' higher priority).
#' 3. `factor` levels, to order a categorical column in a custom order with the
#' first level having highest priority. When not all present values of the
#' column are defined in the levels, the priority will be NA and a warning
#' printed if `quiet = FALSE`.
#'
#' @examples
#' # preliminary data with only total population
#' dt_total <- data.table::CJ(
#'   location = "USA", year = 2000, age_start = 0, age_end = Inf,
#'   method = c("de facto", "de jure"),
#'   status = c("preliminary")
#' )
#' # final data in 10 year age groups
#' dt_10_yr_groups <- data.table::CJ(
#'   location = "USA", year = 2000, age_start = seq(0, 80, 10),
#'   method = c("de facto", "de jure"),
#'   status = c("final")
#' )
#' dt_10_yr_groups[, age_end := age_start + 10]
#' dt_10_yr_groups[age_start == 80, age_end := Inf]
#'
#' input_dt <- rbind(dt_total, dt_10_yr_groups)
#' input_dt[, n_age_groups := .N, by = setdiff(names(input_dt), c("age_start", "age_end"))]
#'
#' output_dt <- prioritize_dt(
#'   dt = input_dt,
#'   rank_by_cols = c("location", "year"),
#'   rank_order = list(
#'     method = c("de facto", "de jure"), # prioritize 'de facto' sources highest
#'     n_age_groups = -1 # prioritize sources with more age groups
#'   )
#' )
#'
#' @export
prioritize_dt <- function(dt, rank_by_cols, rank_order, quiet = FALSE) {

  # validate inputs ---------------------------------------------------------

  checkmate::assert_data_table(dt)
  checkmate::assert_logical(quiet, len = 1)

  checkmate::assert_character(rank_by_cols)
  checkmate::assert_names(names(dt), must.include = rank_by_cols)

  checkmate::assert_list(rank_order)
  checkmate::assert_names(names(dt), must.include = names(rank_order))

  rank_order <- copy(rank_order)
  original_col_order <- names(dt)
  original_keys <- key(dt)

  # Prioritize dataset ------------------------------------------------------

  priority_dt <- unique(dt[, c(rank_by_cols, names(rank_order)), with = F])

  # reformat categorical columns as factors with defined order in
  for (col in names(rank_order)) {
    col_levels <- rank_order[[col]]

    if (!checkmate::test_choice(col_levels, choices = c(1, -1))) {

      checkmate::assert_vector(col_levels)

      # check for non-defined levels for the categorical column
      other_levels <- setdiff(unique(priority_dt[[col]]), col_levels)
      if (!quiet) {
        warning(
        "'", col, "' `rank_order` is missing levels, the priority for these levels will be 'NA'\n",
        "\t- defined levels: ", paste(col_levels, collapse = ","), "\n",
        "\t- missing levels: ", paste(other_levels, collapse = ",")
        )
      }

      priority_dt[, c(col) := factor(get(col), levels = col_levels)]
      # assumes 'col_levels' is already sorted with highest priority first
      rank_order[[col]] <- 1
    }
  }

  # order based on specified rank order
  data.table::setorderv(
    x = priority_dt, cols = names(rank_order),
    order = unlist(rank_order),
    na.last = TRUE
  )
  priority_dt[, priority := seq(.N), by = rank_by_cols]

  # add priority rank back onto original dataset
  dt <- merge(dt, priority_dt, by = setdiff(names(priority_dt), "priority"), all.x = TRUE)

  # format output
  data.table::setcolorder(dt, c(original_col_order, "priority"))
  if (is.null(original_keys)) {
    original_keys <- c(rank_by_cols, "priority")
  } else {
    original_keys <- c(original_keys, "priority")
  }
  data.table::setkeyv(dt, original_keys)

  return(dt)
}
