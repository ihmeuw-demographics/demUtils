#' @title Rank non-unique rows in a data.table using defined priority orders
#'
#' @param dt \[`data.table()`\]\cr
#'   Data to determine rank priority for.
#' @param rank_by_cols \[`character()`\]\cr
#'   Apply `rank_order` priorities to each unique combination of `rank_by_cols`
#'   in `dt`. This should be equal to or a subset of `unique_id_cols`.
#' @param unique_id_cols \[`character()`\]\cr
#'   ID columns that once ranked by priority will uniquely identify rows of `dt`
#'   in combination with the priority column. This should be a superset of
#'   `rank_by_cols`. Default is equal to `rank_by_cols`.
#' @param rank_order \[`list()`\]\cr
#'   Named \[`list()`\] defining the priority order to use when ranking
#'   non-unique rows. Each element of `rank_order` corresponds to a column in
#'   `dt`, the prioritization is applied according to the order of elements in
#'   `rank_order`. Possible values for each column are '1' (ascending), '-1'
#'   (descending) or ordered factor levels when the column is not a numeric. See
#'   details for more information.
#' @param warn_missing_levels \[`logical(1)`\]\cr
#'   Whether to warn about missing levels for elements of `rank_order` or throw
#'   error. Default is 'FALSE' and errors out if there are missing levels.
#' @param warn_non_unique_priority \[`logical(1)`\]\cr
#'   Whether to warn about specified `rank_by_cols` & `rank_order` leading to
#'   non-unique rows of `dt` after generating 'priority' column. Default is
#'   'FALSE' and errors out if there are non-unique rows.
#' @param check_top_priority_unique_only \[`logical(1)`\]\cr
#'   When checking for non-unique rows of `dt` after generating the 'priority'
#'   column with the `rank_by_cols` & names of `rank_order`, only check the
#'   priority=1 rows. This is useful when specified `rank_order` levels are not
#'   exhaustive leading to 'NA' priorities for some rows. Default if 'FALSE' and
#'   errors out if there are any non-unique rows.
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
#' The order of elements in `rank_order` matters. The more important rules
#' should be placed earlier in `rank_order` so that they are applied first.
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
#'   unique_id_cols = c("location", "year", "age_start", "age_end"),
#'   rank_order = list(
#'     method = c("de facto", "de jure"), # prioritize 'de facto' sources highest
#'     n_age_groups = -1 # prioritize sources with more age groups
#'   )
#' )
#'
#' @export
prioritize_dt <- function(dt,
                          rank_by_cols,
                          unique_id_cols = rank_by_cols,
                          rank_order,
                          warn_missing_levels = FALSE,
                          warn_non_unique_priority = FALSE,
                          check_top_priority_unique_only = FALSE) {

  # validate inputs ---------------------------------------------------------

  checkmate::assert_logical(warn_missing_levels, len = 1)
  checkmate::assert_logical(warn_non_unique_priority, len = 1)
  checkmate::assert_logical(check_top_priority_unique_only, len = 1)

  checkmate::assert_data_table(dt)

  checkmate::assert_character(unique_id_cols)
  checkmate::assert_names(names(dt), must.include = unique_id_cols)

  checkmate::assert_character(rank_by_cols)
  checkmate::assert_names(names(dt), must.include = rank_by_cols)
  checkmate::assert_subset(rank_by_cols, choices = unique_id_cols)

  checkmate::assert_list(rank_order)
  checkmate::assert_names(names(dt), must.include = names(rank_order))

  checkmate::assert_disjunct(names(rank_order), rank_by_cols)

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
      if (length(other_levels) > 0) {
        msg <- paste0(
          "'", col, "' `rank_order` is missing levels, the priority for these levels will be 'NA'\n",
          "\t- defined levels: ", paste(col_levels, collapse = ","), "\n",
          "\t- missing levels: ", paste(other_levels, collapse = ",")
        )
        if (warn_missing_levels) {
          warning(msg)
        } else {
          stop(msg)
        }
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

  # check
  check_id_cols <- c(unique_id_cols, "priority")
  check_dt <- dt
  if (check_top_priority_unique_only) check_dt <- dt[priority == 1]
  non_unique_dt <- demUtils::identify_non_unique_dt(check_dt, check_id_cols)
  if (nrow(non_unique_dt) > 0) {
    msg <- paste0(
      "Specified `rank_by_cols`, `rank_order` & returned `priority` do not uniquely identify each row of `dt`.\n",
      "\t- use `warn_non_unique_priority=TRUE` to return `dt` and run demUtils::identify_non_unique_dt\n",
      "\t with `id_cols = c('", paste(check_id_cols, collapse = "', '"), "')`\n",
      paste0(capture.output(non_unique_dt), collapse = "\n")
    )
    if (warn_non_unique_priority) {
      warning(msg)
    } else {
      stop(msg)
    }
  }

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
