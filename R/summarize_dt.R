#' @title Calculate summary statistics
#'
#' @description Calculate summary statistics when collapsing over a certain
#'   variable. For example can calculate summary statistics across a set of
#'   draws, or locations, or location-years, etc.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data to calculate summary statistics for.
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`.
#' @param summarize_cols \[`character()`\]\cr
#'   The `id_cols` that should be collapsed and to calculate summary statistics
#'   over.
#' @param value_cols \[`character()`\]\cr
#'   Value columns that summary statistics should be calculated for. When more
#'   than one column is specified, each of the summary statistic columns that
#'   are returned are prefixed with the value column name.
#' @param summary_fun \[`character()`\]\cr
#'   Names of the functions that can be used to summarize a vector of values.
#'   Default is "mean".
#' @param probs \[`numeric()`\]\cr
#'   Probabilities with values in `[0,1]` to be used when producing sample
#'   quantiles with `stats::quantile()`. Default is 0.025 and 0.975 for the
#'   2.5th and 97.5th quantiles. Can be NULL if no quantiles are needed.
#'
#' @return \[`data.table()`\] with `id_cols` (minus the `summarize_cols`) plus
#'   summary statistic columns. The summary statistic columns have the same name
#'   as each function specified in `summary_fun` and the quantiles are named
#'   like 'q_`(probs * 100)`'. If more than one `value_cols` is specified, each
#'   of the summary statistic columns that are returned are prefixed with the
#'   value column name.
#'
#' @details
#' `summary_fun` correspond to names of functions in R that can take a vector of
#' values and reduce to one summary statistic. This can also include user
#' defined functions specified in the global environment.
#'
#' The `probs` argument is used to specify the probabilities to calculate sample
#' quantiles for using the `stats::quantile()` function. The default 2.5 and
#' 97.5 quantiles would have columns named 'q2.5' and 'q97.5' in the returned
#' data.table.
#'
#' @examples
#' input_dt <- data.table::data.table(location = "USA", draw = 1:101, value = 0:100)
#' output_dt <- summarize_dt(
#'   dt = input_dt,
#'   id_cols = c("location", "draw"),
#'   summarize_cols = "draw",
#'   value_cols = "value"
#' )
#'
#' # no quantiles calculated
#' output_dt <- summarize_dt(
#'   dt = input_dt,
#'   id_cols = c("location", "draw"),
#'   summarize_cols = "draw",
#'   value_cols = "value",
#'   probs = NULL
#')
#'
#' @importFrom methods existsFunction
#' @export
summarize_dt <- function(dt,
                         id_cols,
                         summarize_cols,
                         value_cols,
                         summary_fun = c("mean"),
                         probs = c(0.025, 0.975)) {

  # Validate arguments ------------------------------------------------------

  # check `summarize_cols` argument
  assertive::assert_is_character(summarize_cols)

  # check `value_cols` argument
  assertive::assert_is_character(value_cols)

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that(all(summarize_cols %in% id_cols),
                          msg = "`id_cols` must include `summarize_cols`")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(id_cols, value_cols), only_colnames = F,
                              quiet = T)
  assert_is_unique_dt(dt, id_cols)

  # check `summary_fun` argument
  assertthat::assert_that(assertive::is_character(summary_fun) |
                            assertive::is_empty(summary_fun),
                          all(sapply(summary_fun, methods::existsFunction)),
                          msg = "`summary_fun` must be a correspond to a defined
                          function")

  # check `probs` argument
  assertthat::assert_that(assertive::is_numeric(probs) |
                            assertive::is_empty(probs),
                          all(data.table::between(probs, 0, 1)),
                          msg = "`probs`` must be between 0 and 1
                          (or empty/null)")
  quantile_names <- paste0("q", probs * 100)

  # Calculate summary statistics --------------------------------------------

  by_id_cols <- id_cols[!id_cols %in% summarize_cols]

  original_keys <- copy(key(dt))
  original_keys <- original_keys[!original_keys %in% summarize_cols]
  if (is.null(original_keys)) original_keys <- by_id_cols

  funs <- sapply(summary_fun, get)

  summaries <- lapply(value_cols, function(value_col) {
    summary <- dt[
      ,
      c(
        if (length(summary_fun) > 0) lapply(funs, function(fun) fun(get(value_col))),
        if (length(probs) > 0) as.list(stats::quantile(get(value_col), probs = probs))
      ),
      by = by_id_cols
    ]

    summary_value_cols <- c(summary_fun, if (length(probs) > 0) quantile_names)
    if (length(value_cols) > 1) {
      summary_value_cols <- paste0(value_col, "_", summary_value_cols)
    }

    data.table::setnames(summary, c(by_id_cols, summary_value_cols))
    data.table::setkeyv(summary, original_keys)
  })
  summaries <- Reduce(f = merge, x = summaries)

  return(summaries)
}
