#' @title Calculate percent change
#'
#' @description Calculate percent change from one vector (x) to another (y)
#'
#' @param x \[`numeric()`\]\cr
#'   Baseline value(s)
#' @param y \[`numeric()`\]\cr
#'   Comparison value(s)
#' @param denominator \[`numeric()`\]\cr
#'   Default to 100 for percent (ex: 20%). Specify denominator = 1 to get
#'   percent change as a decimal (ex: 0.20).
#'
#' @return \[`numeric()`\]\cr
#'   Percent change from x to y: denominator * (y - x) / x
#'
#' @examples
#' # vectors
#' pct_change(x = c(10, 11, 12), y = c(100, 22, 18))
#'
#' # data.table
#' library(data.table)
#' dt <- data.table(
#'   x = c(10, 11, 12),
#'   y = c(100, 22, 18)
#' )
#' dt[, change := pct_change(x, y)]
#'
#' # percent change within one data.table column
#' dt <- data.table(
#'   year = c(2000, 2001, 2002),
#'   val = c(10, 11, 12)
#' )
#' dt[, change := pct_change(x = shift(val), y = val)]
#'
#' @export
pct_change <- function(x, y, denominator = 100) {
  return(denominator * (y - x) / x)
}



#' @title Calculate annualized rate of change (ARC)
#'
#' @description Calculate annualized rate of change from one vector which
#'   includes numeric year and another vector which includes values of some
#'   measure for those years.
#'
#' @param years \[`numeric()`\]\cr
#'   Years corresponding to input values
#' @param values \[`numeric()`\]\cr
#'   Input values for some measure evaluated over time
#' @param denominator \[`numeric()`\]\cr
#'   Default to 100 for percent (ex: 20%). Specify denominator = 1 to get
#'   annualized rate of change as a decimal (ex: 0.20).
#'
#' @return \[`numeric()`\]\cr
#' Annualized rate of change for the intervals between elements of `years`.
#'
#' @details
#' ARC for the period between years i and j is calculated as:
#' \deqn{arc[i,j] = 100 * ln(value_j / value_i) / (year_j - year_i)}
#' Index is such that for index i, `output[i]` is arc for period `years[i - 1]`
#' to `years[i]` and the first entry in the output is `NA`.
#' This function does not sort `years` so users should check inputs are
#' ordered correctly.
#'
#' @seealso
#' - Preston Demography textbook Chapter 1
#' - https://www.un.org/esa/sustdev/natlinfo/indicators/methodology_sheets/demographics/population_growth_rate.pdf
#'
#' @examples
#' # vectors
#' arc(years = c(2000, 2005, 2010), values = c(10, 20, 25))
#'
#' # data.table
#' library(data.table)
#' dt <- data.table(
#'   year = c(2000, 2005, 2010),
#'   val = c(10, 20, 25)
#' )
#' dt[, arc := arc(years = year, values = val)]
#'
#' @export
arc <- function(years, values, denominator = 100) {

  # create offset list
  values_offset <- shift(values)
  years_offset <- shift(years)

  # calculate ARC
  output <- denominator *
            log(values - values_offset) /
            (years - years_offset)

  return(output)
}

