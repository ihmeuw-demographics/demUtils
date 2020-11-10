#' @title Interpolate
#'
#' @description Interpolate to dimensions missing in a data.table, between
#'   dimensions which exist. Uses linear interpolation.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data you would like to interpolate. Can either have NAs or be missing
#'   rows (implicit NAs).
#' @param id_cols \[`characher()`\]\cr
#'   Columns which uniquely identify rows of `dt`. Interpolation will be
#'   done separately by group on `id_cols` excluding `interpolate_col`.
#' @param interpolate_col \[`characher(1)`\]\cr
#'   Name of column of `dt` which is a numeric variable defining the groups
#'   you would like to interpolate for.
#' @param value_col \[`characher(1)`\]\cr
#'   Name of a column of `dt` which is a numeric variable defining the values
#'   you would like to interpolate.
#' @param interpolate_vals \[`numeric(1)`\]\cr
#'   The values of `interpolate_col` for which you would like to solve for
#'   an interpolated value of `value_col`.
#' @param ... Other arguments to be passed to `stats::approx`.
#'
#' @details This function uses `stats::approx` to solve a linear interpolation.
#'   Values outside the bounds of known data will be returned as NA. Use
#'   [demUtils::extrapolate()] to get values outside of the bounds of the data.
#'   Consider log transforming your data prior to interpolation if
#'   appropriate.
#'
#' @return \[`data.table()`\] `dt` with added rows for interpolated values.
#'
#' @examples
#' dt <- data.table::data.table(
#'   group = c(rep("a", 5), rep("b", 5)),
#'   x = c(1, 3, 4, 5, 10, 1, 2, 6, 8, 10),
#'   y = c(10, 30, 40, 50, 100, 10, 20, 60, 80, 100)
#' )
#' dt <- interpolate(dt, id_cols = c("group", "x"), interpolate_col = "x",
#'                   value_col = "y", interpolate_vals = c(1:10))
#'
#' @export
interpolate <- function(dt,
                        id_cols,
                        interpolate_col,
                        value_col,
                        interpolate_vals,
                        ...) {

  dt <- copy(dt)

  # validate ----------------------------------------------------------------

  # check `id_cols`
  assertthat::assert_that(is.character(id_cols))

  # check `dt`
  assertable::assert_colnames(
    dt, c(id_cols, value_col), only_colnames = F, quiet = T
  )

  # check `interpolate_col`
  assertthat::assert_that(
    assertthat::is.string(interpolate_col),
    interpolate_col %in% id_cols,
    is.numeric(dt[, get(interpolate_col)]),
    msg = paste0("`interpolate_col` must be an entry of `id_cols` and ",
                 "represent a numeric column of `dt`.")
  )

  # check `value_col`
  assertthat::assert_that(
    assertthat::is.string(value_col),
    value_col %in% names(dt),
    is.numeric(dt[, get(value_col)]),
    msg = "`value_col` must represent a numeric column of `dt`."
  )

  # check `interpolate_vals`
  assertthat::assert_that(is.numeric(interpolate_vals))
  lb <- min(dt[, get(interpolate_col)])
  ub <- max(dt[, get(interpolate_col)])
  assertthat::assert_that(
    between(min(interpolate_vals), lb, ub) |
      between(max(interpolate_vals), lb, ub),
    msg = paste0("There is no overlap between the range of your data and ",
                 "the range of `interpolate_vals`.")
  )

  # interpolate -------------------------------------------------------------

  dt <- dt[
    ,
    list(
      x = interpolate_vals,
      y = stats::approx(
        x = get(interpolate_col),
        y = get(value_col),
        xout = interpolate_vals,
        ...
      )$y
    ),
    by = setdiff(id_cols, interpolate_col)
  ]

  return(dt)

}


#' @title Extrapolate
#'
#' @description Extrapolate to dimensions beyond the range of those which
#'   exist in your data.table. Uses linear, rate of change, or uniform
#'   extrapolation.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data you would like to extrapolate.
#' @param id_cols \[`characher()`\]\cr
#'   Columns which uniquely identify rows of `dt`. Extrapolation will be
#'   done separately by group on `id_cols` excluding `interpolate_col`.
#' @param extrapolate_col \[`characher(1)`\]\cr
#'   Name of column of `dt` which is a numeric variable defining the groups
#'   you would like to extrapolate for.
#' @param value_col \[`characher(1)`\]\cr
#'   Name of a column of `dt` which is a numeric variable defining the values
#'   you would like to extrapolate.
#' @param extrapolate_vals \[`numeric(1)`\]\cr
#'   The values of `extrapolate_col` that you would like included in the
#'   outputs, including both input values and extrapolated values.
#' @param method \[`characher(1)`\]\cr
#'   The method for extrapolation. Must be either 'linear', 'rate_of_change',
#'   or 'uniform'.
#' @param n_groups_fit \[`numeric(1)`\]\cr
#'   The number of values of `extrapolate_col` to include in determining the
#'   extrapolation. For method 'linear' this is the groups used to fit a linear
#'   model. For method 'rate_of_change' this is the groups used to calculate a
#'   mean rate of change. For method 'uniform' this is the number of groups to
#'   combine into a uniform mean value which will be filled in. The groups will
#'   be selected as the first N groups or the last N groups in the data
#'   depending on the direction of extrapolation. Example: for extrapolation
#'   from years 1990:2000 up to year 2020 with `n_groups_fit` = 5, only
#'   years 1995:2000 will be used to fit the extrapolation model.
#'
#' @details
#' For reference on rate of change, see page 12 of the Preston Demography book, or,
#' https://www.un.org/esa/sustdev/natlinfo/indicators/methodology_sheets/demographics/population_growth_rate.pdf
#'
#' @return \[`data.table()`\] `dt` with added rows for extrapolated values.
#'
#' @examples
#' dt <- data.table::data.table(
#'   group = c(rep("a", 5), rep("b", 5)),
#'   x = rep(c(1:5), 2),
#'   y = rep(seq(10, 50, 10), 2)
#' )
#' dt <- extrapolate(dt, id_cols = c("group", "x"), extrapolate_col = "x",
#'                   value_col = "y", extrapolate_vals = c(1:10),
#'                   method = "linear", n_groups_fit = 3)
#'
#' @export
extrapolate <- function(dt,
                        id_cols,
                        extrapolate_col,
                        value_col,
                        extrapolate_vals,
                        method,
                        n_groups_fit) {

  dt <- copy(dt)

  # validate ----------------------------------------------------------------

  # check `id_cols`
  assertthat::assert_that(is.character(id_cols))

  # check `dt`
  assertable::assert_colnames(
    dt, c(id_cols, value_col), only_colnames = F, quiet = T
  )

  # check `extrapolate_col`
  assertthat::assert_that(
    assertthat::is.string(extrapolate_col),
    extrapolate_col %in% id_cols,
    is.numeric(dt[, get(extrapolate_col)]),
    msg = paste0("`extrapolate_col` must be an entry of `id_cols` and ",
                 "represent a numeric column of `dt`.")
  )

  # check `value_col`
  assertthat::assert_that(
    assertthat::is.string(value_col),
    value_col %in% names(dt),
    is.numeric(dt[, get(value_col)]),
    msg = "`value_col` must represent a numeric column of `dt`."
  )

  # check `extrapolate_vals`
  assertthat::assert_that(is.numeric(extrapolate_vals))
  lb <- min(dt[, get(extrapolate_col)])
  ub <- max(dt[, get(extrapolate_col)])
  assertthat::assert_that(
    min(extrapolate_vals) < lb | max(extrapolate_vals) > ub,
    msg = "`extrapolate_vals` are not outside of the range of the data."
  )

  # TEMPORARY: check that we aren't asking for backwards extrapolation
  # TODO: update function work for backwards extrapolation (demUtils issue #26)
  assertthat::assert_that(
    !(min(extrapolate_vals) < lb),
    msg = paste0("For now, extrapolation for `extrapolate_vals` lower than ",
                 "the range of the data is not supported.")
  )

  # check `method`
  methods <- c("linear", "rate_of_change", "uniform")
  assertthat::assert_that(
    assertthat::is.string(method),
    method %in% methods,
    msg = paste0("`method` is '", method, "' but must be one of: ",
                 paste(methods, collapse = ", "))
  )

  # check `n_groups_fit`
  assertthat::assert_that(assertthat::is.count(n_groups_fit))

  # expand ------------------------------------------------------------------

  # fill in NAs
  expand_cols <- list(temp = extrapolate_vals)
  names(expand_cols) <- extrapolate_col
  dt <- expand(dt, expand_cols = expand_cols, id_cols = id_cols)

  # extrapolate -------------------------------------------------------------

  dt <- dt[order(get(extrapolate_col))]

  dt <- rbindlist(
    lapply(
      split(dt, by = setdiff(id_cols, extrapolate_col)),
      FUN = function(d) {

        setnames(d, c(extrapolate_col, value_col), c("x", "y"))

        # subset to `n_groups_fit` groups
        fit_data <- copy(d)
        fit_data <- fit_data[!is.na(y)]
        fit_data[, i := .I]
        max_i <- max(fit_data$i)
        fit_data <- fit_data[i %in% (max_i - n_groups_fit + 1):max_i]

        if (method == "linear") {

          # fit and predict linear model
          fit <- stats::lm(data = fit_data, y ~ x)
          d$y <- stats::predict(fit, newdata = d)

        } else if (method == "rate_of_change") {

          # calculate mean rate of change and project
          roc <- log(fit_data$y / shift(fit_data$y)) /
                 (fit_data$x - shift(fit_data$x))
          roc <- mean(roc, na.rm = T)
          d[, interval := x - max(fit_data$x)]
          for (i in 1:max(d$interval)) {
            d[, y_new := exp((x - shift(x)) * roc) * y[interval == i - 1]]
            d[interval == i, y := y_new]
            d[, y_new := NULL]
          }
          d[, interval := NULL]

        } else if (method == "uniform") {

          # calculate mean and set equal to this mean uniformly
          m <- mean(fit_data$y)
          d[is.na(y), y := m]

        } else {
          stop(paste0("invalid 'method': ", method))
        }

        setnames(d, c("x", "y"), c(extrapolate_col, value_col))
        return(d)
      }
    )
  )

  return(dt)

}


#' @title Expand
#'
#' @description Expands data.table to dimensions not already present, leaving
#'   value columns NA.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data set you would like to expand.
#' @param expand_cols \[`list()`\]\cr
#'   A named list of vectors, where the name of each vector is a column of `dt`
#'   that you would like to expand (or a new column name) and the value within
#'   the vector are the values you want the column to take.
#'   Ex: expand_cols = list(draw = 1:100) would mean you want the returned
#'   data.table to have draws 1 through 100.
#' @param id_cols \[`characher()`\]\cr
#'   Columns which uniquely identify rows of `dt`. These columns will be copied
#'   over into the expanded rows.
#'
#' @return `dt` with added rows for all combinations of
#'   `expand_cols` as specified. In new rows, entries of non-ID columns will
#'   be NA.
#'
#' @details
#' This function is a wrapper for [data.table::CJ()].
#'
#' Syntax for example:
#' `dt[CJ(year = 1990:2000, draw = c(1:2), location = "Mexico", unique = T),
#'    on = list(location, year)]`
#'
#' @examples
#' dt <- data.table::data.table(
#'   location = c("Mexico"),
#'   year = c(1990:1995),
#'   value = c(0:5)
#' )
#' dt <- expand(dt, expand_cols = list(year = 1990:2000, draw = c(1:2)),
#'              id_cols = c("location", "year"))
#'
#' @export
expand <- function(dt,
                   expand_cols,
                   id_cols) {

  dt <- copy(dt)

  # validate ----------------------------------------------------------------

  assertthat::assert_that(is.data.table(dt))
  assertthat::assert_that(is.character(id_cols))
  assertable::assert_colnames(dt, id_cols, only_colnames = F, quiet = T)

  # create string to pass to CJ ---------------------------------------------

  # add single-quotes around characters
  for (col in names(expand_cols)) {
    if (is.character(expand_cols[[col]])) {
      expand_cols[[col]] <- paste0("'", expand_cols[[col]], "'")
    }
  }

  # compile string defining columns and values
  expand_cols_string <- paste(lapply(names(expand_cols), function(col) {
    return(paste0(col, " = c(", paste(expand_cols[[col]], collapse = ","), ")"))
  }), collapse = ", ")

  # add on any ID columns not in `expand_cols`
  other_cols <- setdiff(id_cols, names(expand_cols))
  for (col in other_cols) {
    expand_cols_string <- paste0(
      expand_cols_string, ", ", col, "=", col
    )
  }

  # expand ------------------------------------------------------------------

  dt <- eval(parse(
    text = paste0("dt[CJ(", expand_cols_string, ", unique = TRUE), ",
                  "on = list(", paste(id_cols, collapse = ","), ")]")
  ))

  return(dt)

}
