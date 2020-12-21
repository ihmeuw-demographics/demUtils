

# Interpolate -------------------------------------------------------------

# setup expected output for `interpolate`
expected_output <- data.table::data.table(
  group = c(rep("a", 10), rep("b", 10)),
  x = rep(1:10, 2),
  y = rep(seq(10, 100, 10), 2)
)

testthat::test_that("`interpolate` works", {

  dt <- data.table::data.table(
    group = c(rep("a", 5), rep("b", 5)),
    x = c(1, 3, 4, 5, 10, 1, 2, 6, 8, 10),
    y = c(10, 30, 40, 50, 100, 10, 20, 60, 80, 100)
  )

  output <- testthat::expect_silent(
    interpolate(dt, id_cols = c("group", "x"), interpolate_col = "x",
                value_col = "y", interpolate_vals = c(1:10))
  )
  testthat::expect_equal(output, expected_output)

})


# Extrapolate -------------------------------------------------------------

# new input
dt <- data.table::data.table(
  group = c(rep("a", 5), rep("b", 5)),
  age = rep(c(1:5), 2),
  outcome = c(seq(10, 50, 10), 9, 22, 27, 44, 51)
)

# setup expected output for linear `extrapolate`
expected_output <- data.table::data.table(
  group = c(rep("a", 8), rep("b", 8)),
  age = rep(c(1:8), 2),
  outcome = c(seq(10, 80, 10), 9, 22, 27, 44, 51, 62, 72.4, 82.8)
)

testthat::test_that("`extrapolate` works", {

  output <- testthat::expect_silent(
    extrapolate(dt, id_cols = c("group", "age"), extrapolate_col = "age",
                value_col = "outcome", extrapolate_vals = c(1:8),
                method = "linear", n_groups_fit = 4)
  )
  testthat::expect_equal(output, expected_output)

  output_roc <- testthat::expect_silent(
    extrapolate(dt, id_cols = c("group", "age"), extrapolate_col = "age",
                value_col = "outcome", extrapolate_vals = c(1:8),
                method = "rate_of_change", n_groups_fit = 4)
  )
  expected_output_roc <- copy(expected_output)
  expected_output_roc$outcome <- c(10, 20, 30, 40, 50, 67.86, 92.10, 125,
                                   9, 22, 27, 33, 51, 67.50, 89.33, 118.23)
  testthat::expect_equivalent(output_roc, expected_output_roc, tolerance = 0.1)

  output_unif <- testthat::expect_silent(
    extrapolate(dt, id_cols = c("group", "age"), extrapolate_col = "age",
                value_col = "outcome", extrapolate_vals = c(1:8),
                method = "uniform", n_groups_fit = 4)
  )

  expected_output_unif <- copy(expected_output)
  expected_output_unif[age > 5 & group == "a", outcome := 35]
  expected_output_unif[age > 5 & group == "b", outcome := 36]
  testthat::expect_equal(output_unif, expected_output_unif)

})

testthat::test_that("`extrapolate` works with id_cols that aren't present in every combination", {

  dt <- data.table::data.table(
    age_start = c(1:5),
    year = 1,
    outcome = seq(10, 50, 10)
  )
  dt[, age_end := age_start + 1]
  for (i in 2:3) {
    temp <- copy(dt[year == 1])
    temp[, year := i]
    temp[, outcome := outcome * year]
    dt <- rbind(dt, temp)
  }
  setkeyv(dt, c("age_start", "age_end", "year"))
  dt <- dt[, .SD, .SDcols = c("year", "age_start", "age_end", "outcome")]

  dt <- testthat::expect_silent(extrapolate(
    dt,
    id_cols = c("age_start", "age_end", "year"),
    extrapolate_col = c("year"),
    method = "linear",
    value_col = "outcome",
    extrapolate_vals = c(1:5),
    n_groups_fit = 2
  ))

})
