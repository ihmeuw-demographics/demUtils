
# setup expected output for `interpolate` and linear `extrapolate`
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

testthat::test_that("`extrapolate` works", {

  dt <- data.table::data.table(
    group = c(rep("a", 5), rep("b", 5)),
    x = rep(c(1:5), 2),
    y = rep(seq(10, 50, 10), 2)
  )

  output <- testthat::expect_silent(
    extrapolate(dt, id_cols = c("group", "x"), extrapolate_col = "x",
                value_col = "y", extrapolate_vals = c(1:10),
                method = "linear", n_groups_fit = 3)
  )
  testthat::expect_equal(output, expected_output)

  output_roc <- testthat::expect_silent(
    extrapolate(dt, id_cols = c("group", "x"), extrapolate_col = "x",
                value_col = "y", extrapolate_vals = c(1:10),
                method = "rate_of_change", n_groups_fit = 3)
  )
  expected_output_roc <- copy(expected_output)
  expected_output_roc$y <- rep(c(10, 20, 30, 40, 50, 64.5, 83.3, 107.6, 138.9, 179.3), 2)
  testthat::expect_equivalent(output_roc, expected_output_roc, tolerance = 0.2)

  output_unif <- testthat::expect_silent(
    extrapolate(dt, id_cols = c("group", "x"), extrapolate_col = "x",
                value_col = "y", extrapolate_vals = c(1:10),
                method = "uniform", n_groups_fit = 3)
  )

  expected_output_unif <- copy(expected_output)
  expected_output_unif[x > 5, y := 40]
  testthat::expect_equal(output_unif, expected_output_unif)

})
