library(data.table)
library(testthat)

# Basic test with one value column ----------------------------------------

# set up test input data.table
input_dt <- CJ(location = "USA", year = 1950:2010, draw = 1:101)
input_dt[, deaths := draw - 1]
input_dt[, population := deaths * 2]

# set up expected output table
expected_dt <- CJ(
  location = "USA",
  year = 1950:2010,
  mean = 50, median = 50, sd = sd(0:100),
  min = 0, max = 100,
  q2.5 = 2.5, q10 = 10, q90 = 90, q97.5 = 97.5
)
setkeyv(expected_dt, c("location", "year"))

test_that("calculating summary statistics works with one value column", {
  output_dt <- summarize_dt(
    dt = input_dt,
    id_cols = c("location", "year", "draw"),
    summarize_cols = c("draw"),
    value_cols = "deaths",
    summary_fun = c("mean", "median", "sd", "min", "max"),
    probs = c(0.025, 0.1, 0.9, 0.975)
  )
  expect_identical(output_dt, expected_dt)
})

# Test with multiple value columns ----------------------------------------

stat_cols <- setdiff(names(expected_dt), c("location", "year"))

expected_dt_deaths <- copy(expected_dt)
setnames(expected_dt_deaths, stat_cols, paste0("deaths_", stat_cols))

expected_dt_population <- copy(expected_dt)
expected_dt_population[, (stat_cols) := lapply(stat_cols, function(col) get(col) * 2)]
expected_dt_population[, sd := sd(seq(0, 200, by = 2))]
setnames(expected_dt_population, stat_cols, paste0("population_", stat_cols))

expected_dt <- merge(expected_dt_deaths, expected_dt_population)

test_that("calculating summary statistics works with two value columns", {
  output_dt <- summarize_dt(
    dt = input_dt,
    id_cols = c("location", "year", "draw"),
    summarize_cols = c("draw"),
    value_cols = c("deaths", "population"),
    summary_fun = c("mean", "median", "sd", "min", "max"),
    probs = c(0.025, 0.1, 0.9, 0.975)
  )
  expect_identical(output_dt, expected_dt)
})
