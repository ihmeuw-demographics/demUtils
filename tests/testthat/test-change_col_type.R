library(data.table)
library(testthat)

dt <- data.table::CJ(sex = c("male", "female"), year = 1950L:2000L, age_start = 0L)

test_that("`change_col_type` works", {
  original_dt <- copy(dt)

  # with column not in `dt`, `dt` not modified at all
  change_col_type(dt, check_cols = c("year_start"))
  testthat::expect_identical(dt, original_dt)

  # with column not of the specified type, `dt` not modified at all
  change_col_type(dt, check_cols = c("sex"), is_fun = is.integer)
  testthat::expect_identical(dt, original_dt)

  # changes relevant columns okay
  change_col_type(dt, check_cols = c("year", "age_start"))
  testthat::expect_identical(
    c(class(dt[["year"]]), class(dt[["age_start"]])),
    c("numeric", "numeric")
  )
})
