library(data.table)
library(testthat)


# Test basic scenario -----------------------------------------------------

# set up test input data.table
input_dt <- CJ(
  location = "USA", year = c(2000, 2010), age_start = seq(0, 80, 5),
  report = c(2015, 2020), # numeric variable to prioritize with
  method = c("A", "B"), # categorical variable to prioritize with
  value = 1
)
id_cols <- setdiff(names(input_dt), "value")
setkeyv(input_dt, id_cols)

rank_order <- list(
  method = c("B", "A"),
  report = -1
)

expected_dt <- copy(input_dt)
expected_dt[method == "B" & report == 2020, priority := 1L]
expected_dt[method == "B" & report == 2015, priority := 2L]
expected_dt[method == "A" & report == 2020, priority := 3L]
expected_dt[method == "A" & report == 2015, priority := 4L]
setkeyv(expected_dt, c(id_cols, "priority"))

testthat::test_that("prioritization of data works", {
  output_dt <- prioritize_dt(
    dt = input_dt,
    rank_by_cols = c("location", "year"),
    unique_id_cols = c("location", "year", "age_start"),
    rank_order = rank_order,
  )
  testthat::expect_identical(output_dt, expected_dt)
})

testthat::test_that("'prioritize_dt' catches missing rank order specification", {
  testthat::expect_error(
    prioritize_dt(
      dt = input_dt,
      rank_by_cols = c("location", "year"),
      unique_id_cols = c("location", "year", "age_start"),
      rank_order = rank_order["method"]
    ),
    regexp = "do not uniquely identify each row of"
  )
})

# Rank order missing some categorical factor levels -----------------------

# same input as above but with method 'C' & 'D' added to input

# set up test input data.table
input_dt <- CJ(
  location = "USA", year = c(2000, 2010), age_start = seq(0, 80, 5),
  report = c(2015, 2020), # numeric variable to prioritize with
  method = c("A", "B", "C", "D"), # categorical variable to prioritize with
  value = 1
)
id_cols <- setdiff(names(input_dt), "value")
setkeyv(input_dt, id_cols)

expected_dt <- copy(input_dt)
expected_dt[method == "B" & report == 2020, priority := 1L]
expected_dt[method == "B" & report == 2015, priority := 2L]
expected_dt[method == "A" & report == 2020, priority := 3L]
expected_dt[method == "A" & report == 2015, priority := 4L]
expected_dt[method %in% c("C", "D"), priority := NA]
setkeyv(expected_dt, c(id_cols, "priority"))

testthat::test_that("prioritization of data works when missing levels", {

  # expect error since multiple method/reports will have report NA
  testthat::expect_error(
    suppressWarnings(prioritize_dt(
      dt = input_dt,
      rank_by_cols = c("location", "year"),
      unique_id_cols = c("location", "year", "age_start"),
      rank_order = rank_order,
      warn_missing_levels = TRUE,
    )),
    regexp = "do not uniquely identify each row of"
  )

  # now no error since only checking that top priority is unique
  output_dt <- testthat::expect_warning(
    prioritize_dt(
      dt = input_dt,
      rank_by_cols = c("location", "year"),
      unique_id_cols = c("location", "year", "age_start"),
      rank_order = rank_order,
      warn_missing_levels = TRUE,
      check_top_priority_unique_only = TRUE
    ),
    regexp = "'method' `rank_order` is missing levels"
  )
  testthat::expect_identical(output_dt, expected_dt)
})
