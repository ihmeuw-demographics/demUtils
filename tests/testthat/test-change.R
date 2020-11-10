
library(data.table)

# pct_change --------------------------------------------------------------

x <- c(10, 11, 12)
y <- c(100, 22, 18)

dt <- data.table(x = x)

testthat::test_that("pct_change works", {

  # vector
  testthat::expect_equal(pct_change(x, y), c(900, 100, 50))

  # data.table
  output <- dt[, change := pct_change(shift(x), x)]
  testthat::expect_equivalent(
    output$change, c(NA, 10, 9.091), tolerance = 0.002
  )

})

# arc ---------------------------------------------------------------------

# example from Preston Demography book page 17
years <- c(0, 5, 10)
values <- c(100000, 116183, 134986)

dt <- data.table(years = years, values = values)

testthat::test_that("arc works", {

  # vector
  testthat::expect_equivalent(
    arc(years, values, denominator = 1), c(NA, 0.03, 0.03), tolerance = 0.01
  )

  # data.table
  output <- dt[, arc := arc(years = years, values = values, denominator = 1)]
  testthat::expect_equivalent(
    output$arc, c(NA, 0.03, 0.03), tolerance = 0.01
  )

  # data.table with ID cols
  dt1 <- copy(dt)
  dt2 <- copy(dt)
  dt1$sex <- "male"
  dt2$sex <- "female"
  dt <- rbind(dt1, dt2)

  output <- dt[, arc := arc(years = years, values = values, denominator = 1), by = "sex"]
  testthat::expect_equivalent(
    output$arc, rep(c(NA, 0.03, 0.03), 2), tolerance = 0.01
  )

})
