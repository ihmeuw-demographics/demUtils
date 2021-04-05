library(testthat)

url <- "http://data.un.org/Data.aspx"
session <- polite::bow(url)

table_url <- "http://data.un.org/Data.aspx?d=PopDiv&f=variableID%3a12%3bcrID%3a4%3bvarID%3a2&c=2,4,6,7&s=_crEngNameOrderBy:asc,_timeEngNameOrderBy:desc,_varEngNameOrderBy:asc&v=1"
table_url <- utils::URLdecode(table_url)

expected_query <- list(
  d = "PopDiv",
  f = "variableID:12;crID:4;varID:2",
  c = "2,4,6,7",
  s = "_crEngNameOrderBy:asc,_timeEngNameOrderBy:desc,_varEngNameOrderBy:asc",
  v = "1"
)

expected_locations <- "Afghanistan"
expected_years <- 1950:2100
expected_variants <- "Medium"
expected_table <- data.table::CJ(
  "Country or Area" = expected_locations,
  "Year(s)" = expected_years,
  "Variant" = expected_variants,
  "Value" = 1
)
data.table::setorder(expected_table, `Country or Area`, -`Year(s)`, `Variant`)

expected_npages <- ceiling(nrow(expected_table) / 50)


test_that("table url is correctly converted to a query argument", {
  query <- undata_url_to_query(table_url)
  expect_identical(query, expected_query)
})

test_that("undata table can be scraped", {
  query <- undata_url_to_query(table_url)
  npages <- get_undata_npages(session, query)
  expect_identical(npages, expected_npages)

  update_dates <- get_undata_update_dates(session, query)
  expect_named(update_dates, c("Last", "Next"))

  table <- get_undata_table(session, query, pages = 1:npages)
  table <- dplyr::mutate(.data = table, Value = 1)
  expect_identical(table, expected_table)
})
