
#' @title Scrape data.un.org tables
#'
#' @description Scrape multiple pages from any data.un.org table.
#'
#' @inheritParams polite::scrape
#' @param pages \[`numeric()`\]\cr
#'   Pages to scrape from the table. See `get_undata_npages` to scrape total
#'   number of tables. Default is 1.
#'
#' @return \[`tibble()`\] all pages from the table combined together.
#'
#' @details
#' A list of all possible tables is shown here http://data.un.org/Explorer.aspx.
#'
#' Use the `undata_url_to_query` helper function to convert a valid table url to
#' the `query` argument in order to specify the specific table, filters, columns
#' etc. that one wants to get.
#'
#' The [polite R package](https://dmi3kno.github.io/polite/index.html) is used
#' to scrape pages from data.un.org so that the website is not hammered with too
#' many requests. It is recommended by the popular web scraping R package
#' [rvest](https://rvest.tidyverse.org/index.html).
#'
#' @family get_undata
#'
#' @examples
#' url <- "http://data.un.org/Data.aspx"
#' session <- polite::bow(url)
#'
#' # determine the url query arguments
#' table_url <- "http://data.un.org/Data.aspx?d=POP&f=tableCode%3a260"
#' query <- undata_url_to_query(table_url)
#'
#' npages <- get_undata_npages(session, query)
#' update_dates <- get_undata_update_dates(session, query)
#' data <- get_undata_table(session, query, pages = 1:5)
#'
#' @importFrom magrittr %>%
#' @export
get_undata_table <- function(bow, query, pages = 1) {

  checkmate::assert_numeric(pages)
  checkmate::assert_list(query)

  table <- purrr::map_dfr(
    .x = pages,
    .f = ~get_undata_table_page(bow, query, page = .x)
  )
  checkmate::assert_tibble(table)

  return(table)
}

#' @title Helper function to scrape one page from a table on data.un.org
#' @inheritParams get_undata_table
#' @param page \[`numeric(1)`\]\cr
#'   The page to scrape from the specified table.
#' @importFrom magrittr %>%
get_undata_table_page <- function(bow, query, page) {

  checkmate::assert_number(page)
  checkmate::assert_list(query)

  query[["v"]] <- NULL
  query[["v"]] <- page

  table <- polite::scrape(bow, query) %>%
    rvest::html_element(css = ".DataContainer") %>%
    rvest::html_table()

  # TODO: create page number column

  # TODO: merge on footnotes

  checkmate::assert_tibble(table)
  return(table)
}

#' @title Get the total number of pages available in the specified table.
#'
#' @inheritParams get_undata_table
#'
#' @return \[`numeric(1)`\] the total number of pages available in a table as
#' specified by `query`.
#'
#' @family get_undata
#'
#' @importFrom magrittr %>%
#' @export
get_undata_npages <- function(bow, query) {
  npages <- polite::scrape(bow, query) %>%
    rvest::html_element("#spanPageCountB") %>%
    rvest::html_text() %>%
    as.numeric()
  checkmate::assert_numeric(npages, len = 1)
  return(npages)
}

#' @title Get the listed update dates for a data.un.org table
#'
#' @inheritParams get_undata_table
#'
#' @return a named \[`Date()`\] vector, the names correspond to the description
#' of the date (likely 'Last' or 'Next').
#'
#' @family get_undata
#'
#' @importFrom magrittr %>%
#' @export
get_undata_update_dates <- function(bow, query) {

  # get update element and separate lines
  update_date_text <- polite::scrape(bow, query) %>%
    rvest::html_element(".Update") %>%
    rvest::html_text2() %>%
    strsplit(split = "\n") %>%
    unlist()

  # get description of dates
  update_dates_names <- update_date_text %>%
    gsub(pattern = " update in UNdata: \\d*/\\d*/\\d*$", replacement = "")
  checkmate::assert_character(update_dates_names, any.missing = FALSE)

  # isolate actual dates
  update_dates <- update_date_text %>%
    gsub(
      pattern = paste0("^(", paste(update_dates_names, collapse = "|"), ") update in UNdata: "),
      replacement = ""
    ) %>%
    as.Date(format = "%Y/%m/%d")
  checkmate::assert_date(update_dates, any.missing = FALSE)

  names(update_dates) <- update_dates_names
  return(update_dates)
}

#' @title Convert the undata url to the query argument needed to reference
#'   specific tables
#'
#' @param table_url \[`character(1)`\]\cr
#'   The full url to a specific table. If filters, columns, sort order or other
#'   options are selected, use the 'Link to this page' -> 'Update' button to update
#'   the `table_url`.
#' @param base_url \[`character(1)`\]\cr
#'   The base url of the data.un.org site. Default is 'http://data.un.org/Data.aspx'.
#'
#' @return \[`list()`\] of query parameters to pass to `get_undata_table` or
#'   other related undata functions.
#'
#' @family get_undata
#'
#' @examples
#' table_url <- "http://data.un.org/Data.aspx?d=PopDiv&f=variableID%3a12"
#' query <- undata_url_to_query(table_url)
#'
#' @export
undata_url_to_query <- function(table_url, base_url = "http://data.un.org/Data.aspx") {

  checkmate::assertString(base_url, pattern = paste0("http://"))
  checkmate::assertString(table_url, pattern = paste0("^", base_url, "?"))

  # get the url query arguments
  table_url <- utils::URLdecode(table_url)
  url_query <- gsub(paste0("^", base_url, "\\?"), "", table_url)

  # split into individual variable-value pairs
  pairs <- unlist(strsplit(url_query, "&"))
  # split into list of c(variable, value) vectors
  pairs <- strsplit(pairs, "=")

  query_variable <- sapply(pairs, `[[`, 1)
  query_value <- lapply(pairs, `[[`, 2)
  names(query_value) <- query_variable

  return(query_value)
}
