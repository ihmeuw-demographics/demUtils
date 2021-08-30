
#' @title Download data.un.org tables as csvs
#'
#' @param table_url \[`character(1)`\]\cr
#'   The full url to a specific table. If filters, columns, sort order or other
#'   options are selected, use the 'Link to this page' -> 'Update' button to update
#'   the `table_url`.
#' @param output_dir \[`character(1)`\]\cr
#'   Directory to save separate downloaded data files.
#' @param record_limit \[`numeric(1)`\]\cr
#'   Upper limit of records/rows that you can download at once.
#' @param download_sleep \[`numeric(1)`\]\cr
#'   Seconds to sleep/wait between download calls.
#' @param base_url \[`character(1)`\]\cr
#'   The base url of the data.un.org site. Default is 'http://data.un.org/Data.aspx'.
#' @param downloader_url
#'   The base url of the data.un.org site download interface. Default is
#'   'http://data.un.org/Handlers/DownloadHandler.ashx'.
#'
#' @details
#' A list of all possible tables is shown here http://data.un.org/Explorer.aspx.
#' Currently this function assumes one is able to filter by Year when
#' downloading data.
#'
#' The 'data.un.org' website does not allow one to download more than 100,000
#' records at a time. In order to account for this, we loop through groups of
#' years that stay below the limit. This means `table_url` should not already
#' include specified filters for years as this function is intended to download
#' all years of data.
#'
#' @return \[`character()`\] file paths for each separate file downloaded.
#'
#' @family get_undata
#'
#' @examples
#' \dontrun{

#' table <- download_undata_table(
#'   table_url = "http://data.un.org/Data.aspx?d=POP&f=tableCode:7;areaCode:0&v=1",
#'   output_dir = tempdir()
#' )
#' }
#'
#' @export
download_undata_table <- function(table_url,
                                  output_dir,
                                  record_limit = 100000,
                                  download_sleep = 30,
                                  base_url = "http://data.un.org/Data.aspx",
                                  downloader_url = "http://data.un.org/Handlers/DownloadHandler.ashx") {

  checkmate::assert_directory(output_dir)
  checkmate::assert_numeric(record_limit, len = 1)
  checkmate::assert_numeric(download_sleep, len = 1)
  checkmate::assert_character(downloader_url)

  session <- polite::bow(base_url)
  query <- undata_url_to_query(table_url, base_url)

  if (grepl("refYear", query$f)) {
    stop("Do not use 'table_url' with Year filter included")
  }

  # Determine groups of years to download at once ---------------------------

  message("Determining groups of years to download while meeting the record limit")

  # determine available years to filter on
  years <- polite::scrape(session, query) %>%
    rvest::html_element("#divYearInner") %>%
    rvest::html_text(trim=TRUE)
  if (is.na(years)) {
    years <- polite::scrape(session, query) %>%
      rvest::html_element("#divYearsInner") %>%
      rvest::html_text(trim=TRUE)
  }
  checkmate::assert_character(years, len = 1)
  years <- sapply(seq(from=1, to=nchar(years), by=4), function(i) substr(years, i, i+3))
  years <- sort(as.numeric(years))
  checkmate::assert_numeric(years)

  year_groups <- list(c())
  for (year in years) {
    group <- length(year_groups)
    check_years <- c(year_groups[[group]], year)

    check_query <- copy(query)
    check_query$f <- paste0(check_query$f, ";refYear:", paste(check_years, collapse = ","))
    nrecords <- get_undata_nrecords(session, check_query)

    if (nrecords <= record_limit) {
      # add to group
      year_groups[[group]] <- check_years
    } else {
      # start a new group
      year_groups[[group + 1]] <- year
    }
  }

  # Download separate files -------------------------------------------------

  message("Downloading separate files")
  output_files <- c()
  for (year_group in year_groups) {

    get_query <- copy(query)
    get_query$f <- paste0(get_query$f, ";refYear:", paste(year_group, collapse = ","))

    fname <- paste0(min(year_group), "-", max(year_group), ".csv")
    message("- ", fname)
    fpath <- fs::path(output_dir, fname)
    output_files <- append(output_files, fpath)

    single_download_undata_table(
      query = get_query,
      output_path = fpath,
      bow = session,
      downloader_url = downloader_url
    )
    Sys.sleep(download_sleep)
  }

  return(output_files)
}

#' @title Helper function to download all records for a given data.un.org query
#'
#' @param query \[`list(1)`\]\cr
#'   Query with filters, columns, sort order arguments specified as returned by
#'   `undata_url_to_query`.
#' @param output_path \[`character(1)`\]\cr
#'   Full path and file name to save downloaded csv file to.
#' @inheritParams polite::scrape
#' @inheritParams download_undata_table
#'
#' @return \[`character(1)`\] invisibly returns `output_path`where downloaded
#'   file is saved.
single_download_undata_table <- function(query,
                                         output_path,
                                         bow,
                                         record_limit = 100000,
                                         downloader_url = "http://data.un.org/Handlers/DownloadHandler.ashx") {

  checkmate::assert_list(query)
  checkmate::assert_character(output_path, len = 1)
  checkmate::assert_numeric(record_limit, len = 1)
  checkmate::assert_character(downloader_url, len = 1)

  nrecords <- get_undata_nrecords(bow, query)
  assertthat::assert_that(
    nrecords <= record_limit,
    msg = "given 'table query' has too many records to download at once"
  )

  # remove page query
  query[["v"]] <- NULL

  # specify csv download format
  query[["Format"]] <- "csv"

  # replace query shortcuts
  names(query)[names(query) =="f"] <- "DataFilter"
  names(query)[names(query) =="d"] <- "DataMartId"

  # create full url for 'DownloadHandler'
  # Found by:
  #   1. In Chrome open 'View' -> 'Developer' -> 'Developer Tools'.
  #   2. Open Network tab.
  #   3. Click download button.
  #   4. Copy url in new row added to Network table.
  downloader_url <- httr::modify_url(downloader_url, query=query)
  downloader_url <- utils::URLdecode(downloader_url)

  # get a temporary location to download the zip file to
  zipdir <- tempdir()
  zipfile <- tempfile(tmpdir = zipdir)

  # download the zip file
  res <- httr::GET(downloader_url, httr::write_disk(zipfile))
  stopifnot(res$status_code == 200)

  # get the name of the csv file within the zip file
  fname <- utils::unzip(zipfile, list = TRUE)$Name
  stopifnot(length(fname) == 1)

  # unzip the file and move it to 'output_path'
  utils::unzip(zipfile, exdir = zipdir)
  fs::file_move(fs::path(zipdir, fname), output_path)
  fs::file_delete(zipfile)

  return(invisible(output_path))
}

#' @title Combine together downloaded undata files
#'
#' @param fpaths \[`character()`\]\cr
#'   Path to separate undata files to combine together. Output of
#'   `download_undata_table`.
#' @param n_footnote_col \[`numeric(1)`\]\cr
#'   Column number of the 'data' table where the `footnote_col` is also stored.
#'   Default is '2'.
#' @param data_footnoteid_col \[`character(1)`\]\cr
#'   Name of column in the data table storing the id number. Default is
#'   'Value Footnotes'.
#' @param footnoteid_col \[`character(1)`\]\cr
#'   Name of column in the footnote table storing the id number. Default is
#'   'footnoteSeqID'.
#' @param footnote_col \[`character(1)`\]\cr
#'   Name of column in the footnote table storing the text. Default is 'Footnote'.
#'
#' @return \[`data.table(1)`\] with all data from input paths combined together.
#'
#' @details undata tables contain the standard dataset included in the online tables
#'   plus sometimes a table at the bottom containing footnotes. This function
#'
#' @family undata
#'
#' @examples
#' \dontrun{
#' data <- read_undata_files
#' }
#' @export
read_undata_files <- function(fpaths,
                              n_footnote_col = 2,
                              data_footnoteid_col = "Value Footnotes",
                              footnoteid_col = "footnoteSeqID",
                              footnote_col = "Footnote") {

  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package \"readr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  checkmate::assert_file(fpaths)
  checkmate::assert_numeric(n_footnote_col, len = 1)
  checkmate::assert_character(data_footnoteid_col, len = 1)
  checkmate::assert_character(footnoteid_col, len = 1)
  checkmate::assert_character(footnote_col, len = 1)

  all_data <- lapply(fpaths, function(fpath) {

    # determine what line the footnotes table starts on
    data <- readr::read_csv(fpath)
    data <- data.table(data)

    footnote_start <- rownames(data)[data[, get(names(data)[n_footnote_col]) == footnote_col]]
    if (length(footnote_start) == 1) {
      footnote_start <- as.integer(footnote_start)

      # read in just the data table
      # make sure ',' separated footnote values read in correctly as a string
      col_types <- list("c")
      names(col_types) <- c(data_footnoteid_col)
      data <- readr::read_csv(fpath, n_max = footnote_start - 1, col_types = col_types)
      data <- data.table(data)

      # read in the footnotes table
      names(col_types) <- c(footnoteid_col)
      footnotes <- readr::read_csv(fpath, skip = footnote_start, col_types = col_types)
      footnotes <- data.table(footnotes)

      # generate new rows for data that corresponds to multiple footnotes
      all_footnote_ids <- unique(data[[data_footnoteid_col]])
      concatenated_ids <- grep(',', all_footnote_ids, value = TRUE)
      concatenated_footnotes <- lapply(concatenated_ids, function(id) {
        ids <- strsplit(id, ",")[[1]]
        new_row <- data.table::data.table(
          id = id,
          footnote = paste(footnotes[get(footnoteid_col) %in% ids, get(footnote_col)], collapse = ", ")
        )
        data.table::setnames(new_row, names(new_row), names(footnotes))
        return(new_row)
      })
      concatenated_footnotes <- data.table::rbindlist(concatenated_footnotes)
      footnotes <- rbind(footnotes, concatenated_footnotes)

      # merge the footnote text onto the data table
      data.table::setnames(data, data_footnoteid_col, footnoteid_col)
      data <- merge(data, footnotes, by = footnoteid_col, all.x = TRUE)

      assertable::assert_values(
        data = data[!is.na(get(footnoteid_col))],
        colnames = footnote_col,
        test = "not_na"
      )
    }

    return(data)
  })

  all_data <- data.table::rbindlist(all_data)
  return(all_data)
}

#' @title Get the total number of records (rows) available in the specified table.
#'
#' @inheritParams polite::scrape
#'
#' @return \[`numeric(1)`\] the total number of records available in a table as
#' specified by `query`.
#'
#' @family get_undata
#'
#' @importFrom rvest %>%
#' @export
get_undata_nrecords <- function(bow, query) {
  nrecords <- polite::scrape(bow, query) %>%
    rvest::html_element("#spanRecordCountB") %>%
    rvest::html_text() %>%
    as.numeric()
  checkmate::assert_numeric(nrecords, len = 1)
  return(nrecords)
}

#' @title Get the listed update dates for a data.un.org table
#'
#' @inheritParams polite::scrape
#'
#' @return a named \[`Date()`\] vector, the names correspond to the description
#' of the date (likely 'Last' or 'Next').
#'
#' @family get_undata
#'
#' @importFrom rvest %>%
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
#' @inheritParams download_undata_table
#' @return \[`list()`\] of query parameters to pass to `get_undata_table` or
#'   other related undata functions.
#'
#' @family get_undata
#'
#' @examples
#' table_url <- "http://data.un.org/Data.aspx?d=PopDiv&f=variableID%3a12"
#' query <- undata_url_to_query(table_url)
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
