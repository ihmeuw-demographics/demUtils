% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_undata.R
\name{get_undata_table}
\alias{get_undata_table}
\title{Scrape data.un.org tables}
\usage{
get_undata_table(bow, query, pages = 1)
}
\arguments{
\item{bow}{host introduction object of class \code{polite}, \code{session} created by \code{bow()} or \code{nod()}}

\item{query}{named list of parameters to be appended to URL in the format \code{list(param1=valA, param2=valB)}}

\item{pages}{[\code{numeric()}]\cr
Pages to scrape from the table. See \code{get_undata_npages} to scrape total
number of tables. Default is 1.}
}
\value{
[\code{data.table()}] all pages from the table combined together.
}
\description{
Scrape multiple pages from any data.un.org table.
}
\details{
A list of all possible tables is shown here http://data.un.org/Explorer.aspx.

Use the \code{undata_url_to_query} helper function to convert a valid table url to
the \code{query} argument in order to specify the specific table, filters, columns
etc. that one wants to get.

The \href{https://dmi3kno.github.io/polite/index.html}{polite R package} is used
to scrape pages from data.un.org so that the website is not hammered with too
many requests. It is recommended by the popular web scraping R package
\href{https://rvest.tidyverse.org/index.html}{rvest}.
}
\examples{
url <- "http://data.un.org/Data.aspx"
session <- polite::bow(url)

# determine the url query arguments
table_url <- "http://data.un.org/Data.aspx?d=POP&f=tableCode\%3a260"
query <- undata_url_to_query(table_url)

npages <- get_undata_npages(session, query)
update_dates <- get_undata_update_dates(session, query)
data <- get_undata_table(session, query, pages = 1:5)

}
\seealso{
Other get_undata: 
\code{\link{get_undata_npages}()},
\code{\link{get_undata_update_dates}()},
\code{\link{undata_url_to_query}()}
}
\concept{get_undata}
