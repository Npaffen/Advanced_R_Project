# load packages -------------------------------------------------
library(lubridate)
library(rvest)
library(tidyverse)
library(glue)
# ---------------------------------------------------------------
# if dates is not supplied, then ... passes args to the make_dates()
# function, which [ year, month, from_day, to_day, all_dates (logical),
# respectively ]. Please refer to `make_dates.R`.
scrape_article <- function(page_num, dates, ...) {
  source("src/make_dates.R") # sources make_dates()
  source("src/generate_article_urls.R") # sources generate_urls()

  if (missing(dates)) {
    dates <- make_dates(...)
    dates <- as.Date(unlist(unname(dates)), origin)
  }

  # Article urls for the given year -----------------------------
  # ================================= -----------------------------
  article_urls <- suppressWarnings(map_df(dates, ~ generate_urls(.x, page_num)))
  # Download content ----------------------------------------------
  source("src/get_article_contents.R")

  article_data <- get_article_data(article_urls)

  # returns only the successful ones
  # final output of the function
  article_data
}
