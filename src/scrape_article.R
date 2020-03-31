# load packages -------------------------------------------------

library(lubridate)
library(rvest)
library(tidyverse)
library(glue)

# ---------------------------------------------------------------

scrape_article <- function(version, year, ...) {
  source("src/make_dates.R") # sources make_dates()
  source("src/generate_article_urls.R") # sources generate_urls()

  dates <- make_dates(year = year, ...)
  dates <- as.Date(unlist(unname(dates)), origin)

  # Article urls for the given year -----------------------------

  # ================================= -----------------------------

  # status notifier
  len <- length(dates)


  # ================================= -----------------------------

  article_urls <- map(
    .x = dates,
    .f = function(x) {
      len <<- len - 1
      if (len > 0 || len %% 50 == 0 || len < 10) {
        message(
          message("GETTING URLS", "--------"),
          "Grab some coffee ", "<", len, "> ",
          "iter", if (len > 1) "s", " left....."
        )
      }
      #------------------------------------------
      safely(slowly(generate_urls), quiet = FALSE)(date = x,
        version = version) # will be returned
    }
  )
  article_urls <- transpose(article_urls)
  is_ok_urls <- article_urls$error %>% map_lgl(is_null)
  article_urls_ok <- suppressWarnings(bind_rows(article_urls$result[is_ok_urls],
    .id = "id"
  ))
  article_urls_notok <- article_urls$error[!is_ok_urls]

  # Download content ----------------------------------------------

  source("src/get_article_contents.R")
  article_data <- get_article_data(article_urls_ok)

  # successful ones
  article_data_ok <- article_data$dat_ok

  # final output of the function
  list(
    urls = article_urls,
    data = article_data_ok
  )
}
