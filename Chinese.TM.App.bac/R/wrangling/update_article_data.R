library(tidyverse)
library(lubridate)
library(glue)

# Updating data for new articles after the last download.

update_article_data <- function(year, page_num,
                                write_to_disk = FALSE) {
  stopifnot(page_num %in% c("01", "02"))
  message("Updating raw articles from People's Daily website...")

  path <- paste0(here::here(), "/data/article_data_", year , "_page_", page_num, ".rds")
  nms <- paste0("article_data_", year, "_page_", page_num)
  names(path) <- nms
  if(year == "2019"){ # update until when?
    today <- as.Date("2019-12-31")
  } else {today <- today()}

  df <- map(path, read_rds) # read in the last scraped data sets.

  last_updated <- map(df, ~ max(.x[["date"]]))

  for (j in seq_along(last_updated)) {
    message(
      glue("Page {page_num[[j]]}, last updated on {last_updated[[j]]}.\n")
    )
  }

  dat <- set_names(
    vector("list", length(page_num)),
    nms
  ) # for the updated article

  for (i in seq_along(last_updated)) {
    if (last_updated[[i]] == today) {
      message(
        "It seems that the data set <<", nms[[i]],
        ">> is up-to-date."
      )
      dat[[i]] <- tibble() # will not be updated, since it's up-to-date.
    } else if (last_updated[[i]] < today) {
      dates_to_be_updated <- set_names(
        vector("list", length(page_num)),
        nms
      ) # for dates from `last updated` `to today`

      dates_to_be_updated[[i]] <-
        seq.Date(last_updated[[i]] + 1, today, by = 1)

      message(paste("Beginning scraping..."))
      source(paste0(here::here(),"/R/wrangling/scrape_article.R")) # for scrape_article(page_num, dates, ...)

      dat[[i]] <- scrape_article(
        page_num[[i]],
        dates_to_be_updated[[i]]
      )

      df[[nms[[i]]]] <- bind_rows(
        df[[nms[[i]]]],
        dat[[nms[[i]]]]
      )
      if (all(write_to_disk && !is_empty(dat[[i]]))) {
        saveRDS(df[[i]], paste0(here::here(), "/data/", names(df)[[i]], ".rds"))
      }
    } else {
      return("Data contains articles from the future!", call. = FALSE)
    }
  }

  # --- notification of changes;
  # If a data set (here df) is not updated, the nrows stays the same.
  if (write_to_disk) {
    pwalk(list(df, dat, names(df)), function(.x, .y, .z) {
    message("successfully updated!!")
    message(paste(
      "nrows of ", "<<", .z, ">>: ",
      "<<", (nrow(.x) - nrow(.y)), ">> ",
      "--> ",
      "<<", nrow(.x), ">>", ".", "\n"
    ))
    })
  }
  # ---
  dat
}

