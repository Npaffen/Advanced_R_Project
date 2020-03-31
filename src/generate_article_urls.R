# load packages -------------------------------------------------

library(lubridate)
library(rvest)
library(tidyverse)
library(glue)


# ---------------------------------------------------------------------
# This function produces a url for the 1st "edition/version"
# article published on a specific date and extracts urls of columns on it.

# As there're many contents in an article, we'd to extract links to all
# of the contents (columns or sections on the article). We can do that
# once we get the article's url and the date on which it was published.
# ----------------------------------------------------------------------

# Article versions/editions
# version = "865" # meaning 1st or 01 edition for 2018 and before.
# version = "01" # meaning 1st or 01 edition for 2019 and after.

generate_urls <- function(date, version) {
  if (!is.Date(date) && is.na(as_date(date, origin))) {
    stop("error: not a Date. Give a date, in this 'yyyy-mm-dd' form.")
  }

  if (version == "865" && year(date) > 2018) {
    stop("version = '865' is for 2018 and before. ")
  }
  if (version == "01" && year(date) < 2019) {
    stop("version = '01' is for 2019 and after. ")
  }

  date <- lubridate::ymd(date)
  yyyy <- lubridate::year(date)
  mm <- lubridate::month(date)
  mm <- if_else(mm < 10, paste0(0, mm), as.character(mm))
  dd <- lubridate::day(date)
  dd <- if_else(dd < 10, paste0(0, dd), as.character(dd))

  prefix <- "http://paper.people.com.cn/rmrb"

  if (version == "865") {
    middle <- glue("{prefix}hwb/html/{yyyy}-{mm}/{dd}")

    # article link
    article_url <- glue("{middle}/node_{version}.htm")

    # scrape columns or sections in the article.
    # suffix for columns url
    cols <- read_html(article_url) %>%
      html_nodes('a[href*="content_"]') %>%
      html_attr("href") %>%
      unique()

    column_url <- glue::glue("{middle}/{cols}")
  } else if (version == "01") {
    middle <- glue("{prefix}/html/{yyyy}-{mm}/{dd}")

    # article link
    article_url <- glue("{middle}/nbs.D110000renmrb_{version}.htm")

    max_cols <- 9 # check the note below as to why we chose 9.

    yyyymmdd <- paste0(yyyy, mm, dd)

    # suffix for columns url
    cols <- glue("nw.D110000renmrb_{yyyymmdd}_{1:max_cols}-{version}.htm")

    column_url <- glue("{middle}/{cols}")
  } else {
    stop("wrong input: version should be either of c('865', '01').")
  }

  tibble(
    article_url = article_url,
    column_url = column_url,
    date = date,
    cols = cols
  )
}






#### Note: why we take 9 cols as default number of cols? ####

# Using the following code (which is bound to cols*), we found out that
# the maximum number of columns in a single article is 9.
# We do not have to send requests to scrape just the number of columns
# on/in an article. As a result, we make the number of columns or sections in any
# article default to 9. Anyway, the use of safely(), during the request
# (see `get_article_data()` below), captures the error if the actual
# column counts are below or above 9---Will be flagged as **Not Found (HTTP 404)**.

# checker -------------------------------------------------------
# cols <- read_html(article_url) %>%
#           html_nodes('a[href*="nw.D110000renmrb"]') %>%
#           html_attr("href") %>%
#           unique() # *
# checker -------------------------------------------------------
