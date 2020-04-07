
# ---------------------------------------------------------------------
# This function produces a url for the 1st and 2nd pages
# of articles published on a specific date and extracts urls of columns on it.

# As there're many contents on a page of an article, we need to extract links to all
# of the contents (columns or sections on the article-per page).
# We can do that once we get the article's url and the date on
# which it was published--and then construct the urls of the sections on a page.

# ----------------------------------------------------------------------
# Steps: Article->pages(01&02)->sections/columns->paragraphs
# pages = "01"  or "02" # meaning 1st or 01,
# and 2nd or 02 pages of articles from 2019 and after.

generate_urls <- function(date, page_num) {
  if (!is.Date(date) && is.na(as_date(date, origin))) {
    stop("error: not a Date. Give a date, in this 'yyyy-mm-dd' form.")
  }
  pages <- c("01", "02")
  notin <- negate(`%in%`)
  if (notin(page_num, pages)) {
    stop("page_num should be either c(\"01\", \"02\").", call. = FALSE)
  }

  pp <- pages[match(page_num, pages)]

  date <- lubridate::ymd(date)
  yyyy <- lubridate::year(date)
  mm <- lubridate::month(date)
  mm <- if_else(mm < 10, paste0(0, mm), as.character(mm))
  dd <- lubridate::day(date)
  dd <- if_else(dd < 10, paste0(0, dd), as.character(dd))

  prefix <- "http://paper.people.com.cn/rmrb"
  middle <- glue("{prefix}/html/{yyyy}-{mm}/{dd}")

  # article link
  article_url <- glue("{middle}/nbs.D110000renmrb_{pp}.htm")
  max_cols <- 15 # check the note below as to why we chose 15.
  yyyymmdd <- paste0(yyyy, mm, dd)

  # suffix for columns url
  cols <- glue("nw.D110000renmrb_{yyyymmdd}_{1:max_cols}-{pp}.htm")
  column_url <- glue("{middle}/{cols}")

  tibble(
    article_url = article_url,
    column_url = column_url,
    date = date,
    cols = cols
  )
}


#### Note: why do we take 15 cols as default number of cols? ####

# Using the following code (which is bound to cols*), we found out that
# the maximum number of columns on a page of an article is 15.
# We do not have to send requests to scrape just the number of columns
# on/in an article. For that matter, we make the number of columns or sections in any
# article default to 15. Anyway, the use of safely(), during the request
# (see `get_article_contents()` in src/), captures the error if the actual
# column counts are below or above 15---Will be flagged as **Not Found (HTTP 404)**.

# checker -------------------------------------------------------
# cols <- read_html(article_url) %>%
#           html_nodes('a[href*="nw.D110000renmrb"]') %>%
#           html_attr("href") %>%
#           unique() # *
# checker -------------------------------------------------------
