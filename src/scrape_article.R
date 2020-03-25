
# load packages -------------------------------------------------

library(lubridate)
library(rvest)
library(tidyverse)
library(glue)


## This function creates a url for a "edition/version 01" article published
# on a specific date and for columns on it.

generate_article_url <- function(date, edition = "01") {
  date <- ymd(date)
  yyyy <- year(date)
  mm <- month(date)
  mm <- if_else(mm < 10, paste0(0, mm), as.character(mm))
  dd <- day(date)
  dd <- if_else(dd < 10, paste0(0, dd), as.character(dd))

  initial <- glue("http://paper.people.com.cn/rmrb/html/{yyyy}-{mm}/{dd}")

  # article link
  article_url <- glue("{initial}/nbs.D110000renmrb_{edition}.htm")


  # Using the following code (which is bound to cols*), we found out that
  # the maximum number of columns in a single article is 9.
  # We do not have to scrape just to get the number of column counts
  # for each article, we default the number of columns or sections in any
  # article to 9. Anyway, the use of safely() in the data getter function
  # (see `get_article_data()` below) captures the error if the actual
  # column counts are below or above 9.

  # checker -------------------------------------------------------
  # cols <- read_html(article_url) %>%
  #           html_nodes('a[href*="nw.D110000renmrb"]') %>%
  #           html_attr("href") %>%
  #           unique() # *
  # checker -------------------------------------------------------

  max_cols <- 9

  yyyymmdd <- paste0(yyyy, mm, dd)

  # suffix for columns url
  cols <- glue("nw.D110000renmrb_{yyyymmdd}_{1:max_cols}-{edition}.htm")

  cols_url <- glue("{initial}/{cols}")

  tibble(
    article_url = article_url,
    cols_url = cols_url,
    date = date,
    cols = cols
  )
}



extract_content <- function(cols_url) {
  page <- read_html(cols_url)

  get_h1 <- compose(html_text,
    partial(html_nodes, css = "h1"),
    partial(html_nodes, css = ".text_c"),
    .dir = "backward"
  )

  get_h3 <- compose(html_text,
    partial(html_nodes, css = "h3"),
    partial(html_nodes, css = ".text_c"),
    .dir = "backward"
  )

  get_paragraph <- compose(html_text,
    partial(html_nodes, css = "p"),
    partial(html_nodes, css = ".c_c"),
    .dir = "backward"
  )

  df <- list(
    title = get_h1(page),
    subtitle = get_h3(page),
    content = get_paragraph(page)
  )

  df <- tibble(
    title = df$title,
    subtitle = df$subtitle,
    content = paste0(df$content, collapse = "||"),
    num_paragraph = length(df$content)
  )
  df
}

# Scraping begins -----------------------------------------------


# make a pause in each iteration.
safely_slowly_extract <- safely(
  slowly(
    extract_content,
    quiet = FALSE
  ),
  quiet = FALSE
)


# create a function that extractes the article text data and ----
# other additional infor.


get_article_data <- function(article_urls) {
  count <- 0

  article_data <- map(article_urls$cols_url, function(x) {
    count <<- count + 1

    print(sprintf(
      "Hang on there, %d iterations left. :(",
      length(article_urls$cols_url) - count
    ))

    safely_slowly_extract(x)
  })


  names(article_data) <- article_urls$cols

  article_data <- transpose(article_data)


  is_ok <- article_data$error %>% map_lgl(is_null)


  article_data_ok <- bind_rows(article_data$result[is_ok], .id = "id")
  article_data_notok <- article_data$error[!is_ok]

  rm(article_data)
  rm(is_ok)

  article_data_ok %>%
    mutate(
      date = ymd(str_extract(id, "\\d{8}")),
      column_num = str_extract(id, "(_\\d-)"),
      column_num = str_extract(column_num, "\\d")
    )

  list(
    article_data_ok = article_data_ok,
    article_data_notok = article_data_notok
  )
}

# create vectors of dates.

date_vec_2020 <- seq(ymd("2020-01-01"), today(), by = 1)

date_vec_2019 <- seq(ymd("2019-01-01"), ymd("2020-01-01") - 1, by = 1)

# For 2020 ---------------------------------------------------
article_urls_2020 <- map_df(date_vec_2020, generate_article_url)


# saveRDS(article_urls_2020, 'data/article_urls_2020.RDS')

# For 2019 ---------------------------------------------------

article_urls_2019 <- map_df(date_vec_2019, generate_article_url)

# saveRDS(article_urls_2019, 'data/article_urls_2019.RDS')



article_data_2020 <- get_article_data(article_urls_2020)

# successful ones
article_data_ok_2020 <- article_data_2020$article_data_ok

article_data_2019 <- get_article_data(article_urls_2019)

# successful ones
article_data_ok_2019 <- article_data_2019$article_data_ok

# saveRDS(article_data_ok_2020, 'data/article_data_ok_2020.rds')
# saveRDS(article_data_ok_2019, 'data/article_data_ok_2019.rds')
