## This function creates a url for a "edition/version 01" article published
# on a specific date and for columns on it.

node <- 865 # meaning first (01) edition.

generate_article_url <- function(date, node = "865") {
  date <- lubridate::ymd(date)
  yyyy <- lubridate::year(date)
  mm <- lubridate::month(date)
  mm <- if_else(mm < 10, paste0(0, mm), as.character(mm))
  dd <- lubridate::day(date)
  dd <- if_else(dd < 10, paste0(0, dd), as.character(dd))

  initial <- glue("http://paper.people.com.cn/rmrbhwb/html/{yyyy}-{mm}/{dd}")

  # article link
  article_url <- glue("{initial}/node_{node}.htm")


  # counts the number of columns or sections in the article.

  cols <- read_html(article_url) %>%
    html_nodes('a[href*="content_"]') %>%
    html_attr("href") %>%
    unique()

  cols_url <- glue::glue("{initial}/{cols}")

  tibble(
    article_url = article_url,
    cols_url = cols_url,
    date = date,
    cols = cols
  )
}


# func 2. ======================= start -----------------------------------

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

# func 2. ======================= end -----------------------------------


# func 3. ======================= start ------------------------------

# make a pause in each iteration.
safely_slowly_generate_url <- safely(
  slowly(
    generate_article_url,
    quiet = FALSE
  ),
  quiet = FALSE
)

# func 3. ======================= end ------------------------------



# func 4. ======================= start ------------------------------

# make a pause in each iteration.
safely_slowly_extract <- safely(
  slowly(
    extract_content,
    quiet = FALSE
  ),
  quiet = FALSE
)

# func 4. ======================= end ------------------------------


# func 5. ======================= start ------------------------------

# create a function that extractes the article text data and ----
# other additional infor.


get_article_data <- function(article_urls) {
  count <- 0

  article_data <- map(article_urls$cols_url, function(x) {
    count <<- count + 1

    print(sprintf(
      "Hang on there, %d iters left >>>>> :D",
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


# func 5. ======================= end -------------------------------


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++----
# Scraping begins
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# For 2018 -----------------------------------------------------------

# create vectors of dates.
first_date_2018 <- ymd("2018-01-01")
last_date_2018 <- ymd("2019-01-01") - 1
dates_2018 <- seq.Date(first_date_2018, last_date_2018, by = 1)

# first issue/subarticle of 2018.
# source: http://paper.people.com.cn/rmrbhwb/html/2018-01/01/content_1826928.htm
# last issue/subarticle of 2018.
# source: http://paper.people.com.cn/rmrbhwb/html/2018-12/31/content_1901725.htm

# For 2017 -----------------------------------------------------------

first_date_2017 <- ymd("2017-01-01")
last_date_2017 <- ymd("2018-01-01") - 1
dates_2017 <- seq.Date(first_date_2017, last_date_2017, by = 1)



# Article urls for 2018 

# count <- 0
# article_urls_2018 <- map(dates_2018, function(x) {
#   count <<- count + 1
#   print(sprintf(
#     "Hang on there, %d iters left >>>>> :D",
#     length(dates_2018) - count
#   ))
# safely_slowly_generate_url(x)
# })
# 
# 
# names(article_urls_2018) <- dates_2018
# 
# article_urls_2018 <- transpose(article_urls_2018)
# 
# 
# is_ok_urls <- article_urls_2018$error %>% map_lgl(is_null)
# 
# 
# article_urls_2018_ok <- bind_rows(article_urls_2018$result[is_ok_urls], .id = "id")
# article_urls_2018_notok <- article_urls_2018$error[!is_ok_urls]
# 
# 
# 
# # saveRDS(article_urls_2018, 'data/article_urls_2018.RDS')
# 
# 
# 
# # Download content ----------------------------------------------
# 
# article_data_2018 <- get_article_data(article_urls_2018_ok)
# 
# # successful ones
# article_data_ok_2018 <- article_data_2018$article_data_ok
# 
# # saveRDS(article_data_ok_2018, 'data/article_data_ok_2018.rds')

# Article urls for 2017

count <- 0
article_urls_2017 <- map(dates_2017, function(x) {
  count <<- count + 1
  print(sprintf(
    "Hang on there, %d iters left >>>>> :D",
    length(dates_2017) - count
  ))
  safely_slowly_generate_url(x)
})


names(article_urls_2017) <- dates_2017

article_urls_2017 <- transpose(article_urls_2017)


is_ok_urls_2017 <- article_urls_2017$error %>% map_lgl(is_null)


article_urls_2017_ok <- bind_rows(article_urls_2017$result[is_ok_urls_2017], .id = "id")
article_urls_2017_notok <- article_urls_2017$error[!is_ok_urls_2017]



# saveRDS(article_urls_2017, 'data/article_urls_2017.RDS')



# Download content ----------------------------------------------

article_data_2017 <- get_article_data(article_urls_2017_ok)

# successful ones
article_data_ok_2017 <- article_data_2017$article_data_ok

# saveRDS(article_data_ok_2017, 'data/article_data_ok_2017.rds')