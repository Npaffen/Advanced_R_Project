## This function creates a url for a "edition/version 01" article published
# on a specific date and for columns on it.

generate_article_url <- function(date, edition = "01") {
  date <- lubridate::ymd(date)
  yyyy <- lubridate::year(date)
  mm <- lubridate::month(date)
  mm <- if_else(mm < 10, paste0(0, mm), as.character(mm))
  dd <- lubridate::day(date)
  dd <- if_else(dd < 10, paste0(0, dd), as.character(dd))
  
  initial <- glue::glue("http://paper.people.com.cn/rmrb/html/{yyyy}-{mm}/{dd}/")

  # article link
  article_url <- glue::glue("{initial}nbs.D110000renmrb_{edition}.htm")


  # counts the number of columns or sections in the article.

  cols <- read_html(article_url) %>%
    html_nodes('a[href*="nw.D110000renmrb"]') %>%
    html_attr("href") %>%
    unique()

  # cols_num <- seq(1, length(cols))


  yyyymmdd <- paste0(yyyy, mm, dd)


  cols_url <- glue::glue("{initial}{cols}")

  list(
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


date_vec <- seq(as.Date("2020-01-01"), lubridate::today(), by = 1)

article_urls <- map(date_vec, generate_article_url)

article_urls <- setNames(article_urls, date_vec)


article_urls <- map_df(article_urls, as_tibble)

# saveRDS(article_urls, 'data/article_urls.RDS')



# Scraping begins -----------------------------------------------


# make a pause in each iteration.
safely_slowly_extract <- safely(
  slowly(
    extract_content,
    quiet = FALSE
  ),
  quiet = FALSE
)


## here it goes 

count <- 0

article_data <- map(article_urls$cols_url, function(x) {
  count <<- count + 1
  
  print(sprintf("Hang on there, %d iteration left.:(", 
                length(article_urls$cols_url) - count))
  
  safely_slowly_extract(x)
})


names(article_data) <- article_urls$cols

article_transposed <- transpose(article_data)


is_ok <-  article_transposed$error %>% map_lgl(is_null)


article_data_ok <- bind_rows(article_transposed$result[is_ok], .id = 'id')


# saveRDS(article_data_ok, 'data/article_data_ok.rds')
# saveRDS(article_transposed, 'data/article_transposed.rds')

# article_transposed$error[!is_ok]

# $`nw.D110000renmrb_20200118_4-01.htm`
# <simpleError in open.connection(x, "rb"): Recv failure: Connection reset by peer>
