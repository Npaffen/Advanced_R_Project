# func 1. ======================= start -----------------------------------

extract_content <- function(column_url) {
  page <- read_html(column_url)
  
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

# func 1. ======================= end -----------------------------------




# func 2. ======================= start ------------------------------

# create a function that extractes the article text data and ----
# other additional info en mass.


get_article_data <- function(article_urls) {
  len <- length(article_urls$column_url)
  
  dat <- map(article_urls$column_url, function(x) {
    len <<- len - 1
    if (len %% 10 == 0 || len < 10) {
      message(
        message("GETTING TEXT DATA"),
        "Grab some coffee ", "<", len, "> ",
        "iter", if (len > 1) "s", " left....."
      )
    }
    safely(slowly(extract_content)
    )(x)
    # so that there is a short pause between consecutive requests.
  })
  
  
  names(dat) <- article_urls$cols
  
  dat <- transpose(dat)
  
  
  is_ok <- dat$error %>% map_lgl(is_null)
  
  
  dat_ok <- suppressWarnings(bind_rows(dat$result[is_ok], .id = "id"))
  dat_notok <- dat$error[!is_ok]
  
  dat_ok <- dat_ok %>%
    mutate(
      date = ymd(str_extract(id, "\\d{8}")),
      column_num = str_extract(id, "(_\\d-)"),
      column_num = str_extract(column_num, "\\d"),
      id = str_replace(str_extract(id, "_[\\d_-]+"), "-", "_"),
      page_num = str_extract(id, "\\d{2}$")
    ) %>%
    select(
      id, page_num, date, column_num, title, subtitle,
      content, everything()
    )
  
  dat_ok
}


# func 2. ======================= end -------------------------------