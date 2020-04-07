ca_content <- function(url_articles) {
  
  remDr$navigate(
    str_c(
      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
      url_articles,
      sep = "/"
    )
  )
  
  if (read_html(remDr$getPageSource()[[1]]) %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() == timeout %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text()) message(str_c("Timeout after",
                                 as.numeric(Sys.time()-start_time,
                                            units = "mins") ),
                           sep = " ")

  source(str_c(here::here(),"src", "ca_captcha.R", sep = "/"))
  
  if (read_html(remDr$getPageSource()[[1]]) %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() %>%
      gsub(x = .,
           pattern ="(\\n)+|(\\t)+|\\s|(\\r)+",
           replacement = "") ==  captcha_tester %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() %>%
      gsub(x = .,
           pattern ="(\\n)+|\\r|\\s",
           replacement = "")) { # check if the archive requests a captcha
    ca_captcha()
  } # solve the captcha if necessary
  
  
  
  
  if (read_html(remDr$getPageSource()[[1]]) %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() %>%
      gsub(x = .,
           pattern ="(\\n)+|(\\t)+|\\s|(\\r)+",
           replacement = "") ==  captcha_tester %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() %>%
      gsub(x = .,
           pattern ="(\\n)+|\\r|\\s",
           replacement = "")) { # check if the captcha was solved correctly
    ca_captcha()
  } # solve again if not true
  
  
  page <- read_html(remDr$getPageSource()[[1]])
  
  get_title <- compose(html_text,   
                       partial(html_nodes, css = ".title"),
                       partial(html_nodes, css = "#detail_pop_content"),
                       .dir = "backward"
  )
  
  get_subtitle <- compose(html_text,
                          partial(html_nodes, css = ".subtitle"),
                          partial(html_nodes, css = "#detail_pop_content"),
                          .dir = "backward"
  )
  
  get_author <- compose(html_text,
                        partial(html_nodes, css = ".author"),
                        partial(html_nodes, css = "#detail_pop_content"),
                        .dir = "backward"
  )
  
  get_paragraph <- compose(html_text,
                           partial(html_nodes, css = "p"),
                           partial(html_nodes, css = "#detail_pop_content"),
                           .dir = "backward"
  )
  
  get_date <- compose(html_text,
                      partial(html_nodes, css = ".sha_left span:nth-child(1)"),
                      partial(html_nodes, css = "#detail_pop_content"),
                      .dir = "backward"
  )
  
  get_page_num <- compose(html_text,
                          partial(html_nodes, css = ".sha_left span:nth-child(2)"),
                          partial(html_nodes, css = "#detail_pop_content"),
                          .dir = "backward"
  )
  
  df_l <- list(
    title = get_title(page),
    subtitle = get_subtitle(page),
    author = get_author(page),
    content = get_paragraph(page),
    date = get_date(page),
    PageNumber = get_page_num(page) 
  ) %>% 
    map(~if(length(.x) == 0){.x = NA} else .x = .x)
  
  
  
  df <- tibble(
    title = df_l$title,
    subtitle = df_l$subtitle,
    date = df_l$date,
    PageNumber = df_l$PageNumber,
    content = paste0(df_l$content, collapse = ""),
    num_paragraph = length(df_l$content),
    id = url_articles
  ) # grab the article content
  
  df
}