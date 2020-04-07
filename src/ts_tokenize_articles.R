tokenize_aritcles_db <- function(page_num = page_num, )
##### function   to tokenize the articles
tidy_text <- function(data) {
  
  
  select(data, id, date, content) %>%
    mutate(content = str_split(content, "\\|\\|")) %>%
    unnest(cols = content) %>%
    group_by(id) %>%
    mutate(para_id = row_number()) %>%
    ungroup() %>%
    tidytext::unnest_tokens(word, content) %>%
    dplyr::filter(!str_detect(word, "\\d+")) %>% # remove any digit
    mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
    select(id, date, month, para_id, word, everything())
}
##### load and process the articles




##### check which page should be used for analysis
if (page_num == 1) {
  article_2020_p_1_td <- read_rds(str_c(here::here(),
                                        "output",
                                        "processed_articles_2020_page_01_EN.rds",
                                        sep = "/")) %>%
    tidy_text() 
  
  article_2019_p_1_td <- read_rds(str_c(here::here(),
                                        "output",
                                        "processed_articles_2019_page_01_EN.rds",
                                        sep = "/")) %>%
    tidy_text()
  
  database = article_2019_p_1_td %>% full_join(article_2020_p_1_td)
} else {
  article_2020_p_2_td <- read_rds(str_c(here::here(),
                                        "output",
                                        "processed_articles_2020_page_02_EN.rds",
                                        sep = "/")) %>%
    tidy_text()
  
  
  
  article_2019_p_2_td <- read_rds(str_c(here::here(),
                                        "output",
                                        "processed_articles_2019_page_01_EN.rds",
                                        sep = "/")) %>%
    tidy_text()
  
  database = article_2019_p_2_td %>% full_join(article_2020_p_2_td)}

database