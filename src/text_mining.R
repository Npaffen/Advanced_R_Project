library(dplyr)
library(purrr)
library(stringr)
library(tidytext)
library(stopwords)


# convert into tidy text format--a table with **one-token-per-row**,
# token == a word.

tidy_text <- function(data) {
  chinese_stop_words <- tibble(word = stopwords("zh", "misc"))

  select(data, id, date, content) %>%
    mutate(content = str_split(content, "\\|\\|")) %>%
    unnest(cols = content) %>%
    group_by(id) %>%
    mutate(para_id = row_number()) %>%
    ungroup() %>%
    tidytext::unnest_tokens(word, content) %>%
    anti_join(chinese_stop_words, by = "word") %>%
    dplyr::filter(!str_detect(word, "\\d+")) %>% # remove any digit
    mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
    select(id, date, month, para_id, word, everything())
}

# Casting tidy text data into a matrix, DocumentTermMatrix.

cast_into_dtm <- function(tidy_data) {
  tidy_data %>%
    count(id, word, name = "count", sort = TRUE) %>%
    cast_dtm(
      document = id,
      term = word,
      value = count
    )
}

# Let's do it

# 1. tidy it, one-token-per-row-per article

article_2020_td <- read_rds("data/article_data_ok_2020.rds") %>%
  tidy_text()

article_2019_td <- read_rds("data/article_data_ok_2019.rds") %>%
  tidy_text()

# 2. document/article term matrix

article_2020_dtm <- article_2020_td %>% cast_into_dtm()
article_2019_dtm <- article_2019_td %>% cast_into_dtm()

# 3. saving

# nms <- ls(pattern = "_\\d{4}_(td|dtm)")
# 
# walk2(.x = list(article_2019_td, article_2019_dtm,
#            article_2020_td, article_2020_dtm),
#       .y = nms, ~saveRDS(.x, paste0("output/", .y, ".rds")))


# Analyzing word and document frequency: 
# term frequency(tf) and/or inverse document frequency (idf)

article_2019_td %>%
  count(date, word) %>%
  bind_tf_idf(word, date, n) %>%
  arrange(desc(-tf_idf))
