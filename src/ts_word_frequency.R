ts_word_frequency <- function(page = 1, start_date = as.Date("2019-01-01"), end_date = today(), eng_word   )

library(dplyr)
library(purrr)
library(stringr)
library(tidytext)
library(readr)
library(stopwords)
library(tidyr)
library(lubridate)

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


article_2020_p_1_td <- read_rds("data/article_data_2020_page_01.rds") %>%
  tidy_text()

article_2020_p_2_td <- read_rds("data/article_data_2020_page_02.rds") %>%
  tidy_text()

article_2019_p_1_td <- read_rds("data/article_data_2019_page_01.rds") %>%
  tidy_text()

article_2019_p_2_td <- read_rds("data/article_data_2019_page_02.rds") %>%
  tidy_text()

all_articles_p_1 <- article_2019_p_1_td %>% full_join(article_2020_p_1_td)
all_articles_p_2 <- article_2019_p_2_td %>% full_join(article_2020_p_2_td)
  
  
  if (page == 1) {database = all_articles_p_1} else {database = all_articles_p_2}

#translate the english word into chinese
transl <- read_rds(str_c(here::here(),
                         "output",
                         "dictionary.rds",
                         sep = "/")) %>% 
  filter(english == eng_word | english ==
                      str_c(gsub(x = eng_word,
                                 pattern = "^[a-zA-Z]{1}",
                                 replacement = toupper(substr(eng_word,
                                                              start = 1,
                                                              stop = 1)
                                                       )
                                 )
                            )
  ) %>%
  select(chinese)
         

db_filter <- database %>%
  filter(between(.$date, start_date, end_date)) %>%
  filter(word %in% transl$chinese)
  
#most common words in the dataset  
newspaper_words <- db_filter %>% count(word, sort = T)

#most common words within particular newspaper of page 1 
words_by_newspaper_date_page <- db_filter %>%
  count(date, word, sort = T) %>%
  ungroup()

#Finding tf-idf within  newspaper of page 1 
tf_idf <- words_by_newspaper_date_page %>%
  bind_tf_idf(word, date, n) %>%
  arrange(desc(tf_idf))%>%
  rename( eng_word = n)#or by date with %>% arrange(date)
tf_idf

#time-series
tf_idf %>%
  ggplot(aes(x= date,y = eng_word))+
  geom_line(color = "#00AFBB", size = 1)+
  stat_smooth(
    color = "#FC4E07",
    fill = "#FC4E07",
    method = "loess")+
  scale_x_date(date_labels = "%d%b/%Y")

