library(tidytext)
library(stopwords)
library(lubridate)
library(quanteda)
library(Quandl)
library(fredr)
library(tidyverse)


#  helper function ----------------------------------------
# this function tokenizes the articles i.e. converts them into
# tidy text format--a table with **one-token-per-row**, token == a word.

tidy_text <- function(data) {
  data %>%
    select(page_num, date, content) %>%
    unnest_tokens(word, content) %>%
    anti_join(stop_words, by = "word") %>%
    dplyr::filter(!str_detect(word, "\\d+")) %>% # remove any digit
    select(page_num, date, everything())
}


# the main function -------------------------------------------------------

# page_num is the number of newspaper page either 01 or 02, start and end_date
# define the time-span for the analysis, eng_word is the english word a user is looking for,
# econ_data can either be "NASDAQ_CNY" or "dollar_yuan_exch" to plot the eng_word frequency against
# this economic indicator

ts_word_frequency <- function(page_num = "01",
                              start_date = ymd("2019-01-01"),
                              end_date = today() - 1,
                              eng_word = "outbreak",
                              econ_data = "NASDAQ_CNY") {
  
  source(here::here("Chinese.TM.App/R/Scraping/ts_economic_data.R"))

  economic_data <- ts_economic_data(start_date, end_date, econ_data)

  # load and process the articles --------------
  page_01_files <- list.files("Chinese.TM.App/output", "_page_01_EN.rds$", full.names = TRUE)
  page_02_files <- list.files("Chinese.TM.App/output", "_page_02_EN.rds$", full.names = TRUE)

  # check which page should be used for analysis
  if (page_num == "01") {
    database <- map_df(page_01_files, read_rds) %>% tidy_text()
  } else if (page_num == "02") {
    database <- map_df(page_02_files, read_rds) %>% tidy_text()
  }

  # look for the eng_word frequency in the specific time-span, check for lower/upper case of first letter
  db_filter <- database %>%
    dplyr::filter(between(date, start_date, end_date)) %>%
    dplyr::filter(word %in% eng_word |
      word %in% str_to_title(eng_word) |
      word %in% str_to_lower(eng_word))

  # most common words within particular newspaper of page 1
  words_by_newspaper_date_page <-
    db_filter %>%
    count(date, word, sort = TRUE) %>%
    ungroup()

  ##### Finding tf-idf within  newspaper of page_num
  tf_idf <-
    words_by_newspaper_date_page %>%
    bind_tf_idf(word, date, n) %>%
    arrange(date)

  #### plotting
  if (econ_data == "dollar_yuan_exch") {
    colnames(tf_idf)[[3]] <- eng_word
    tf_idf_yuan <- tf_idf %>%
      right_join(economic_data, by = "date") %>%
      select(date, eng_word, value) %>%
      gather(key = "variable", value = "value", -date) %>%
      mutate(value = ifelse(is.na(value), 0, value))

    tf_idf_yuan %>%
      ggplot(aes(date, value)) +
      geom_line(aes(color = variable), size = 1) +
      ggtitle(str_c("Time Series Word Frequency for",
        eng_word, "against Dollar/Yuan Exchange Rate",
        start_date, "-", end_date,
        sep = " "
      )) +
      scale_x_date(date_labels = "%b/%Y", date_breaks = "3 month") +
      theme_minimal()
  } else if (econ_data == "NASDAQ_CNY") {
    tf_idf_NAS <- tf_idf %>%
      mutate(n = normalize_to_x(n, 100))
    colnames(tf_idf_NAS)[[3]] <- eng_word

    tf_idf_NAS <- tf_idf_NAS %>%
      right_join(economic_data, by = "date") %>%
      select(date, NASDAQ_norm, eng_word) %>%
      gather(key = "variable", value = "value", -date) %>%
      mutate(value = ifelse(is.na(value), 100, value))

    tf_idf_NAS %>%
      ggplot(aes(date, value)) +
      geom_line(aes(color = variable), size = 1) +
      ggtitle(str_c("Time Series Word Frequency for",
        eng_word, "against NASDAQ_CNY",
        start_date, "-", end_date,
        sep = " "
      )) +
      scale_x_date(date_labels = "%b/%Y", date_breaks = "3 month") +
      theme_minimal() +
      theme(legend.position = "bottom")
  } else {
    tf_idf %>% ggplot(aes(date, n)) +
      geom_line(color = "#00AFBB", size = 1) +
      stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") +
      ggtitle(str_c("Time Series Word Frequency for",
        eng_word, start_date, "-", end_date,
        sep = " "
      )) +
      scale_x_date(date_labels = "%b/%Y", date_breaks = "3 month") +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
}
