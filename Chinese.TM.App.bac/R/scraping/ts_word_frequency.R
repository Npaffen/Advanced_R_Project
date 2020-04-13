  # page_num is the number of newspaper page either 1 or 2, start and end_date
#define the time-span for the anlysis, eng_word is the english word a user is looking for,
#econ_data can either be "NASDAQ_CNY" or "dollar_yuan_exch" to plot the eng_word frequency against
# this economic indicator
ts_word_frequency <- function(page_num = 1, start_date = as.Date("2019-01-01"), end_date = today()-1,
                              eng_word = "outbreak",
                              econ_data = "NASDAQ_CNY") {

  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidytext)
  library(readr)
  library(stopwords)
  library(tidyr)
  library(lubridate)
  library(quanteda)
  library(readr)
  library(Quandl)
  library(fredr)
  library(ggplot2)  
  source(str_c(here::here(), "R/scraping/ts_economic_data.R", sep = "/"))
  source(str_c(here::here(), "R/scraping/ts_economic_data.R", sep = "/"))

  
  economic_data <- ts_economic_data(start_date = start_date,
              end_date = end_date,
              econ_data = econ_data)


##### function   to tokenize the articles
tidy_text <- function(data) {


  select(data, id,
         date,
         content) %>%
    mutate(content = str_split(content,
                               "\\|\\|")) %>%
    unnest(cols = content) %>%
    group_by(id) %>%
    mutate(para_id = row_number()) %>%
    ungroup() %>%
    tidytext::unnest_tokens(word,
                            content) %>%
    dplyr::filter(!str_detect(word,
                              "\\d+")) %>% # remove any digit
    mutate(month = lubridate::month(date,
                                    label = TRUE,
                                    abbr = FALSE)) %>%
    select(id,
           date,
           month,
           para_id,
           word,
           everything())
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

  database <- article_2019_p_1_td %>% full_join(article_2020_p_1_td)
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

  database <- article_2019_p_2_td %>% full_join(article_2020_p_2_td)
}



#### function to standardize a time-series to a specific start value
normalization_to_x <- function(dataset, x){
  factor_val <- dataset[1] - x
  normalize <- dataset - factor_val
}

##### look for the eng_word frequency in the specific time-span, check for lower/upper case of first letter
db_filter <- database %>%
  filter(between(.$date,
                 start_date,
                 end_date)) %>%
  filter( .$word  %in% eng_word | .$word  %in%
            str_c(gsub(x = eng_word,
                       pattern = "^[a-zA-Z]{1}",
                       replacement = toupper(substr(eng_word,
                                                    start = 1,
                                                    stop = 1)
                                             )
                       )
                  )| .$word  %in%
            str_c(gsub(x = eng_word,
                       pattern = "^[a-zA-Z]{1}",
                       replacement = tolower(substr(eng_word,
                                                    start = 1,
                                                    stop = 1)
                                             )
                       )
                  )
          )

#####most common words in the dataset
newspaper_words <- db_filter %>% count(word,
                                       sort = T)

#####most common words within particular newspaper of page 1
words_by_newspaper_date_page <- db_filter %>%
  count(date,
        word,
        sort = T) %>%
  ungroup()

#####Finding tf-idf within  newspaper of page_num
tf_idf <- words_by_newspaper_date_page %>%
  bind_tf_idf(word,
              date,
              n)  %>%
  arrange(date)
####produce the ggplots
if (econ_data == "dollar_yuan_exch"){
  tf_idf_yuan <- tf_idf %>%
    rename(eng_word = n) %>%
    right_join(economic_data,
               by = "date") %>%
  select(date,
         value, eng_word ) %>%
  gather(key = "variable",
         value = "value", -date) %>%
    mutate(value = ifelse(is.na(value) == T,
                          0,
                          value))


  tf_idf_yuan %>%
    ggplot(aes(x = date,
               y = value)) +
    geom_line(aes(color = variable),
              size = 1)+
    ggtitle(str_c("Time Series Word Frequency for",
                  eng_word, "against Dollar/Yuan Exchange Rate",
                  as.character(start_date),
                  "-",
                  as.character(end_date),
                  sep = " " ))+
    scale_x_date(date_labels = "%b/%Y",
                 date_breaks = "3 month")+
    theme_minimal()
  } else if (econ_data == "NASDAQ_CNY"){
   tf_idf_NAS <- tf_idf %>%
    mutate(eng_word = normalization_to_x(n, 100)) %>%
  right_join(economic_data, by = "date")%>%
    select(date,
           NASDAQ_norm,
           eng_word ) %>%
    gather(key = "variable",
           value = "value",
           -date) %>%
     mutate(value = ifelse(is.na(value) == T,
                           100,
                           value))


   tf_idf_NAS %>%
     ggplot(aes(x = date, y = value)) +
    geom_line(aes(color = variable), size = 1)+
     ggtitle(str_c("Time Series Word Frequency for",
                   eng_word,
                   "against NASDAQ_CNY",
                   as.character(start_date),
                   "-",
                   as.character(end_date),
                   sep = " " ))+
     scale_x_date(date_labels = "%b/%Y",
                  date_breaks = "3 month")+
    theme_minimal()
   } else {
  tf_idf %>%
  ggplot(aes(x= date,
             y = n ))+
  geom_line(color = "#00AFBB",
            size = 1)+
  stat_smooth(
    color = "#FC4E07",
    fill = "#FC4E07",
    method = "loess")+
       ggtitle(str_c("Time Series Word Frequency for",
                     eng_word,
                     as.character(start_date),
                     "-",
                     as.character(end_date),
                     sep = " " ))+
       scale_x_date(date_labels = "%b/%Y", date_breaks = "3 month")+
       theme_minimal()
     }



}

