# page_num is the number of newspaper page either 1 or 2, start and end_date 
#define the time-span for the anlysis, eng_word is the english word a user is looking for,
#econ_data can either be "NASDAQ_CNY" or "dollar_yuan_exch" to plot the eng_word frequency against
# this economic indicator
ts_word_frequency <- function(page_num = 1, start_date = as.Date("2019-01-01"),
                              end_date = today(), eng_word = "outbreak", econ_data = "NASDAQ_CNY") {

  ## please add a description, what is eng_word, etc.
  ## adding a table of contents also makes sense at this length
  
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
  
#### function to convert monthly econ data to daily econ data
monthly_to_daily  <- function(ts_monthly) {
  df.xts <- xts(ts_monthly$Value,order.by = ts_monthly$Date)
  ts_monthly_daily <- na.locf(merge(df.xts, 
                                    foo=zoo(NA,
                                            order.by=seq(start(df.xts),
                                                         end(df.xts),
                                                         "day",
                                                         drop=F)))[, 1]) %>%
    tk_tbl() %>%
    
    rename("date" = "index", "Value" = "df.xts" )
}
#### function to standardize a time-series to a specific start value
normalization_to_x <- function(dataset, x){
  #factor_val <- solve(dataset[1],100)
  #normalized <- dataset * factor_val
  factor_val <- dataset[1] - x
  normalize <- dataset - factor_val
}
##### dowload economic data, adjust time span, standardize
current_key <- Sys.getenv("FRED_API_KEY")
fredr_set_key("c66bdbc4919216612f5cb63ec4994a81")


if(econ_data == "dollar_yuan_exch") {
dollar_yuan_exch <- fredr(
  series_id = "DEXCHUS",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "d") %>%
  mutate(value = ifelse(is.na(value) == T, value[is.na(value)+1], value)) %>% 
  #there is a missing value at 1st of Jan 2019, correct it by setting it to the value of 2nd Jan
  select(date, value_norm )
} else if (econ_data == "NASDAQ_CNY"){
#
NASDAQ_CNY <- Quandl("NASDAQOMX/NQCN2000CNY",
                     api_key="jsMbTodosyHDq3sWMuzo",
                     transform = "normalize",
                     order = "asc",
                     collapse = "daily") %>%
  mutate("date" = .$`Trade Date`, "NASDAQ_Value" = .$`Index Value`) %>% 
  arrange(date) %>%
  filter(between(.$date, start_date, end_date)) %>%
  mutate("NASDAQ_norm" = normalization_to_x(.$NASDAQ_Value, 100)) %>%
  select(date, NASDAQ_norm )
} 
 


##### function to process the articles into a document-token-tibble
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
article_2020_p_1_td <- read_rds(str_c(here::here(),
                                      "output",
                                      "processed_articles_2020_page_01_EN.rds",
                                      sep = "/")) %>%
  tidy_text() 

article_2020_p_2_td <- read_rds(str_c(here::here(),
                                      "output",
                                      "processed_articles_2020_page_02_EN.rds",
                                      sep = "/")) %>%
      tidy_text()

article_2019_p_1_td <- read_rds(str_c(here::here(),
                                      "output",
                                      "processed_articles_2019_page_01_EN.rds",
                                      sep = "/")) %>%
      tidy_text()

article_2019_p_2_td <- read_rds(str_c(here::here(),
                                      "output",
                                      "processed_articles_2019_page_01_EN.rds",
                                      sep = "/")) %>%
      tidy_text()

  
##### check which page should be used for analysis
  if (page_num == 1) {database = article_2019_p_1_td %>% full_join(article_2020_p_1_td)
  } else {database = article_2019_p_2_td %>% full_join(article_2020_p_2_td)}


'#translate the english word into chinese
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
  transl   
'
##### look for the eng_word frequency in the specific time-span
db_filter <- database %>%
  filter(between(.$date, start_date, end_date)) %>%
  filter( .$word  %in% eng_word )
  
#####most common words in the dataset  
newspaper_words <- db_filter %>% count(word, sort = T)

#####most common words within particular newspaper of page 1 
words_by_newspaper_date_page <- db_filter %>%
  count(date, word, sort = T) %>%
  ungroup()

#####Finding tf-idf within  newspaper of page_num
tf_idf <- words_by_newspaper_date_page %>%
  bind_tf_idf(word, date, n)  %>%
  arrange(date) 
####produce the ggplots
if (econ_data == "dollar_yuan_exch"){
  tf_idf_yuan <- tf_idf %>%
    rename(eng_word = n) %>%
    right_join(dollar_yuan_exch, by = "date") %>%
  select(date, value, eng_word ) %>%
  gather(key = "variable", value = "value", -date) %>%
    mutate(value = ifelse(is.na(value) == T, 0, value))
  
  tf_idf_yuan$variable[
    tf_idf_yuan$variable = "eng_word"
    ] <- eng_word 
    
  tf_idf_yuan %>%
    ggplot(aes(x = date, y = value)) + 
    geom_line(aes(color = variable), size = 1)+
    theme_minimal()
  } else if (econ_data == "NASDAQ_CNY"){
   tf_idf_NAS <- tf_idf %>%
    mutate(eng_word = normalization_to_x(n, 100)) %>%
  right_join(NASDAQ_CNY, by = "date")%>%
    select(date, NASDAQ_norm, eng_word ) %>%
    gather(key = "variable", value = "value", -date) %>%
     mutate(value = ifelse(is.na(value) == T, 100, value))
   
  tf_idf_NAS$variable[
    tf_idf_NAS$variable = "eng_word"
    ] <- eng_word 

   tf_idf_NAS %>%
     ggplot(aes(x = date, y = value)) + 
    geom_line(aes(color = variable), size = 1)+
    theme_minimal()
   } else {
  tf_idf %>%
  ggplot(aes(x= date,y = n ))+
  geom_line(color = "#00AFBB", size = 1)+
  stat_smooth(
    color = "#FC4E07",
    fill = "#FC4E07",
    method = "loess")+
  scale_x_date(date_labels = "%d%b/%Y")
  }



}

ts_word_frequency(page_num = 1, start_date = as.Date("2019-01-01"),
                              end_date = today(), eng_word = "outbreak", econ_data = "NASDAQ_CNY")
  