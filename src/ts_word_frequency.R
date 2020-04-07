  e# page_num is the number of newspaper page either 1 or 2, start and end_date 
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
library(tidytextxt)
library(readr)
library(stopwords)
library(tidyr)
library(lubridate)
library(quanteda)
library(readr)
library(Quandl)
library(fredr)
library(ggplot2)  
  
source(str_c(here::here(), "src", "ts_economic_data.R", sep = "/"))
 
 economic_data(start_date = start_date,
              end_date = end_date,
              econ_data = econ_data)


database <- tokenize_aritcles_db()

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
  right_join(econ, by = "date")%>%
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
                              end_date = today(), eng_word = "New Crown pneumonia", econ_data = "NASDAQ_CNY")
  