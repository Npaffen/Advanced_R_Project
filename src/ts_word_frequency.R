ts_word_frequency <- function(page_num = 1, start_date = as.Date("2019-01-01"),
                              end_date = today(), eng_word)

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
library(chinese.misc)
library(readr)
library(stringr)
library(timetk)
library(Quandl)

monthly_to_daily  <- function(ts_monthly) {
  df.xts <- xts(ts_monthly$Value,order.by = ts_monthly$Date)
  ts_monthly_daily <- na.locf(merge(df.xts, 
                                    foo=zoo(NA,
                                            order.by=seq(start(df.xts),
                                                         end(df.xts),
                                                         "day",
                                                         drop=F)))[, 1]) %>%
    tk_tbl() %>%
    
    rename("date" = "index", "Value" = "df.xts" ) %>%
    arrange(desc(date))
}


NASDAQ_CNY <- Quandl("NASDAQOMX/NQCN2000CNY", api_key="jsMbTodosyHDq3sWMuzo") %>% 
  rename("date" = "Trade Date", "NASDAQ_Value" = "Index Value")

EERI_mon_real <-Quandl("BIS/EM_MRNTW", api_key="jsMbTodosyHDq3sWMuzo") %>% 
  monthly_to_daily() %>%
  rename("EERI_Value" = "Value")
#Weighted averages of bilateral exchange rates, 
#where the weights are based on manufacturing trade flows and capture direct bilateral trade as well as third-market competition.

Imp_Exp_Price_Ind <- Quandl("BLSN/EIUCOCHNTOT", api_key="jsMbTodosyHDq3sWMuzo") %>%
  monthly_to_daily() %>%
  rename("IEPI_Value" = "Value")


  
#Series: China (Dec. 2003=100) - All commodities
#Index Type: LOCALITY OF ORIGIN
#Not Seasonally Adjusted
#Additional references: BLS's Import/Export Price Indexes Overview page


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

article_2020_p_1_td <- read_rds(str_c(here::here(), "output", "processed_articles_2020_page_01_EN.rds", sep = "/")) %>%
      tidy_text()

article_2020_p_2_td <- read_rds(str_c(here::here(), "output", "processed_articles_2020_page_02_EN.rds", sep = "/")) %>%
      tidy_text()

article_2019_p_1_td <- read_rds(str_c(here::here(), "output", "processed_articles_2019_page_01_EN.rds", sep = "/")) %>%
      tidy_text()

article_2019_p_2_td <- read_rds(str_c(here::here(), "output", "processed_articles_2019_page_01_EN.rds", sep = "/")) %>%
      tidy_text()

  
  
  if (page_num == 1) {database = article_2019_p_1_td %>% full_join(article_2020_p_1_td)
  } else {database = article_2019_p_2_td %>% full_join(article_2020_p_2_td)}


#translate the english word into chinese
#transl <- read_rds(str_c(here::here(),
#                         "output",
#                         "dictionary.rds",
#                         sep = "/")) %>% 
#  filter(english == eng_word | english ==
#                      str_c(gsub(x = eng_word,
#                                 pattern = "^[a-zA-Z]{1}",
#                                 replacement = toupper(substr(eng_word,
#                                                              start = 1,
#                                                              stop = 1)
#                                                       )
#                                 )
#                            )
#  ) %>%
  #select(chinese)
  #transl   

db_filter <- database %>%
  filter(between(.$date, start_date, end_date)) %>%
  filter( .$word  %in% "outbreak")
  
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
  rename (eng_word = n) %>%#or by date with %>% arrange(date)
inner_join(NASDAQ_CNY, by = "date") %>%
  inner_join(Imp_Exp_Price_Ind, by = "date") %>%
  inner_join(EERI_mon_real, by = "date")  %>%
  select(date, EERI_Value, IEPI_Value, NASDAQ_Value, eng_word ) %>%
  gather(key = "variable", value = "value", -date)

tf_idf
  
  
library(ggplot2)
#time-series
tf_idf %>%
  ggplot(aes(x= date,y = eng_word ))+
  geom_line(color = "#00AFBB", size = 1)+
  stat_smooth(
    color = "#FC4E07",
    fill = "#FC4E07",
    method = "loess")+
  scale_x_date(date_labels = "%d%b/%Y")


#have a look at this plot pls
tf_idf %>% ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#E7B399" )) +
  theme_minimal()


####
#Try quantada
article_2020_p_1_td <- read_rds(str_c(here::here(), "output", "processed_articles_2020_page_01_CN.rds", sep = "/")) %>%
  corpus(docid_field = "id", text_field = "content") %>% dfm(remove = stopwords(language = "zh", source = "misc"))
