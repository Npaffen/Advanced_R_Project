library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)

map(x, ~today() - .x)
Sys.setlocale("LC_TIME", "C")

#target_regex <- "(XTM)|(xtm)|((k|K)(i|I|1|11)(d|D)(n|N).)|(Are)\\s(you)\\s(in)| (LOAN)|(AR(\\s|\\S)[0-9])|((B|b)(i|1|l)tc.)|(Coupon)|
#(Plastic.King)|(organs)|(SILI)|(Electric.Cigarette.Machine)|(Drone.Air.\\+)|(Hover)"
all.nodes <- c(".lai , h1, p")

#Zeitung 1 http://paper.people.com.cn/rmrb/html/2020-01/30/nw.D110000renmrb_20200130_3-02.htm nodes : .lai , h1, p
  paper <- read_html("http://paper.people.com.cn/rmrbhwb/html/2020-02/14/node_865.htm#")
  test <-  map(all.nodes, ~ html_nodes(paper, .x) %>%
                       html_text()) %>%
  as_tibble(.name_repair = "unique")%>%


scraper_info <- function(pages){
  newspaper <- read_html(paste("http://ipaidabribe.com/reports/paid?page", pages, sep = "="))
  map(all.nodes, ~ html_nodes(bribe, .x) %>%
        html_text()) %>%
    invisible(as_tibble(.name_repair = "unique")) %>%
    mutate(html =(
      html_nodes(bribe, ".read-more") %>% 
        html_attr("href") %>% 
        as_tibble(.name_repair = "unique"))$value) %>% 
    filter(str_detect(...1, target_regex, negate = TRUE)) %>% 
    mutate(Report = map_chr(html, ~read_html(.x) %>%  
                              html_node(".body-copy-lg") %>% 
                              html_text))
}


pages <- seq(10, 1300, by = 10)
bribe <- map_df(pages, ~scraper_info(.x)) %>% rename(  "Title"= ...1 ,   "Bribe" = ...2, "Date_char" = ...3,
                                                       "Location" = ...4 , "Department" = ...5 ) %>% 
  mutate(Date = as.Date(Date_char, format = "%B %d, %Y")) %>% select(-Date_char, -html) %>%  #format dates
  filter(Date >= as.Date("2018-12-31") & Date <= as.Date("2020-01-01")) #filter only for dates of 2019

bribe$Location <- modify(bribe$Location, ~gsub("\\s{2,}", "", .x)) #Delete more than one whitespace