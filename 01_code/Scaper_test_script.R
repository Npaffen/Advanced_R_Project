library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)
require()
map(x, ~today() - .x)

Sys.setlocale("LC_TIME", "C")

#target_regex <- "(XTM)|(xtm)|((k|K)(i|I|1|11)(d|D)(n|N).)|(Are)\\s(you)\\s(in)| (LOAN)|(AR(\\s|\\S)[0-9])|((B|b)(i|1|l)tc.)|(Coupon)|
#(Plastic.King)|(organs)|(SILI)|(Electric.Cigarette.Machine)|(Drone.Air.\\+)|(Hover)"
all.nodes <- c(".sha_right ", " .author", ".subtitle", ".sha_left span", " #FontZoom", " .title")


date <- today()-0:time_length(interval(ymd(str_c((year(today())-5), str_c(0,month(today())), day(today()) , sep = "-" )), ymd(today())) , unit = "day")
date <- as.character(dates) %>% gsub(pattern = "-" , replacement = "",  x = . )

library(RSelenium)
RSelenium::startServer()
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()
remDr$navigate("https://tinyurl.com/rq8vom4")

remDr$findElement("css selector", "#login :nth-child(1)")$sendKeysToElement(list("dschulze"))
remDr$findElement("css selector", "#login :nth-child(2)")$sendKeysToElement(list("bonsaibonsai"))
remDr$findElement("css selector", "#login_button")$sendKeysToElement(list(key = "enter"))

#function(date){
date_url <- str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",date[1], 1, sep="/")
remDr$navigate(date_url)

paper_length <- read_html(remDr$getPageSource()[[1]]) %>% 
  html_nodes("#banci_btn > div > div > ul") %>% 
  xml_length()%>%
  1:.

#}
#all_articles <-function(paper_length, date) {
  remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",date[1], 1, sep="/"))
  articles <- read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("li h3 a") %>%
  html_attr("href") %>%
  gsub('(/[a-z]+/\\d+/\\d/)', replacement ="", x = . ) %>%
  grep('([a-z0-9]{32})' , value = T, x = .)
#} 
remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",date[1],paper_length[1],articles[1], sep = "/"))
test_one_article<- map(all.nodes, ~read_html(remDr$getPageSource()[[1]])%>%
html_nodes(.x) %>%
  html_text() %>%
  gsub(x = . , pattern = "\\s{10,}", replacement = "")%>%
  gsub(x = ., pattern = "(\\n\\t{3,})", replacement = "")) 
  
  
remDr$screenshot(display = T)
                                                                                                                  




xsummary(n)


url <- 
#Direct access to the login, using tinyurl to keep things short, tinyURL page states : urls "never expire"
session <- html_session("https://tinyurl.com/rq8vom4")
session
form <- html_form(session)[[1]]

form <- set_values(form,
                   j_username = "dschulze",
                   j_password = "bonsaiboi")

submit_form(session , form, submit = '_eventId_proceed',config(referer = session$url))
session
paper <-   jump_to(session,"http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb/20200217/1")
paper  
session_history(session2)
session2


  #Zeitung 1 http://paper.people.com.cn/rmrb/html/2020-01/30/nw.D110000renmrb_20200130_3-02.htm nodes : .lai , h1, p
  paper <- read_html("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb/20200217/1")
  test <- html_nodes(paper, "li h3") %>%
                       html_text()


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
