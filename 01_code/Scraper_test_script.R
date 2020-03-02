library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)


Sys.setlocale("LC_TIME", "C")

all.nodes <- c(".sha_right ", " .author", ".subtitle", ".sha_left span", " #FontZoom", " .title")

date <- (today()-1)-0:time_length(interval(ymd(str_c((year((today()-1))-5), str_c(0,month((today()-1))), day((today()-1)) , sep = "-" )),
                                           ymd((today()-1))) , unit = "day")
date <- as.character(date) %>% gsub(pattern = "-" , replacement = "",  x = . )

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()
remDr$navigate("https://tinyurl.com/rq8vom4")

remDr$findElement("css selector", "#login :nth-child(1)")$sendKeysToElement(list("dschulze"))
remDr$findElement("css selector", "#login :nth-child(2)")$sendKeysToElement(list("bonsaibonsai"))
remDr$findElement("css selector", "#login_button")$sendKeysToElement(list(key = "enter"))

f_articles_url<- function(paper_length, date){
  Sys.sleep(round(runif(1,3,5)))
    remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb", date, paper_length, sep="/")) 
  
  

  read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("li h3 a") %>% #
    html_attr("href") %>%
    gsub('(/[a-z]+/\\d+/\\d+/)', replacement = "", x = . ) %>%
    grep('([a-z0-9]{32})', value = T, x = .) %>%
    gsub('^', replacement = str_c(date, paper_length, "", sep = "/"), x = .)
  
  }

f_content <- function(url_articles){
  Sys.sleep(round(runif(1,3,5)))
  remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
                       url_articles, sep = "/"))
  
  map(all.nodes, ~read_html(remDr$getPageSource()[[1]]) %>%
          html_nodes(.x) %>%
          html_text() %>%
          gsub(x = . , pattern = "\\s{10,}", replacement = "")%>%
          gsub(x = ., pattern = "(\\n\\t{3,})", replacement = ""))
  
  }


f_scraper <- function(date){
remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",date[1], 1, sep="/"))
paper_length <- read_html(remDr$getPageSource()[[1]]) %>% 
  html_nodes("#banci_btn > div > div > ul") %>% 
  xml_length()%>%
  1:.

url_articles <- map(paper_length,~f_articles_url(.x, date[1] ))

map(flatten(url_articles), ~f_content(.x ))
}
  

database <- map(date,~f_scraper(.x))  
save(database, file = "Test.RData")
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
