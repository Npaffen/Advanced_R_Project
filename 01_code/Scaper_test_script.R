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
  Sys.sleep(runif(1,5,10))
    remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb", date[1], paper_length[1], sep="/")) 
  
  

  read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("#titleList a") %>% #
    html_attr("href") %>%
    gsub('(/[a-z]+/\\d+/\\d+/)', replacement = "", x = . ) %>%
    grep('([a-z0-9]{32})', value = T, x = .) %>%
    gsub('^', replacement = str_c(date, paper_length, "", sep = "/"), x = .)
  
  }

f_content <- function(url_articles){
  Sys.sleep(runif(1,5,10))
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

url_articles <- map(paper_length,~f_articles_url(.x, dates ))

map(flatten(url_articles), ~f_content(.x ))
}
  

database <- map(date,~f_scraper(.x))  
save(database, file = "Test.RData")
remDr$screenshot(display = T)
                                                                                                                  






monyear <- dates %>% sub(pattern = "-\\d{2}$",
                         "",
                         x = .) 

sday <- dates %>% sub(pattern = "\\d{4}-\\d{2}-" ,
                      "",
                      x = .)

dates <- map2(monyear, sday, ~str_c(.x, .y, sep = "/")) %>% unlist()


#Zeitung 1 http://paper.people.com.cn/rmrb/html/2020-01/30/nw.D110000renmrb_20200130_3-02.htm nodes : .lai , h1, p

f_url_articles <- function(dates, paper_length){
 
  read_html(str_c("http://paper.people.com.cn/rmrb/html/", 
                                  dates[[1]],
                                  "/nbs.D110000renmrb_0",
                                  paper_length[[1]], 
                                  ".htm", sep = "")) %>%
    html_nodes( "#titleList a") %>%
    html_attr("href") %>%
    map(~str_c(dates[[1]], .x, sep = "/"))
}
all_nodes <- c(".lai "," h2"," h1", "#ozoom p")
f_content <- function(url_articles){
  article <- read_html(str_c("http://paper.people.com.cn/rmrb/html/",
                             url_articles, 
                             sep = "/"))
    map(all_nodes, ~html_nodes(article, .x) %>%
    html_text())
  
}

f_scraper <- function(dates){
  paper_length <- 1:2 # since we only want page 1 & 2
  
  url_articles <- map(paper_length, ~f_url_articles(dates = dates, paper_length = .x))
  

    map(unlist(url_articles), ~f_content(.x))
}
f_scraper(dates[[1]]) 
  
  
  
  
  
  
  
  
  
  

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
