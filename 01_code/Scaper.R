library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)

#### PLEASE USE THIS GUIDE TO INSTALL DOCKER https://rpubs.com/johndharrison/RSelenium-Docker

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
                                    
                                                                              


 