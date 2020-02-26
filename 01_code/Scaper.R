library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)

#### PLEASE USE THIS GUIDE TO INSTALL DOCKER https://rpubs.com/johndharrison/RSelenium-Docker



all.nodes <- c(".sha_right ", " .author", ".subtitle", ".sha_left span", " #FontZoom", " .title")


date <- today()-0:time_length(interval(ymd(str_c((year(today())-5), str_c(0,month(today())), day(today()) , sep = "-" )), ymd(today())) 
                              , unit = "day")
date <- as.character(date) %>% gsub(pattern = "-" , replacement = "",  x = . )

library(RSelenium)
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
  grep('([a-z0-9]{32})' , value = T, x = .)%>%
  gsub('^', replacement = str_c(1,'/',sep = ""), x = .)
#} 
remDr$navigate(str_c("http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",date[1],paper_length[1],articles[1], sep = "/"))
test_one_article<- map(all.nodes, ~read_html(remDr$getPageSource()[[1]])%>%
html_nodes(.x) %>%
  html_text() %>%
  gsub(x = . , pattern = "\\s{10,}", replacement = "")%>%
  gsub(x = ., pattern = "(\\n\\t{3,})", replacement = "")) 
  
  
remDr$screenshot(display = T)
                                                                                                                  



