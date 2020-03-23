library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)
library(magick)
library(rlist)
#### PLEASE USE THIS GUIDE TO INSTALL DOCKER https://rpubs.com/johndharrison/RSelenium-Docker

Sys.setlocale("LC_TIME", "C")

all.nodes <- c(".sha_right ", " .author", ".subtitle", 
               ".sha_left span", " #FontZoom", " .title")
captcha_tester <- read_html("captcha.html")

dates <- seq(from = as.Date('2015-01-01'),
             to = as.Date('2020-03-23'),
             by = 1)


dates <- dates %>% gsub(pattern = "-" ,
                        replacement = "",
                        x = .)

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
remDr$open()
  remDr$navigate("https://tinyurl.com/rq8vom4")

remDr$findElement("css selector", "#login :nth-child(1)")$sendKeysToElement(list("dschulze"))
remDr$findElement("css selector", "#login :nth-child(2)")$sendKeysToElement(list("bonsaibonsai"))
remDr$findElement("css selector", "#login_button")$sendKeysToElement(list(key = "enter"))

f_articles_url <- function(paper_length, dates) {
  Sys.sleep(runif(1,5,10))
  
  remDr$navigate(
    str_c(
      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
      dates,
      paper_length,
      sep = "/"
    )
  )
  
  
  
  read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("li h3 a") %>%
    html_attr("href") %>%
    gsub('(/[a-z]+/\\d+/\\d+/)',
         replacement = "",
         x = .) %>%
    grep('([a-z0-9]{32})', value = T, x = .) %>%
    gsub('^',
         replacement = str_c(dates, paper_length, "", sep = "/"),
         x = .)
  
}

f_captcha_content <- function(){
 
  remDr$screenshot(file = "captcha.png") #screenshot of the full site with captcha
  magick::image_crop(image_read("captcha.png"),geometry_area(97,38.5,480,290) ) %>%
    image_write( "captcha.png", format = "png") #crop the captcha
    
  
  
  
  cap_POST <-  httr::POST(url = "http://2captcha.com/in.php?key=d3ce30748e45dc73365f4e327acaebee&method=post",
                          encode = "multipart",
                          body = list(file = upload_file(path = "captcha.png"))) #send the captcha to the api
  captcha_ID <- content(cap_POST) %>%
    xml_child() %>%
    xml_text() %>%
    gsub('[^0-9]+',
         replacement = "",
         x =.) #catch the ticket ID
  
  Sys.sleep(20L) #wait untill solving
  
  if(httr::GET(url = str_c("https://2captcha.com/res.php?key=d3ce30748e45dc73365f4e327acaebee&action=get&id=",
                           captcha_ID,
                           sep = ""))%>% 
     content() == "CAPCHA_NT_READY") #check if the captcha is solved
    Sys.sleep(5L)# add extra time if not
  
    
    captcha_key <- httr::GET(url = str_c("https://2captcha.com/res.php?key=d3ce30748e45dc73365f4e327acaebee&action=get&id=",
                                         captcha_ID,
                                         sep = "")) %>% 
    content() %>%
    gsub(x = .,
         pattern ="[OK|]",
         replacement = "")# grab the captcha code
  
  remDr$findElement("css selector", "#validateCode")$sendKeysToElement(list(captcha_key, key = "enter")) # post it to the form
  

  map(
    all.nodes,
    ~ read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(.x) %>%
      html_text() %>%
      gsub(
        x = . ,
        pattern = "\\s{10,}",
        replacement = ""
      ) %>%
      gsub(
        x = .,
        pattern = "(\\n\\t{3,})",
        replacement = ""
      )
  ) # grab the article content
  }

f_content <- function(url_articles) {
  remDr$navigate(
    str_c(
      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
      url_articles,
      sep = "/"
    )
  )
  
  

  
  if (read_html(remDr$getPageSource()[[1]]) %>% xml_child( 2)%>% xml_child( 2) %>% 
html_text() == html_text(xml_child(xml_child(captcha_test, 2), 2)))#check if the archive requests a captcha
  f_captcha() #solve the captcha if necessary
 
  
  
  
  else if (read_html(remDr$getPageSource()[[1]]) %>% xml_child( 2)%>% xml_child( 2) %>% 
        html_text() == html_text(xml_child(xml_child(captcha_test, 2), 2))) # check if the captcha was solved correctly
      f_captcha() # solve again if not true
  
  else 
  map(
    all.nodes,
    ~ read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(.x) %>%
      html_text() %>%
      gsub(
        x = . ,
        pattern = "\\s{10,}",
        replacement = ""
      ) %>%
      gsub(
        x = .,
        pattern = "(\\n\\t{3,})",
        replacement = ""
      )
  ) #grab the article content
  
}


f_scraper <- function(dates) {

  paper_length <- 1:2 # Only page 1 and 2 of each newspaper will be analyzed

  
  url_articles <- map(paper_length,  ~ f_articles_url(.x, dates)) #grab the article adresses
  
  map(flatten(url_articles), ~ f_content(.x)) #grab the content
}


#shell("git.lnk config --global user.email \"nils.paffen@wopic.de\"")
#shell("git.lnk config --global user.name \"npaffen\"")

archive_partly <- dates <- seq(from = as.Date('2019-01-01'),
                               to = as.Date('2020-03-23'),
                               by = 1)

time_span_archive_monthly <- seq(from = 1,
                                 to = length(dates)-length(archive_partly),
                                 by = 30)
j = 12
for (i in seq_along(sort(time_span_archive_monthly, decreasing = T))){
  j = j + 1
  
  assign(str_c("database","_m", j, sep = ""),
         map(dates[(length(dates)-i-30):(length(dates)-i)],
             ~ f_scraper(.x)))
  list.save(x =str_c("database","_m", j, sep = ), str_c("database","_m", j,".rds", sep = ""))
}





