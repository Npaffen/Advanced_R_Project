
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

captcha_tester <- read_html("captcha.html")

dates <-map(c(2015:2018), ~make_dates(year = .x ,month = 1:12, all_dates = T))

date_url <-map(dates,
               ~  as.Date(
                 unlist(
                   unname(.x)),
                 origin) %>%
                 gsub(
                   pattern = "-",
                   replacement = "",
                   x = .
                   )
               )









f_scraper <- function(date_url) {
  start_time <- Sys.time()
  paper_length <- 1:2 # Only page 1 and 2 of each newspaper will be analyzed


  url_articles <- map(paper_length, ~ f_articles_url(.x, date_url)) # grab the article adresses

  content <-tryCatch(map_df(flatten(url_articles), ~ f_content(.x)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #grab the content
  Sys.sleep(runif(1,600,630))
  content
  }


#shell("git.lnk config --global user.email \"nils.paffen@wopic.de\"")
#shell("git.lnk config --global user.name \"npaffen\"")



time_span_archive_monthly <- seq(from = 1,
                                 to = length(dates),
                                 by = 30)
j = 12
y = 2018
login()
for (i in time_span_archive_monthly){
  
  
  assign(str_c("database", "m", j,"year", y, sep =  "_"),
         map_df(date_url[(length(date_url)-i+1):(length(date_url)-i-28)],
             ~ f_scraper(.x))) %>%
  save(file = str_c("database_", "m_", j, "_", "year", "_", y, ".rds", sep = ""))
  if (j == 1) 
    j = 12 
  else j = j-1
  if (j==1)
    y = y - 1 
}

