library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)

#### PLEASE USE THIS GUIDE TO INSTALL DOCKER https://rpubs.com/johndharrison/RSelenium-Docker

Sys.setlocale("LC_TIME", "C")

all.nodes <- c(".sha_right ", " .author", ".subtitle", 
               ".sha_left span", " #FontZoom", " .title")

# date <- (today()-1)-0:time_length(interval(ymd(str_c((year((today()-1))-5), str_c(0,month((today()-1))), day((today()-1)) , sep = "-" )),
#                                            ymd((today()-1))) , unit = "day")

# This will get the same result, what do you think?
# And, `date` is an existing function so we should probabily
# avoid overwriting it, so `dates` would be a good name.

dates <- seq(from = as.Date('2015-01-01'),
             to = as.Date('2019-01-01'),
             by = 1)
# the result is already is a character vector.

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

f_content <- function(url_articles) {
  Sys.sleep(runif(1,5,10))
  remDr$navigate(
    str_c(
      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
      url_articles,
      sep = "/"
    )
  )
  
  
  #captcha <- read_html(remDr$getPageSource()[[1]])
  #download_html(remDr$navigate(""), file = "captcha.hmtl")
  
  if (read_html(remDr$getPageSource()[[1]])%>%
      html_nodes() %>% html_text()  == captcha_text) message(str_c("Captcha Code after", Sys.time() - start_time, sep = " " ))
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
  )
  
}


f_scraper <- function(dates) {
  start_time <- Sys.time()
  #  remDr$navigate(
#    str_c(
#      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
#      dates,
#      1,
#      sep = "/"
#    )
#  )
  paper_length <- 1:2 # read_html(remDr$getPageSource()[[1]]) %>% 
#    html_nodes("#banci_btn > div > div > ul") %>%
#    xml_length() %>%
#    1:.
  
  url_articles <- map(paper_length,  ~ f_articles_url(.x, dates[1]))
  
  map(flatten(url_articles), ~ f_content(.x))
}

data
database_m1 <- map(dates[(length(dates)-6):length(dates)],  ~ f_scraper(.x))
database_m2 <- map(dates[(length(dates)-13):(length(dates)-7)],  ~ f_scraper(.x))


#######################################NEW SCRAPER########
dates <- seq(from = as.Date('2019-01-01'),
            to = as.Date('2020-01-01'),
            by = 1)

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
                  dates,
                  "/nbs.D110000renmrb_0",
                  paper_length, 
                  ".htm", sep = "")) %>%
    html_nodes( "#titleList a") %>%
    html_attr("href") %>%
    map(~str_c(dates, .x, sep = "/"))
}
all_nodes <- c(".lai "," h2"," h1", "#ozoom p", ".list_l .l_t")
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






#DO NOT EXECUTE WILL BE DELETED LATER

shell("git.lnk config --global user.email \"nils.paffen@wopic.de\"")
shell("git.lnk config --global user.name \"npaffen\"")
soda <- seq(from = length(dates)-29, to = length(dates), by =29)
j = 10
for (i in seq_along(soda)){
  j = j + 1
  
  assign(str_c("database","_m", j, sep = ), map(dates[(length(dates)-i-29):(length(dates)-i)],  ~ f_scraper(.x)))
  
}
list.save("database","_m", j,".rds", sep = )
  remDr$screenshot




shell('git add -a')
shell(str_c('git commit -m', "", sep = " "))
