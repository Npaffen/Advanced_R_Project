
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

dates <- seq(
  from = as.Date("2015-01-01"),
  to = as.Date("2018-12-31"),
  by = 1
)


date_url <- dates %>% gsub(
  pattern = "-",
  replacement = "",
  x = .
)

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
login <- function(){
remDr$open()
remDr$navigate("https://tinyurl.com/rq8vom4")

remDr$findElement("css selector", "#login :nth-child(1)")$sendKeysToElement(list("dschulze"))
Sys.sleep(3L)
remDr$findElement("css selector", "#login :nth-child(2)")$sendKeysToElement(list("bonsaibonsai"))
Sys.sleep(3L)
remDr$findElement("css selector", "#login_button")$sendKeysToElement(list(key = "enter"))
}
f_articles_url <- function(paper_length, date_url) {
  Sys.sleep(runif(1, 5, 10))

  remDr$navigate(
    str_c(
      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
      date_url,
      paper_length,
      sep = "/"
    )
  )



  read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("li h3 a") %>%
    html_attr("href") %>%
    gsub("(/[a-z]+/\\d+/\\d+/)",
      replacement = "",
      x = .
    ) %>%
    grep("([a-z0-9]{32})", value = T, x = .) %>%
    gsub("^",
      replacement = str_c(date_url, paper_length, "", sep = "/"),
      x = .
    )
}

f_captcha <- function() {
  remDr$screenshot(file = "captcha.png") # screenshot of the full site with captcha
  magick::image_crop(image_read("captcha.png"), geometry_area(97, 38.5, 480, 290)) %>%
    image_write("captcha.png", format = "png") # crop the captcha




  cap_POST <- httr::POST(
    url = "http://2captcha.com/in.php?key=d3ce30748e45dc73365f4e327acaebee&method=post",
    encode = "multipart",
    body = list(file = upload_file(path = "captcha.png"))
  ) # send the captcha to the api
  captcha_ID <- content(cap_POST) %>%
    xml_child() %>%
    xml_text() %>%
    gsub("[^0-9]+",
      replacement = "",
      x = .
    ) # catch the ticket ID

  Sys.sleep(10L) # wait untill solving

  if (httr::GET(url = str_c("https://2captcha.com/res.php?key=d3ce30748e45dc73365f4e327acaebee&action=get&id=",
    captcha_ID,
    sep = ""
  )) %>%
    content() == "CAPCHA_NT_READY") { # check if the captcha is solved
    Sys.sleep(5L)
  } # add extra time if not


  captcha_key <- httr::GET(url = str_c("https://2captcha.com/res.php?key=d3ce30748e45dc73365f4e327acaebee&action=get&id=",
    captcha_ID,
    sep = ""
  )) %>%
    content() %>%
    gsub(
      x = .,
      pattern = "[OK|]",
      replacement = ""
    ) # grab the captcha code

  remDr$findElement("css selector", "#validateCode")$sendKeysToElement(list(captcha_key, key = "enter")) # post it to the form


  
}

f_content <- function(url_articles) {
  
   remDr$navigate(
    str_c(
      "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
      url_articles,
      sep = "/"
    )
  )

  if (read_html(remDr$getPageSource()[[1]]) %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() == timeout %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text()) message(str_c("Timeout after",as.numeric(Sys.time()-start_time, units = "mins") ), sep = " ")
      
      if (read_html(remDr$getPageSource()[[1]]) %>%
      xml_child(2) %>%
      xml_child(2) %>%
    html_text() %>%
    gsub(x = .,
         pattern ="(\\n)+|(\\t)+|\\s|(\\r)+",
         replacement = "") ==  captcha_tester %>%
    xml_child(2) %>%
    xml_child(2) %>%
    html_text() %>%
    gsub(x = .,
         pattern ="(\\n)+|\\r|\\s",
         replacement = "")) { # check if the archive requests a captcha
    f_captcha()
  } # solve the captcha if necessary




  if (read_html(remDr$getPageSource()[[1]]) %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() %>%
      gsub(x = .,
           pattern ="(\\n)+|(\\t)+|\\s|(\\r)+",
           replacement = "") ==  captcha_tester %>%
      xml_child(2) %>%
      xml_child(2) %>%
      html_text() %>%
      gsub(x = .,
           pattern ="(\\n)+|\\r|\\s",
           replacement = "")) { # check if the captcha was solved correctly
    f_captcha()
  } # solve again if not true


    page <- read_html(remDr$getPageSource()[[1]])
    
    get_title <- compose(html_text,   
                      partial(html_nodes, css = ".title"),
                      partial(html_nodes, css = "#detail_pop_content"),
                      .dir = "backward"
    )
    
    get_subtitle <- compose(html_text,
                            partial(html_nodes, css = ".subtitle"),
                            partial(html_nodes, css = "#detail_pop_content"),
                            .dir = "backward"
    )
    
    get_author <- compose(html_text,
                            partial(html_nodes, css = ".author"),
                            partial(html_nodes, css = "#detail_pop_content"),
                            .dir = "backward"
    )
    
    get_paragraph <- compose(html_text,
                             partial(html_nodes, css = "p"),
                             partial(html_nodes, css = "#detail_pop_content"),
                             .dir = "backward"
    )
    
    get_date <- compose(html_text,
                             partial(html_nodes, css = ".sha_left span:nth-child(1)"),
                             partial(html_nodes, css = "#detail_pop_content"),
                             .dir = "backward"
    )
    
    get_page_num <- compose(html_text,
                        partial(html_nodes, css = ".sha_left span:nth-child(2)"),
                        partial(html_nodes, css = "#detail_pop_content"),
                        .dir = "backward"
    )
    
    df_l <- list(
      title = get_title(page),
      subtitle = get_subtitle(page),
      author = get_author(page),
      content = get_paragraph(page),
      date = get_date(page),
      PageNumber = get_page_num(page) 
    ) %>% 
      map(~if(length(.x) == 0){.x = NA} else .x = .x)
    

  
    df <- tibble(
      title = df_l$title,
      subtitle = df_l$subtitle,
      date = df_l$date,
      PageNumber = df_l$PageNumber,
      content = paste0(df_l$content, collapse = ""),
      num_paragraph = length(df_l$content)
    ) # grab the article content
      
      df
}


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

