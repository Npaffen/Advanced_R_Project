library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(stringr)
Sys.setlocale(locale = "Chinese") # fix character encoding
fail <- read_html(str_c("http://www.iciba.com", 
                        dictionary[1,1], sep = "/")) %>%
 html_nodes( ".clearfix p span")
dictionary_chin <- dictionary[1]
page <- function(dictionary){
  Sys.sleep(runif(n = 1, min = 3 , max = 5))                      
 read_html(str_c("http://www.iciba.com", 
                                       dictionary, sep = "/"))))}
    
  
                         if (html_nodes( ".clearfix p span") == fail) html_nodes(page, ".in-base-top div:nth-child(2)")
                             else html_nodes(page, ".clearfix p span") )
                         
                         
 dictionary_test <- map(dictionary_chin[100], ~page(.x))
 