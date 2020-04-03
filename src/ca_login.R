ca_login <- function(username, password){
  library(RSelenium)
  remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
  
  remDr$open()
  remDr$navigate("https://tinyurl.com/rq8vom4")
  
  remDr$findElement("css selector", "#login :nth-child(1)")$sendKeysToElement(list(as.character(username)))
  Sys.sleep(3L)
  remDr$findElement("css selector", "#login :nth-child(2)")$sendKeysToElement(list(as.character(password)))
  Sys.sleep(3L)
  remDr$findElement("css selector", "#login_button")$sendKeysToElement(list(key = "enter"))
}