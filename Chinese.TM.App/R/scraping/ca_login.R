ca_login <- function(username = "dschulze", password = "bonsaibonsai"){


  remDr$navigate("https://tinyurl.com/rq8vom4")

  remDr$findElement("css selector", "#login :nth-child(1)")$sendKeysToElement(list(as.character(username)))
  Sys.sleep(3L)
  remDr$findElement("css selector", "#login :nth-child(2)")$sendKeysToElement(list(as.character(password)))
  Sys.sleep(3L)
  remDr$findElement("css selector", "#login_button")$sendKeysToElement(list(key = "enter"))
}
