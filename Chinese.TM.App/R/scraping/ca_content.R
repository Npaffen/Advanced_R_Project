ca_captcha <- function(personal_2captcha_key = "d3ce30748e45dc73365f4e327acaebee") {

  remDr$screenshot(file = str_c(here::here(),
                                "captcha.png",
                                sep = "/")) # screenshot of the full site with captcha
  magick::image_crop(
    image_read(
      str_c(here::here(),
            "captcha.png",
            sep = "/")),
    geometry_area(97, 38.5, 480, 290)) %>%
    image_write(
      str_c(here::here(),
            "captcha.png",
            sep = "/"),
      format = "png") # crop the captcha

  cap_POST <- httr::POST(
    url = as.character(personal_2captcha_key),
    encode = "multipart",
    body = list(file = upload_file(path = str_c(here::here(),
                                                "captcha.png",
                                                sep = "/")))) # send the captcha to the api

  captcha_ID <- content(cap_POST) %>%
    xml_child() %>%
    xml_text() %>%
    gsub("[^0-9]+",
         replacement = "",
         x = .) # catch the ticket ID

  Sys.sleep(10L) # wait untill solving

  while (httr::GET(url = str_c("https://2captcha.com/res.php?key=",personal_2captcha_key,"&action=get&id=",
                               captcha_ID,
                               sep = "")) %>%
         content() == "CAPCHA_NT_READY")# check if the captcha key is not ready yet
  {Sys.sleep(5L)} # if so add extra time and to solve and check again

  captcha_key <- httr::GET(url = str_c("https://2captcha.com/res.php?key=",personal_2captcha_key,"&action=get&id=",
                                       captcha_ID,
                                       sep = "")) %>%
    content() %>%
    gsub(
      x = .,
      pattern = "[OK|]",
      replacement = ""
    ) # grab the captcha code

  remDr$findElement("css selector",
                    "#validateCode")$sendKeysToElement(list(captcha_key,
                                                            key = "enter")) # post the captcha code to crossasia
}
