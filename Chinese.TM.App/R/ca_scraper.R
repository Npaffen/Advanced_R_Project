
ca_scraper <- function(username, password, years, month=1:12, paper_length=1:2, personal_2captcha_key){
  source(str_c(here::here(), "ca_login.R", sep = "/"))
  source(str_c(here::here(), "ca_articles_url.R", sep = "/"))
  source(str_c(here::here(), "ca_content.R", sep = "/"))
  source(str_c(here::here(), "ca_date_urls.R", sep = "/"))


  ca_login(username = username,
           password = password)

  date_urls <- (map(paper_length,
                    ~ca_date_urls(years=years,
                                  month=month,
                                  paper_length = .x))) %>%
    unlist() %>%
    sort()

  url_articles <- map(date_urls,
                      ~ ca_articles_url(.x))

  map_df(url_articles,
         ~ca_content(.x))
  Sys.sleep(runif(1, 300,330))
}

