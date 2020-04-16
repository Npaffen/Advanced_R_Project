
ca_scraper <- function(username ="dschulze", password = "bonsaibonsai", years=2019:2020, month=1:12, paper_length=1:2, personal_2captcha_key ="d3ce30748e45dc73365f4e327acaebee"){
  source(str_c(here::here(), "R", "scraping",  "ca_login.R", sep = "/"))
  source(str_c(here::here(), "R", "scraping", "ca_articles_url.R", sep = "/"))
  source(str_c(here::here(), "R", "scraping", "ca_content.R", sep = "/"))
  source(str_c(here::here(), "R", "scraping", "ca_date_urls.R", sep = "/"))

  remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)

  remDr$open()

  ca_login(username = username, password = password )

  date_urls <- map(paper_length ,
                    ~ca_date_urls(years=years,
                                  month=month,
                                  paper_length = .x)) %>%
    unlist() %>%
    sort()

  url_articles <- map(date_urls,
                      ~ ca_articles_url(.x)) %>%
    unlist()

  map_df(url_articles,
         ~ca_content(.x))
  Sys.sleep(runif(1, 300,330))
}

