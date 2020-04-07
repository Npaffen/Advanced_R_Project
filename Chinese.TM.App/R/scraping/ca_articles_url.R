ca_dates_url <- function(date_urls) {
  Sys.sleep(runif(1, 5, 10)) #a short break between the scraping process of the articles url list                              

remDr$navigate(date_urls)

  read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("li h3 a") %>%
    html_attr("href") %>%
    gsub("(/[a-z]+/\\d+/\\d+/)",
         replacement = "",
         x = .
    ) %>%
    grep("([a-z0-9]{32})", value = T, x = .) %>%
    gsub("^",
         replacement = date_urls,
         x = .
    )
}