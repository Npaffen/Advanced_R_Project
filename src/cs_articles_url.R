cs_articles_url <- function(date_url, paper_length) {
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