ca_date_urls <- function(years, month, paper_length){
source(str_c(here::here(), "scraping", "make_dates.R", sep = "/"))

  dates <-  map(years,
            ~make_dates(year = .x,
                        month = month,
                        all_dates = T))
date_url <-map(dates,
               ~  as.Date(
                 unlist(
                   unname(.x)),
                 origin) %>%
                 gsub(
                   pattern = "-",
                   replacement = "",
                   x = .
                 )
               )

map(date_url,~str_c(
  "http://data.people.com.cn.s894ibwr0870.erf.sbb.spk-berlin.de/rmrb",
  .x,
  paper_length,
  sep = "/")
  )

}
