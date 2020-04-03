# Scraping begins here

pages <- c(page_01 = "01", page_02 = "02")
years <- c(year_2019 = 2019, year_2020 = 2020)
months_2019 <- 1:12
months_2020 <- 1:4
# all_dates <- TRUE


# Year 2019 -----------------------------------------------------

article_data_2019 <- map(
  pages,
  ~ scrape_article(
    page = .x,
    year = years[[1]],
    month = months_2019,
    all_dates = TRUE
  )
)

# Year 2020 -----------------------------------------------------

article_data_2020 <- map(
  pages, ~ scrape_article(
    page = .x,
    year = years[[2]],
    month = months_2020,
    all_dates = TRUE
  )
)

# # saving ---------------
#
# saveRDS(article_data_2019$page_01,
#         "data/article_data_2019_page_01.rds" )
#
# saveRDS(article_data_2019$page_02,
#         "data/article_data_2019_page_02.rds" )
#
# saveRDS(article_data_2020$page_01,
#         "data/article_data_2020_page_01.rds" )
# saveRDS(article_data_2020$page_02,
#         "data/article_data_2020_page_02.rds" )


