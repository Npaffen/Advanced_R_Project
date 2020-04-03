# Scraping begins here
source("src/scrape_article.R")

pages <- c(page_01 = "01", page_02 = "02")
years <- c(year_2019 = 2019, year_2020 = 2020)
months_2019 <- 1:12
months_2020 <- 1:4
# all_dates <- TRUE


# Year 2019 -----------------------------------------------------

article_data_2019 <- map(
  pages,
  ~ scrape_article(
    page_num  = .x,
    year = years[[1]],
    month = months_2019,
    all_dates = TRUE
  )
)

# Year 2020 -----------------------------------------------------

article_data_2020 <- map(
  pages, ~ scrape_article(
    page_num = .x,
    , # dates = not supplied
    year = years[[2]],
    month = months_2020[[4]],
    all_dates = TRUE
  )
)

# saving ---------------------------------------------

walk2(
  article_data_2019, names(article_data_2019),
  ~ saveRDS(
    .x,
    paste0("data/article_data_2019_", .y, ".rds")
  )
)

walk2(
  article_data_2020, names(article_data_2020),
  ~ saveRDS(
    .x,
    paste0("data/article_data_2020_", .y, ".rds")
  )
)
