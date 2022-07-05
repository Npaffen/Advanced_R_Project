# some simple descriptives for the raw article data

require(dplyr)
require(ggplot2)
require(jiebaR)
source(str_c(here::here(),"/app/functions.R"))

# read data
wdir <- here::here()
articles201 <- readRDS(paste0(wdir, "/data/article_data_2020_page_01.rds"))
articles202 <- readRDS(paste0(wdir, "/data/article_data_2020_page_02.rds"))
articles191 <- readRDS(paste0(wdir, "/data/article_data_2019_page_01.rds"))
articles192 <- readRDS(paste0(wdir, "/data/article_data_2019_page_02.rds"))
en_articles201 <- readRDS(paste0(wdir, "/output/processed_articles_2020_page_01_EN.rds"))
en_articles202 <- readRDS(paste0(wdir, "/output/processed_articles_2020_page_02_EN.rds"))
en_articles191 <- readRDS(paste0(wdir, "/output/processed_articles_2019_page_01_EN.rds"))
en_articles192 <- readRDS(paste0(wdir, "/output/processed_articles_2019_page_02_EN.rds"))

# count unique dates to check completeness
length(unique(articles191$date)) # should be 365
length(unique(articles192$date)) # should be 356
length(unique(articles201$date)) # should be around 94
length(unique(articles202$date)) # should be around 94

# check for duplicates
dim(unique(articles191))[1] - dim(articles191)[1] ## should be zero
dim(unique(articles192))[1] - dim(articles192)[1]
dim(unique(articles201))[1] - dim(articles201)[1] # is -7
dim(unique(articles202))[1] - dim(articles202)[1]

# count and plot articles per day, print outliers


count_art_day(articles201, min_art_outliers = 9,
              add2ggplot = paste0(
                "ggtitle('Articles per day on page 1 in 2020') + ",
                "ylab('articles') + ",
                "xlab('dates in 2020')")
)
              ## cool: first and second pages are quite different
count_art_day(articles191, min_art_outliers = 9,
              add2ggplot = paste0(
                "ggtitle('Articles per day on page 1 in 2019') + ",
                "ylab('articles') + ",
                "xlab('dates in 2019')")
)
count_art_day(articles192, min_art_outliers = 8,
              add2ggplot = paste0(
                "ggtitle('Articles per day on page 2 in 2020') + ",
                "ylab('articles') + ",
                "xlab('dates in 2020')")
)
count_art_day(articles202, min_art_outliers = 9,
              add2ggplot = paste0(
                "ggtitle('Articles per day on page 2 in 2019') + \n",
                "ylab('articles') + \n",
                "xlab('dates in 2019')")
)



## text analysis

keywordslist <- function(articles, topn){
  all_content <- paste(articles$content[1:100], collapse = " ")
  words = all_content
  keys = worker("keywords", topn = topn)
  keys <= words
}

keywordslist(articles191, 100)
keywordslist(en_articles191, 1000)
