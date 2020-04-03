# some simple descriptives for the raw article data

require(dplyr)
require(ggplot2)

# read data
wdir <- here::here()
articles201 <- readRDS(paste0(wdir, "/data/article_data_2020_page_01.rds"))
articles202 <- readRDS(paste0(wdir, "/data/article_data_2020_page_02.rds"))
articles191 <- readRDS(paste0(wdir, "/data/article_data_2019_page_01.rds"))
articles192 <- readRDS(paste0(wdir, "/data/article_data_2019_page_02.rds"))

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

# count and plot articles per day
count_art_day <- function(articles, plot = TRUE){
  art_per_day <- articles %>% group_by(date) %>% count()
  ggplot(art_per_day,aes(date, n)) +
    geom_point() +
    geom_smooth()
}

count_art_day(articles201) ## cool: first and second pages are quite different
count_art_day(articles191) ## check what's up with outliers with n>10 in the first pages
count_art_day(articles192)
count_art_day(articles202)




