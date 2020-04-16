### This will load the data and perform global preparation for the app.

## Structure
# 0. Preparation
# 1. Load and check files
# 2. Check database status
# 3. Word Frequencies


message(paste("Welcome to the People's Daily Mining App. Please stand by for a few seconds while it loads..."))

#####################################################################
# 0. Preparation
wdir <- here::here()
#source("app/functions.R")
require(stringr)
# require("DT")
require(tibble)
require(dplyr)
source(str_c(here::here(), "R/scraping/ts_word_frequency.R", sep = "/"))
#source("scraping/ts_word_frequency.R")
source(str_c(here::here(), "R/wrangling/update_article_data.R", sep = "/"))
#source("wrangling/update_article_data.R")
source(str_c(here::here(), "R/app/functions.R", sep = "/"))



#####################################################################
# 1. Load and check files

# check folders for files
files_data <- list.files(paste0(wdir, "/data"))
files_output <- list.files(paste0(wdir, "/output"))

raw_articles <- files_data[grep("article_data", files_data)]
processed_articles <- files_output[grep("processed_articles", files_output)]
dictionary <- files_output[grep("dictionary.rds", files_output)]


# create list of all loaded files
loaded_files <- list()
if(length(raw_articles) != 0){loaded_files$"raw_articles" = raw_articles}
if(length(processed_articles) != 0){loaded_files$"processed_articles" = processed_articles}
if(length(dictionary) != 0){loaded_files$"dictionary" = dictionary}


# load in .rds files
load_rds(filenames = c(dictionary, processed_articles),
         wdir = wdir,
         folder = "output")
load_rds(filenames = raw_articles,
         wdir = wdir,
         folder = "data")



#####################################################################
# 2. Check database status

# output database status table
database_status <- tibble("type" = character(),
                              "file" = character(),
                              "dimensions" = character(),
                              "size in kb" = numeric(),
                              "articles" = numeric(),
                              "dates" = numeric(),
                              "start" = character(),
                              "end" = character()
                              )

# check individual files
database_status <- check_files(loaded_files)




#####################################################################
# 3. Word Frequencies


freq_words <<- c( "outbreak", "development",
                 "inspection"

                # "comrade", "committee"
                # "corruption", "crown",
                # , "supervision"
)

blank_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank()

word_freq1 <<- render_word(freq_words[[1]])
word_freq2 <- render_word(freq_words[[2]])
word_freq3 <- render_word(freq_words[[3]])
word_freq4 <- blank_plot

