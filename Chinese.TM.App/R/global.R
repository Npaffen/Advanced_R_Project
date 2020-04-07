### This will load the data and perform global preparation for the app.

## Structure
# 0. Preparation
# 1. Load and check files
# 2. Check database status
# 3. Word Frequencies



#####################################################################
# 0. Preparation
wdir <- here::here()
source("src/app/functions.R")
require(stringr)
require(tibble)
require(dplyr)
source("src/ts_word_frequency.R")



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
for( i in names(loaded_files) ){
  temp <- database_status[0,]
  temp[1:length(loaded_files[[i]]), "type"] <- i
  temp[1:length(loaded_files[[i]]), "file"] <- loaded_files[[i]]
  for( j in 1:length(loaded_files[[i]]) ){
    file_name <- loaded_files[[i]][[j]]
    file_name <- str_sub(file_name, 1, length(file_name)-6)
    temp[j, "dimensions"] <- dim(eval(as.name((file_name)))) %>% str_c(collapse = " ")
    temp[j, "size in kb"] <- object.size(eval(as.name((file_name))))/10e3
    if(i != "dictionary"){
      temp[j, "articles"] <- length(unique(get(file_name)$content))
      temp[j, "dates"] <- length(unique(get(file_name)$date))
      temp[j, "start"] <- as.character(min(get(file_name)$date))
      temp[j, "end"] <- as.character(max(get(file_name)$date))
    }
  }
  database_status <- rbind(temp, database_status)
}

database_status <- database_status[with(database_status, order(file)), ]



#####################################################################
# 3. Word Frequencies

freq_words <- c( "development", "inspection",
                 "outbreak"
                # "comrade", "committee"
                # "corruption", "crown",
                # , "supervision"
)

word_freq1 <- render_word(freq_words[[1]])
word_freq2 <- render_word(freq_words[[2]])
word_freq3 <- render_word(freq_words[[3]])
