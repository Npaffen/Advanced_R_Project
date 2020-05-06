### Most of our convenience functions alphabetically ordered

## CONTENT
# check_if_complete(), compares two lists, to see if anything is missing
# count_art_day(), count and plot articles per day, print outliers
# delete_numbers(), takes a char vector and deletes all numbers
# extract_dictionary(), extract dictionary of unique words from article words
# insert_spaces(), into text from a .rds database
# load_rds(), loads files based on their filename and folder
# remove_duplicates(), from a tibble
# request_translation(), contacts the Yandex API to translate the dictionary
# translate_articles(), uses the dictionary to translate the articles
# words_cn_to_en(), translate a title, subtitle, or content chunk

## FUNCTIONS FOR APP
# check_files(), creates a table of descriptives from the data files
# render_frequency(), creates an article frequency plot to be rendered in server.R
# render_word(), creates a word frequency plot to be rendered
# update_in_app(), runs update on the requested year and page data, capturing and printing messages


### required packages
require("stringr") #install.packages("stringr")
# require("DT")
require("dplyr") #install.packages("dplyr")
require("Rwordseg") # devtools::install_github("lijian13/Rwordseg")
if(0){ # if using coreNLP
  require("coreNLP") # install.packages("coreNLP")
  require(rJava) # install.packages("rJava")
  coreNLP::downloadCoreNLP()
}
require("jiebaR")
require("purrr") # install.packages("purrr")
require("tibble") # install.packages("tibble")
require("RYandexTranslate") #devtools::install_github("mukul13/RYandexTranslate")
require("shiny")
source(str_c(here::here(), "R/scraping/ts_word_frequency.R", sep = "/"))
#source("scraping/ts_word_frequency.R")

### check_if_complete(), compares two lists A and B, to see if anything is missing from B
# not used at the moment
# check_if_complete <- function(A, B){
#   if(length(setdiff(A, B)) == 0){
#     set_id <- TRUE
#   } else {set_id <- FALSE}
#   if(length(B)<length(A)){
#     length_id <- FALSE
#   } else {length_id <- TRUE}
#   return(list("set_id" = set_id,
#               "length_id" = length_id))
# }


### count and plot articles per day, print outliers
count_art_day <- function(articles, plot = TRUE,
                          add2ggplot = # optional add to plot
                            "ggtitle('Articles per day on page x')",
                          min_art_outliers = 10){
  art_per_day <- articles %>% group_by(date) %>% count()
  out <- eval(parse(text= paste0(
                      "ggplot(art_per_day,aes(date, n)) +",
                      "geom_point() +",
                      "geom_smooth() +",
                      add2ggplot)))
  outliers <- min_art_outliers
  #outliers <- art_per_day[art_per_day$n > min_art_outliers,]
  #print(outliers)
  #View(filter(articles, date %in% outliers$date))
  return(list("out" = out,
              "outliers" = outliers))
}


### delete_numbers(), takes a char vector and deletes all numbers
delete_numbers <- function(strings){
  return(str_subset(strings, "^[^[:digit:]]+$"))
}

### extract dictionary of unique words from article words
extract_dictionary <- function(vec_articles){
  message(paste("Extracting unique words from Chinese text for dictionary..."))
  dict_CN <- vec_articles[,c("title","subtitle","content")] %>%
    unlist %>% unique
  return(dict_CN[dict_CN != ""]) # remove empty strings
}

### takes the .rds database and inserts spaces into text
insert_spaces <- function(articles,
                          analyzer = "jiebaR",
                          # different options for "analyzer" : "default",
                          # "hmm", "jiebaR", "fmm","coreNLP"
                          nature = TRUE, # recognizes word nature
                          nosymbol = TRUE, # eliminates symbols
                          returnType = "tm" # default is insert spaces
                          ) {
  message(paste("Inserting spaces into Chinese text..."))
  articles[,2:dim(articles)[2]] <- modify_if(
    articles[,2:dim(articles)[2]],
    is.character,
    # check if is nonzero character
    ~ segmentCN(.,
      analyzer = analyzer,
      nature = nature,
      nosymbol = nosymbol,
      returnType = returnType
    )
  )
  return(articles)
}


### load_rds(), loads files based on their filename and folder
load_rds <- function(filenames, folder, wdir){
  for(i in filenames){
    assign(
      substr(i, 1, stop = str_length(i)-4),
      readRDS(paste0(wdir,"/", folder, "/",i)),
      envir = .GlobalEnv
    )
  }
}



### remove_duplicates(), from the articles tibble
remove_duplicates <- function(articles){
  return(articles[!duplicated(articles),])
}


### contacts the Yandex API to translate the dictionary
request_translation <- function(dict_CN,
                                api_key,
                                start = 1 # replace 1 here with last step "i" if time-out
                                ){
  message(paste("Requesting translation from Yandex translation API, please wait..."))
  dict_EN <- character(length = length(dict_CN)) # create empty English dictionary
  for(i in start:length(dict_CN)){
    # translate each single entry, to avoid "contamination" from using many at once
    timeout <- 0
    repeat{
      timeout <- timeout + 1
      try(dict_EN[i] <- translate(api_key, text=dict_CN[i], lang="zh-en")$text)
      if(length(dict_EN[i]) == 0){
        message(paste0("request failed at i = ", i,
                   ", retry request_translation() with \"start = ", i, "-1\""),
              stderr())
      } else{ break }
      if(timeout == 10){
        return(paste0("request failed at i = ", i,
                    " after 10 tries."),
               stderr())
        }
    }
    if(i %% 10 == 0){
      message(paste("translated ", i, " out of ", length(dict_CN), "\n"))
      }
  }
  return(data.frame(chinese = dict_CN, english = dict_EN, stringsAsFactors = FALSE))
}




### uses the dictionary to translate the articles
translate_articles <- function(vec_articles, dict_CN_EN){
  message(paste("Beginning translation of articles, please wait..."))
  dict_CN_EN <- readRDS(here::here("output/dictionary.rds"))
  vec_articles_EN <- vec_articles # translation target
  str_not_zero <- function(x){nchar(x) !=0} # nonempty character condition
  for(i in c("title","subtitle","content")){
    for(j in 1:length(vec_articles[[i]])){
      vec_articles_EN[[i]][[j]] <- modify_if(vec_articles[[i]][[j]], str_not_zero,
                                          ~ words_cn_to_en(.x, dict_CN_EN))
      if(j %% 10 == 0){
        message(paste("translated ", i, j, " out of ", length(vec_articles[[i]]), "\n"))
        }
    }
    # collapse the vectors back to text chunks
    vec_articles_EN[[i]] <- modify(vec_articles_EN[[i]], ~ str_c(., collapse = " "))
  }
  # unnest tibble list Matryoshkas
  vec_articles_EN <- vec_articles_EN %>% tidyr::unnest(subtitle) %>% tidyr::unnest(content)
  return(vec_articles_EN)
}



### translate a title, subtitle, or content chunk
words_cn_to_en <- function(words_cn, dict_CN_EN){
  words_en <- modify(words_cn, ~ {
    if(is.character(.x)){
      word_en <- dict_CN_EN[dict_CN_EN[[1]] == .x,2] # find translation
      if(length(word_en) != 0) {
        str_replace(.x, pattern = .x, replacement = word_en)
      } else {.x}
    }})
  if(words_en != "") return(words_en) else return(words_cn)
}




#######################################################################

## FUNCTIONS FOR APP

## check_files(), creates a table of descriptives from the data files
check_files <- function(loaded_files, database_status){
  for( i in names(loaded_files) ){
    temp <- database_status[0,]
    temp[1:length(loaded_files[[i]]), "type"] <- i
    temp[1:length(loaded_files[[i]]), "file"] <- loaded_files[[i]]
    for( j in 1:length(loaded_files[[i]]) ){
      file_name <- loaded_files[[i]][[j]]
      file_name <- str_sub(file_name, 1, length(file_name)-6)
      temp[j, "dimensions"] <- dim(eval(as.name((file_name)))) %>% str_c(collapse = " ")
      temp[j, "size in kb"] <- as.numeric(object.size(eval(as.name((file_name))))/10e3)
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
  return(database_status)
}

## render_frequency(), creates an article frequency plot to be rendered
render_frequency <- function(file, file_name){
  count_art_day(file, plot = TRUE, min_art_outliers = 100)$out +
    ylab("articles per day") +
    ggtitle(file_name)
}

## render_word(), creates a word frequency plot to be rendered
render_word <- function(word, start_date = "2019-01-01"){
  message(paste("Rendering a stunning word frequency plot just for you..."))
  wordd <- word
  ts_word_frequency(page_num = 1, start_date = as.Date(start_date), end_date = today()-1,
                    eng_word = wordd,
                    econ_data = "NASDAQ_CNY")
}

# update_in_app(), runs update on the requested year and page data, capturing and printing messages
update_in_app <- function(request_year_page){
  year <- substr(request_year_page, 1, 4)
  page <- substr(request_year_page, 6, 7)
  request <- paste0("article_data_", year,
                    "_page_", page)
  # check existance of raw files and if needs update
  if(exists(request)){
    request <- eval(as.name(request))
    if((year == 2019 && max(request$date) == as.Date("2019-12-31")) |
       (Sys.Date() - max(request$date) == 0)){
      message(paste("Raw data is up to date! Exiting update."))
    } else if((year == 2019 && max(request$date) - as.Date("2019-12-31") > 7) |
              (year == 2020 && Sys.Date() - max(request$date) > 7)){
      message(paste("Raw data is older than 1 week! Perform long update?"))
      showModal(confirm_update())
    } else{
      message(paste("Raw data is recent, will update."))
      updating_text_data_app(
        target = input$request_year_page,
        api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
        RUN_API = TRUE,
        RUN_TRANSLATION = TRUE,
        RUN_UPDATE = TRUE
      )
    }
  } else {
    message(paste("Raw data is missing, please redownload \'article_data_\' ",
                  "file into /data folder or reinstall app"))
  }
  # check existance of dictionary and if needs creation
  if(!exists("dictionary")){
    message(paste("Missing /output/dictionary.rds file!",
                  "Creating from scratch in 5 seconds..."))
    Sys.sleep(5)
    updating_text_data_app(
      target = input$request_year_page,
      api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
      RUN_API = TRUE,
      RUN_TRANSLATION = TRUE,
      RUN_UPDATE = TRUE
    )
  } else {
    message(paste("Dictionary file found."))
  }
  # check if processed files exists and need update
  request <- paste0("processed_articles_", year,
                    "_page_", page)
  if(!exists(paste0(request, "_CN")) |
     !exists(paste0(request, "_EN"))){
    message(paste("Processed file(s) missing! Creating from scratch in 5 seconds..."))
    Sys.sleep(5)
    updating_text_data_app(
      target = input$request_year_page,
      api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
      RUN_API = TRUE,
      RUN_TRANSLATION = TRUE,
      RUN_UPDATE = TRUE
    )
  } else {
    message(paste0("Processed files found."))
  }
}

# react to long update confirmation
observeEvent(input$button_long_update, {
  removeModal()
  withCallingHandlers({
    shinyjs::html("html", "")
    updating_text_data_app(
      target = input$request_year_page,
      api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
      RUN_API = TRUE,
      RUN_TRANSLATION = TRUE,
      RUN_UPDATE = TRUE
    )
  },
  message = function(m) {
    shinyjs::html(id = "update_report", html = m$message, add = TRUE)
    shinyjs::html(id = "update_report", html = "<br/>", add = TRUE)
  })
})
