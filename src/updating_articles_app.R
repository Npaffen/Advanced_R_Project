### Updating Translation (for use in the app, requires existing translations)
## only checks if something is missing in 2020!
## Tasks
# 0. Preparation
# 1. Identify new articles
# 2. Insert spaces with NLP tool
# 3. Find new words not in dictionary
# 4. Translate new words with API, articles with dictionary
# 5. Update databases 


updating_articles <- function(TESTING = FALSE,
                              # activate if testing
                              RUN_API = TRUE,
                              # might take very long only if
                              # you are really sure that yandex
                              # API is available
                              RUN_TRANSLATION = TRUE
                              # might take very long
                              ){
  
  RUN_UPDATE <- FALSE # will self-activate if updating
  
  
  #####################################################################
  # 0. Preparation
  
  # check if relevant files are present
  
  # check article data
  wdir <- here::here()
  files <- list.files(paste0(wdir,"/data"))
  if(length(grep("article_data_",files)) == 0){
    ans <- readline(prompt="Article data missing in folder '/data',
                  type 'run' to recompile, press [Enter] to abort.")
    if(ans == "run"){message("need to download articles")}
  } else {
    message("article_data files found in '/data' \n",
            files[grep("article_data_",files)], sep = "\n")
  }
  
  # check processed articles
  processed_files <- list.files(paste0(wdir,"/output/"))
  processed_files <- processed_files[  grep("processed_article", processed_files)]
  if(length(processed_files) == 0){
    ans <- readline(prompt="Processed article data missing in folder '/output',
                  enter 'run' to reprocess,
                  press [Enter] to abort. ")
    if(ans == "run"){
      message("processing articles")
      source("src/process_articles.R")
    }
  } else {
    message(cat("processed files found in '/output'. \n", processed_files, sep = "\n"))
  }
  
  # dictionary
  if(!file.exists(paste0(wdir,"/output/dictionary.rds"))){
    ans <- readline(prompt="Dictionary data missing in folder '/output',
                  type 'run' to recompile, press [Enter] to abort. ")
    if(ans == "run"){
      message("processing articles")
      source("src/create_dictionary.R")
    }
  } else {
    message(cat("dictionary data found in '/output'. \n",
                "entries: ", dim(readRDS("output/dictionary.rds"))[1])
    )
  }
  
  
  # load data
  new_articles_1 <- readRDS(
    paste0(wdir,"/data/article_data_2020_page_01.rds")) # new articles p1
  new_articles_2 <- readRDS(
    paste0(wdir, "/data/article_data_2020_page_02.rds")) # new articles p2
  articles_en_1 <- readRDS(
    paste0(wdir,"/output/processed_articles_2020_page_01_EN.rds")) # old articles p1
  articles_en_2 <- readRDS(
    paste0(wdir,"/output/processed_articles_2020_page_02_EN.rds")) # old articles p2
  
  
  if(TESTING){ # remove some articles for testing
    articles_en_1 <- articles_en_1[1:(length(articles_en_1$id)-20),]
    articles_en_2 <- articles_en_2[1:(length(articles_en_2$id)-20),]
  }
  
  
  # check if update required in 2020
  
  missing_1 <- suppressWarnings(order(new_articles_1$id) != order(articles_en_1$id))
  missing_2 <- suppressWarnings(order(new_articles_2$id) != order(articles_en_2$id))
  
  
  if(sum(c(missing_1, missing_2)) == 0){
    RUN_UPDATE <- FALSE
    stop("No new articles to update, aborting...")
  } else{
    RUN_UPDATE <- TRUE
    message(cat("Missing articles: ",
                sum(c(missing_1, missing_2)),
                "\n Running update...")
    )
  }
  
  
  
  
  if(RUN_UPDATE) {
    
    # source self-written functions
    source("src/functions.R")
    source("src/update_article_data.R")
    
    # load/install required packages
    require("dplyr") # install.packages("dplyr")
    require("purrr") # install.packages("purrr")
    require("tibble") # install.packages("tibble")
    
    # install/load NLP packages for separating Chinese words
    # "Rwordseg" by Jian Li (2019) (https://github.com/lijian13/Rwordseg)
    # "coreNLP" by Arnold Taylor and Lauren Tilton (2016)
    require("Rwordseg") # devtools::install_github("lijian13/Rwordseg")
    if(0){ # if using coreNLP
      require("coreNLP") # install.packages("coreNLP") 
      require(rJava) # install.packages("rJava")
      coreNLP::downloadCoreNLP()
    }
    require("jiebaR") # install.packages("jiebaR")
    require("HMM") # install.packages("HMM")
    
    # install/load packages for translation API
    require("RYandexTranslate") #devtools::install_github("mukul13/RYandexTranslate")
    
    # install/load packages for translating with the dictionary
    require("stringr") #install.packages("stringr")
    
    
    #load other data
    if(file.exists("output/processed_articles_2020_page_01_EN.rds")){
      articles_en <- readRDS("output/processed_articles_2020_EN.rds")
    } else {stop("Error: 2020 processed articles missing!")}
    if(file.exists("output/dictionary.rds")){
      dictionary <- readRDS("output/dictionary.rds")
    } else {stop("Error: Dictionary file missing!")}
    
    
    # remove duplicates from articles
    new_articles <- remove_duplicates(new_articles)
    articles_cn <- remove_duplicates(articles_cn)
    articles_en <- remove_duplicates(articles_en)
    
    #####################################################################
    # 1. Identify new articles
    
    # insert spaces to make article lists comparable,
    # because there's no identifier
    no_identifiers_yet <- TRUE
    new_articles <- insert_spaces(
      articles = new_articles,
      analyzer = "jiebaR",
      # different options for "analyzer" : "default",
      # "hmm", "jiebaR", "fmm","coreNLP"
      nature = TRUE, # recognizes word nature
      nosymbol = TRUE,   # eliminates symbols
      returnType = "tm" # default is insert spaces
    )
    
    new <- anti_join(new_articles,
                     articles_cn,
                     by=c("Date", 'title', 'subtitle', 'content'))
    
    
    
    #####################################################################
    # 2. Insert spaces with NLP tool
    if(!no_identifiers_yet){
      sep_articles <- insert_spaces(articles = new,
                                    analyzer = "jiebaR",
                                    # different options for "analyzer" : "default",
                                    # "hmm", "jiebaR", "fmm","coreNLP"
                                    nature = TRUE, # recognizes word nature
                                    nosymbol = TRUE, # eliminates symbols
                                    returnType = "tm" # default is insert spaces
      )
    }
    
    art_words <- insert_spaces(articles = new,
                               analyzer = "jiebaR",
                               # different options for "analyzer" : "default",
                               # "hmm", "jiebaR", "fmm","coreNLP"
                               nature = FALSE, # recognizes word nature
                               nosymbol = TRUE, # eliminates symbols
                               returnType = "vector" # default is insert spaces
    )
    
    # extract dictionary of unique words from new articles
    dict_CN <- art_words %>%
      extract_dictionary() %>%
      delete_numbers()
    
    
    
    #####################################################################
    # 3. Find new words not in dictionary
    dict_CN <- anti_join(enframe(dict_CN),
                         enframe(dictionary$chinese),
                         by = "value") %>% deframe %>% unname
    
    
    #####################################################################
    # 4. Translate new words with API, articles with dictionary
    if(RUN_API){ # use sparingly! character limit: 1,000,000/day, 10,000,000 month
      Sys.setlocale(locale = "Chinese") # fix character encoding
      new_dict_CN_EN <- request_translation(dict_CN,
                                            # free keys can be generated here:
                                            # https://tech.yandex.com/translate/
                                            api_key="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
                                            start = 1 # if this fails at a certain "i", just restart at i-1
      )
      dict_CN_EN <- bind_rows(new_dict_CN_EN, dictionary)
      saveRDS(dict_CN_EN, "output/dictionary.rds")
      write.csv(dict_CN_EN, # csv file for further use
                file = "output/dictionary.csv",
                fileEncoding = "UTF-16LE")
      Sys.setlocale() # restore default locale
    }
    
    
    #####################################################################
    # 5. Update databases 
    if(RUN_TRANSLATION){ # takes around 45 min.
      Sys.setlocale(locale = "Chinese") # fix character encoding
      art_words_EN <- translate_articles(art_words)
      art_words_EN <- bind_rows(articles_en, art_words_EN)
      saveRDS(art_words_EN, "output/processed_articles_2020_EN.rds")
      Sys.setlocale() # restore default locale
    } else
    
  }
  

} 