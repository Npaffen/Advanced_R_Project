### creates a Chinese-English dictionary

## loads raw article data
## creates a dictionary of unique words
## requests translations from Yandex API

## Tasks (repeats for each article file)
# 0. Preparation
# 1. Create dictionary of unique words in articles and check for old version
# 2. Request translation of the dictionary with translation API

create_dictionary <- function(
  RUN_API = FALSE,# set to true only if you are really sure Yandex API is available 
  # and you have a lot of time, a new dictionary might take hours
  OVERWRITE = FALSE, # set to true if you suspect the old dictionary is corrupt
  api_key = "trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a"
  ){
  
  message("Creating dictionary from processed articles, please wait...")
  
  #####################################################################
  # 0. Preparation
  
  # source self-written functions
  source("src/functions.R")
  
  # load/install required packages
  require(purrr) # install.packages("purrr")
  require(dplyr) # install.packages("dplyr")
  require(tibble) # install.packages("tibble")
  # require(devtools) # install.packages("devtools") # for installing packages
  # install/load packages for separating Chinese words
  # "Rwordseg" by Jian Li (2019) (https://github.com/lijian13/Rwordseg)
  # "coreNLP" by Arnold Taylor and Lauren Tilton (2016)
  require("Rwordseg") # devtools::install_github("lijian13/Rwordseg")
  if(0){ # if using coreNLP, HMM
    require("coreNLP") # install.packages("coreNLP") 
    require(rJava) # install.packages("rJava")
    coreNLP::downloadCoreNLP()
    require("HMM") # install.packages("HMM")
  }
  require("jiebaR") # install.packages("jiebaR")
  # install/load packages for translation API
  require("RYandexTranslate") #devtools::install_github("mukul13/RYandexTranslate")
  # install/load packages for translating with the dictionary
  require("stringr") #install.packages("stringr")
  
  #####################################################################
  
  # find list of article files
  wdir <- here::here()
  files <- list.files(paste0(wdir, "/data"))
  files <- files[grep("article_data", files)] 
  
  
  ### repeat for each article file
  
  for(i in files){
    # read article data and delete duplicates
    
    message(cat("#### beginning processing of file: ", i, " ####\n"))
    articles <- readRDS(paste0(wdir, "/data/", i))%>%
      remove_duplicates()
    
    #####################################################################
    ## 1. Create dictionary of all words in articles and check for old version
    
    # make a list that returns the articles separated into word vectors
    vec_articles <- insert_spaces(articles,
                                  analyzer = "jiebaR",
                                  # different options for "analyzer" : "default",
                                  # "hmm", "jiebaR", "fmm","coreNLP"
                                  nature = FALSE, # recognizes word nature
                                  nosymbol = TRUE, # eliminates symbols
                                  returnType = "vector" # default is insert spaces
    )
    
    # extract dictionary of unique words from article words
    dict_CN <- vec_articles %>%
      extract_dictionary() %>%
      delete_numbers()
    
    # try to read any old dictionary.rds and determine new words
    if(file.exists(paste0(wdir,"/output/dictionary.rds")) & !OVERWRITE ){
      message("found existing dictionary.rds, translating and appending new words \n")
      old_dict <- readRDS(paste0(wdir,"/output/dictionary.rds"))
      dict_CN <- anti_join(enframe(dict_CN),
                           enframe(old_dict$chinese),
                           by = "value") %>% deframe %>% unname
    }
    
    
    #####################################################################
    # 2. Request translation of the dictionary with Yandex translation API
    if(RUN_API){ # only run once! character limit: 1,000,000/day, 10,000,000 month
      message("starting dictionary translation with Yandex API \n")
      Sys.setlocale(locale = "Chinese") # fix character encoding
      dict_CN_EN <- request_translation(dict_CN,
                                        # free keys can be generated here:
                                        # https://tech.yandex.com/translate/
                                        api_key= api_key,
                                        start = 1 # if this fails at a certain "i", just restart at i-1
      )
      if(exists("old_dict")){ # append to old dict, if it exists
        dict_CN_EN <- bind_rows(old_dict, dict_CN_EN)
      }
      saveRDS(dict_CN_EN, "output/dictionary.rds")
      write.csv(dict_CN_EN, # csv file for further use
                file = "output/dictionary.csv",
                fileEncoding = "UTF-16LE")
      Sys.setlocale() # restore default locale
    } # this part took several attempts, due to server time-out, connection errors, etc.
    # if it fails definitively, make sure Yandex bandwith is still open and
    # get the last step from "i" and repeated with "start = i-1" until finished
    
    if(exists("old_dict")){
      message("Appended new unique words and translations. \n")
    }
    message("Dictionary created. \n")
    
  }
  
  
}
