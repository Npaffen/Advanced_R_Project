### Updating Translation (for use in the app, requires existing translations)
## only checks if something is missing in 2020!
## Tasks
# 0. Preparation
# 1. Identify new articles
# 2. Insert spaces with NLP tool
# 3. Find new words not in dictionary
# 4. Translate new words with API, articles with dictionary
# 5. Update databases 


updating_text_data_app <- function(
  target = "2020-01",
  api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
  TESTING = FALSE,
  # activate if testing
  RUN_API = TRUE,
  # might take very long only if
  # you are really sure that yandex
  # API is available
  RUN_TRANSLATION = FALSE
  # might take very long
  ){
  
  RUN_UPDATE <- FALSE # will self-activate if updating
  
  
  #####################################################################

  # construct target file names
  year <- substr(target, 1, 4)
  page_num <- substr(target, 6, 7)
  raw_file <- paste0("article_data_", year,
                 "_page_", page_num)
  processed_file <- paste0("processed_articles_", year,
                     "_page_", page_num)
  
  # check if relevant files are present and create if necessary
  
  # check article data
  wdir <- here::here()
  raw_path <- paste0(wdir, "/data/", raw_file, ".rds")
  if(!file.exists(raw_path)){
    message("Article data missing in folder '/data',
             will start scraping from scratch in 10 seconds,
             which might take very long. Close window to abort.")
    Sys.sleep(10)
    update_article_data(page_num = page_num,
                        write_to_disk = TRUE)
    } else {
      message("Article_data files found, updating...")
      update_article_data(page_num = page_num,
                          write_to_disk = TRUE)
    }

  
  # check processed articles
  
  path <- paste0(wdir, "/output/", processed_file, "_CN.rds")
  
  if(!file.exists(path)){
    message("Processed Chinese article data missing in folder '/output',
            will start reprocessing in 5 seconds from scratch,
            which might take a while. Close window to abort.")
    source("src/process_articles.R")
    process_articles()
    } else {
    message("Processed Chinese articles found, updating...")
    process_articles()
  }
  
  # dictionary
  if(!file.exists(paste0(wdir,"/output/dictionary.rds"))){
    message("Dictionary data missing in folder '/output',
            will start recompiling in 5 seconds from scratch,
            which might take a while. Close window to abort.")
    Sys.sleep(5)
    create_dictionary(
      RUN_API = TRUE, # make sure Yandex is responding
      api_key = "trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a"
      )
  } else {
    message(cat("dictionary data found in '/output'. \n",
                "entries: ", dim(readRDS("output/dictionary.rds"))[1]))
    dictionary <- readRDS("output/dictionary.rds")
  }
  
  
  # load old translated and new raw artcile data for comparison 
  new_articles <- readRDS(raw_path) # new articles 
  en_articles <- readRDS(paste0(wdir, "/output/",
                                processed_file, "_EN.rds")) # old translated articles 
  
  # remove some articles when testing
  if(TESTING){
    articles_en <- articles_en[1:(length(articles_en$id)-20),]
  }
  
  # check if updates required
  missing <- suppressWarnings(order(new_articles$id) != order(en_articles$id))

  if(sum(missing) == 0){
    RUN_UPDATE <- FALSE
    stop("Target file is up-to-date, aborting...")
  } else{
    RUN_UPDATE <- TRUE
    message(cat("Missing articles: ",
                sum(missing),
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
    
   
    
    
    #####################################################################
    # 1. Identify new articles
    # compare with English, because it's the final output 
    
    new <- anti_join(new_articles,
                     en_articles,
                     by="id")
    message(cat("Identified new articles: \n",
                as.character(new$date), sep = "        "))
    
    
    #####################################################################
    # 2. Insert spaces with NLP tool
    message("Vectorizing new articles with NLP algorithm.")
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
    message(cat("Found ", length(dict_CN), " new words."))
    
    
    #####################################################################
    # 4. Translate new words with API, articles with dictionary
    if(1){ # use sparingly! character limit: 1,000,000/day, 10,000,000 month
      Sys.setlocale(locale = "Chinese") # fix character encoding
      new_dict_CN_EN <- request_translation(dict_CN,
                                            # free keys can be generated here:
                                            # https://tech.yandex.com/translate/
                                            api_key = api_key,
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
    if(1){ # takes around 45 min.
      Sys.setlocale(locale = "Chinese") # fix character encoding
      art_words_EN <- translate_articles(art_words)
      art_words_EN <- bind_rows(en_articles, art_words_EN)
      saveRDS(art_words_EN,
              paste0("output/processed_articles_",
              year, "_page_", page_num, "_EN.rds")
      )
      Sys.setlocale() # restore default locale
    }
  }

}