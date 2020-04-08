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
  target,
  api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
  RUN_API = TRUE,
  # might take very long only if
  # you are really sure that yandex
  # API is available
  RUN_TRANSLATION = TRUE
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

  # check and make or update article data
  wdir <- here::here()
  raw_path <- paste0(wdir, "/data/", raw_file, ".rds")
  if(!file.exists(raw_path)){
    message(paste("Article data missing in folder '/data',
             will start scraping from scratch in 10 seconds,
             which might take very long. Close window to abort."))
    Sys.sleep(10)
    update_article_data(page_num = page_num, year = year,
                        write_to_disk = TRUE)
    } else {
      message(paste("Article_data files found, updating..."))
      update_article_data(page_num = page_num, year = year,
                          write_to_disk = TRUE)
    }


  # check and make or update processed articles
  path <- here::here(paste0("output/", processed_file, "_CN.rds"))

  if(!file.exists(path)){
    message(paste0("Processed Chinese article data missing in folder '/output', ",
                   "will start reprocessing in 5 seconds from scratch, ",
                   "which might take a while. Close window to abort."))
    source("app/process_articles.R")
    process_articles(year = year, page_num = page_num)
  } else {
    message(paste("Processed Chinese articles found, updating..."))
    process_articles(year = year, page_num = page_num)
  }

  # check and make dictionary if necessary
  if(!file.exists(here::here("output/dictionary.rds"))){
    message(paste("Dictionary data missing in folder '/output',
            will start recompiling in 5 seconds from scratch,
            which might take a while. Close window to abort."))
    Sys.sleep(5)
    create_dictionary(
      RUN_API = TRUE, # make sure Yandex is responding
      api_key = "trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a"
      )
  } else {
    message(paste("dictionary data found in '/output'. \n",
                "entries: ", dim(readRDS("output/dictionary.rds"))[1]))
  }
  dictionary <- readRDS("output/dictionary.rds")

  # check and make or update translated articles
  path <- here::here(paste0("output/", processed_file, "_EN.rds"))

  if(!file.exists(path)){
    message(paste0("Translated English article data missing in folder '/output', ",
                   "will start reprocessing in 5 seconds from scratch, ",
                   "which might take a while. Close window to abort."))
    RUN_UPDATE <- TRUE
  } else {
    message(paste("Processed Chinese articles found."))
  }


  if(RUN_UPDATE) {

    # source self-written functions
    source("app/functions.R")
    source("wrangling/update_article_data.R")
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


    message(paste("Running translation update..."))

    #####################################################################
    # 1. Identify new articles
    # compare with English, because it's the final output

    if(file.exists(here::here(paste0("output/",processed_file, "_EN.rds")))){
        # load old translated and new raw article data for comparison
        new_articles <- readRDS(raw_path) # new articles
        en_articles <- readRDS(paste0(wdir, "/output/",
                                      processed_file, "_EN.rds")) # old translated articles
        new <- anti_join(new_articles,
                       en_articles,
                       by="id")
        message(paste("Identified new articles: \n"))
        message(paste(as.character(new$date), collapse = "\n"))
    } else {
      message(paste("Article translation missing, translating from scratch..."))
      new <- readRDS(raw_path) # new articles
      en_articles <- new[0,]
    }


    #####################################################################
    # 2. Insert spaces with NLP tool
    message(paste("Vectorizing new articles with NLP algorithm."))
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
    # 3. Find new words not in dictionary or use old one
    dict_CN <- anti_join(enframe(dict_CN),
                         enframe(dictionary$chinese),
                         by = "value") %>% deframe %>% unname
    message(paste("Found ", length(dict_CN), " new words."))
    if(length(dict_CN) == 0){
      dict_CN_EN <- dictionary # use old dictionary
      RUN_API <- FALSE
      message(paste("No new words found for dictionary, none translated or added."))
    }

    #####################################################################
    # 4. Translate new words with API, articles with dictionary
    if(RUN_API){ # use sparingly! character limit: 1,000,000/day, 10,000,000 month
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

    if(RUN_TRANSLATION){ # can take some time
      Sys.setlocale(locale = "Chinese") # fix character encoding
      art_words <- insert_spaces(articles = new,
                                 analyzer = "jiebaR",
                                 # different options for "analyzer" : "default",
                                 # "hmm", "jiebaR", "fmm","coreNLP"
                                 nature = FALSE, # recognizes word nature
                                 nosymbol = TRUE, # eliminates symbols
                                 returnType = "vector" # default is insert spaces
      )
      art_words_EN <- translate_articles(art_words, dictionary)
      art_words_EN <- vctrs::vec_rbind(en_articles, art_words_EN, .ptype = en_articles)
      saveRDS(art_words_EN,
              paste0("output/processed_articles_",
                     year, "_page_", page_num, "_EN.rds")
      )
      Sys.setlocale() # restore default locale
    }

  }
  message("******* updating text data completed *******")
}
