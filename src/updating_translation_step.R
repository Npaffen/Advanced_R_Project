### Updating Translation (for use in the app, requires existing translations)
## Tasks
# 1. Identify new articles
# 2. Insert spaces with NLP tool
# 3. Find new words not in dictionary
# 4. Translate new words with API, articles with dictionary
# 5. Update databases 

testing <- FALSE # activate if testing
run_update <- TRUE # activate if updating

#####################################################################
# 0. Preparation

# check if there is anything to update (without id, just comparing dates)
new_articles <- readRDS("output/news_article_2020.rds") # load new articles
articles_cn <- readRDS("output/news_article_2020_sep.rds") # load old articles
if(!testing){
  if(suppressWarnings(all(sort(new_articles$Date) == sort(articles_cn$Date)))){
    run_update <- FALSE
    stop("no new articles to update")
  }
}


if(run_update) {
  
  # source self-written functions
  source("src/functions.R")
  
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
  articles_en <- readRDS("output/news_article_2020_sep_EN.rds")
  dictionary <- readRDS("output/dictionary.rds")
  
  # remove duplicates from articles
  new_articles <- remove_duplicates(new_articles)
  articles_cn <- remove_duplicates(articles_cn)
  articles_en <- remove_duplicates(articles_en)
  
  # for testing with subsample
  if(testing){ 
    articles_cn <- articles_cn[1:800,]
    articles_en <- articles_en[1:800,]
    dictionary <- dictionary[1:20000,]
  }
  
  
  
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
  if(0){ # use sparingly! character limit: 1,000,000/day, 10,000,000 month
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
  if(0){ # takes around 45 min.
    Sys.setlocale(locale = "Chinese") # fix character encoding
    art_words_EN <- translate_articles(art_words)
    art_words_EN <- bind_rows(articles_en, art_words_EN)
    saveRDS(art_words_EN, "output/news_article_2020_sep_EN.rds")
    Sys.setlocale() # restore default locale
  }
  
  
} # end of update