### Language Processing (legacy code for starting from scratch)
## Tasks
# 1. Insert spaces to separate Chinese words for quant analysis
# 2. Create dictionary of unique words in articles
# 3. Request translation of the dictionary with translation API
# 4. Translate all articles word for word (for quant analysis only!)



#####################################################################
# 0. Preparation

# source self-written functions
source("src/functions.R")

# load/install required packages
require(purrr) # install.packages("purrr")
# require(devtools) # install.packages("devtools") # for installing packages

# install/load packages for separating Chinese words
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

# read article data and delete duplicates
articles <- readRDS("output/news_article_2020.rds") %>%
                      remove_duplicates()


#####################################################################
## 1. Insert spaces to separate Chinese words

# use the separation algorithm on each element
# make a list that inserts spaces

sep_articles <- insert_spaces(articles,
                              analyzer = "jiebaR",
                              # different options for "analyzer" : "default",
                              # "hmm", "jiebaR", "fmm","coreNLP"
                              nature = TRUE, # recognizes word nature
                              nosymbol = TRUE, # eliminates symbols
                              returnType = "tm" # default is insert spaces
)
saveRDS(sep_articles, "output/news_article_2020_sep.rds")



#####################################################################
## 2. Create dictionary of all words in articles

# make a list that returns the articles separated into word vectors
art_words <- insert_spaces(articles,
                              analyzer = "jiebaR",
                              # different options for "analyzer" : "default",
                              # "hmm", "jiebaR", "fmm","coreNLP"
                              nature = FALSE, # recognizes word nature
                              nosymbol = TRUE, # eliminates symbols
                              returnType = "vector" # default is insert spaces
)

# extract dictionary of unique words from article words
dict_CN <- art_words %>%
  extract_dictionary() %>%
  delete_numbers()



#####################################################################
# 3. Request translation of the dictionary with Yandex translation API
if(0){ # only run once! character limit: 1,000,000/day, 10,000,000 month
  Sys.setlocale(locale = "Chinese") # fix character encoding
  dict_CN_EN <- request_translation(dict_CN[2],
                     # free keys can be generated here:
                     # https://tech.yandex.com/translate/
                     api_key="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
                     start = 1 # if this fails at a certain "i", just restart at i-1
                     )
  saveRDS(dict_CN_EN, "output/dictionary.rds")
  write.csv(dict_CN_EN, # csv file for further use
            file = "output/dictionary.csv",
            fileEncoding = "UTF-16LE")
  Sys.setlocale() # restore default locale
} # this part took several attempts, due to server time-out, connection errors, etc.
  # get the last step from "i" and repeated with "start = i-1" until finished

"output/dictionary.rds" %>%
  readRDS  %>%  # load in case translation not run
  modify_if(is.factor, as.character) -> dict_CN_EN 


#####################################################################
# 4. Translate all articles word for word (for quant analysis only!)

# translate CN article words with the dictionary using the vector list
# and remerging chunks after translation
if(0){ # takes around 45 min.
  Sys.setlocale(locale = "Chinese") # fix character encoding
  art_words_EN <- translate_articles(art_words)
  saveRDS(art_words_EN, "output/news_article_2020_sep_EN.rds")
  Sys.setlocale() # restore default locale
}



