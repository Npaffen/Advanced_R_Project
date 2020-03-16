### Language Processing
## Tasks
# 1. Insert spaces to separate Chinese words for quant analysis
# 2. Create dictionary of unique words in articles
# 3. Request translation of the dictionary with translation API
# 4. Translate all articles word for word (for quant analysis only!)



#####################################################################
# 0. Preparation

# load/install required packages
require(purrr) # install.packages("purrr")
require(devtools) # install.packages("devtools") # for installing packages

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

# read article data
articles <- readRDS("output/news_article_2020.rds")



#####################################################################
## 1. Insert spaces to separate Chinese words

# use the separation algorithm on each element
# make a list that inserts spaces
sep_articles <- modify_if(articles[,2:dim(articles)[2]], is.character,
                          ~ segmentCN(.,
                                      analyzer = "jiebaR",
                                      # different options for "analyzer" : "default", 
                                      #   "hmm", "jiebaR", "fmm","coreNLP"
                                      nature = TRUE,  # recognizes word nature
                                      nosymbol = TRUE, # eliminates symbols
                                      returnType = "tm" ) # returns string sep by spaces)
                          )
saveRDS(sep_articles, "output/news_article_2020_sep.rds")



#####################################################################
## 2. Create dictionary of all words in articles

# make a list that returns vectors of words for the dictionary
art_words <- modify_if(articles[,2:dim(articles)[2]], is.character,
                       ~ segmentCN(.,
                                   analyzer = "jiebaR",
                                   # different options for "analyzer" : "default", 
                                   #   "hmm", "jiebaR", "fmm","coreNLP"
                                   nature = FALSE,  # recognizes word nature
                                   nosymbol = TRUE, # eliminates symbols
                                   returnType = "vector" ) # returns vectors of strings)
)

# extract dictionary of unique words from article words
dict_CN <- art_words %>% unlist %>% unique
dict_CN <- dict_CN[dict_CN != ""] # remove empty string



#####################################################################
# 3. Request translation of the dictionary with Yandex translation API
dict_EN <- character(length = length(dict_CN)) # create empty English dictionary
Sys.setlocale(locale = "Chinese") # fix character encoding
if(0){ # only run once! character limit: 1,000,000/day, 10,000,000 month
  api_key="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a"
  for(i in 1:length(dict_CN)){ # replace 1 here with last step "i" if time-out
    dict_EN[i] <- translate(api_key, text=dict_CN[i], lang="zh-en")$text
    if(i %% 100 == 0){cat(i, " out of ", length(dict_CN), "\n")}
  }
  dict_CN_EN <- data.frame(chinese = dict_CN, english = dict_EN)
  saveRDS(dict_CN_EN, "output/dictionary.rds")
  write.csv(dict_CN_EN, file = "output/dictionary.csv", fileEncoding = "UTF-16LE")
} # this part took several attempts, due to server time-out
  # I just evaluated "i" to get the last step and repeated from there until finished
Sys.setlocale() # restore default locale
"output/dictionary.rds" %>%
  readRDS  %>%  # load in case translation not run
  mutate_if(is.factor, as.character) -> dict_CN_EN 


#####################################################################
# 4. Translate all articles word for word (for quant analysis only!)

# below is brute force matching each dictionary entry to the whole
# database, open to suggestions that improve performance
# idea: reverse maching would be faster
Sys.setlocale(locale = "Chinese") # fix character encoding
if(0){ # takes hours at the moment
  sep_articles_EN <- sep_articles
  #for(i in rownames(dict_CN_EN)){
  for(i in 2800:length(rownames(dict_CN_EN))){
    for(j in names(sep_articles)){
      sep_articles_EN[[j]] <- sep_articles_EN[[j]] %>%
        str_replace_all(., dict_CN_EN[i,"chinese"], dict_CN_EN[i,"english"])
    }
    if(as.integer(i) %% 100 == 0){cat(i, " out of ", dim(dict_CN_EN)[1], "\n")}
  }
  saveRDS(sep_articles_EN, "output/news_article_2020_sep_EN.rds")
  Sys.setlocale() # restore default locale
  
}



