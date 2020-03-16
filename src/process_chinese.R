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
if(0){ # only run once! character limit: 1,000,000/day, 10,000,000 month
  dict_EN <- character(length = length(dict_CN)) # create empty English dictionary
  Sys.setlocale(locale = "Chinese") # fix character encoding
  api_key="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a"
  for(i in 1:length(dict_CN)){ # replace 1 here with last step "i" if time-out
    dict_EN[i] <- translate(api_key, text=dict_CN[i], lang="zh-en")$text
    if(i %% 100 == 0){cat(i, " out of ", length(dict_CN), "\n")}
  }
  dict_CN_EN <- data.frame(chinese = dict_CN, english = dict_EN)
  saveRDS(dict_CN_EN, "output/dictionary.rds")
  write.csv(dict_CN_EN, file = "output/dictionary.csv", fileEncoding = "UTF-16LE")
  Sys.setlocale() # restore default locale
} # this part took several attempts, due to server time-out
  # I just evaluated "i" to get the last step and repeated from there until finished

"output/dictionary.rds" %>%
  readRDS  %>%  # load in case translation not run
  modify_if(is.factor, as.character) -> dict_CN_EN 


#####################################################################
# 4. Translate all articles word for word (for quant analysis only!)

# below is the first try brute force matching each dictionary entry
# to the whole database, works slowly and with mistakes
if(0){ # takes half a day at the moment
  Sys.setlocale(locale = "Chinese") # fix character encoding
  sep_articles_EN <- sep_articles
  for(i in rownames(dict_CN_EN)){
  #for(i in 2800:length(rownames(dict_CN_EN))){
    for(j in names(sep_articles)){
      word_cn <- paste0(" ",dict_CN_EN[i,"chinese"], " ")
      word_en <- paste0(" ",dict_CN_EN[i,"english"], " ")
      sep_articles_EN[[j]] <- sep_articles_EN[[j]] %>%
        str_replace_all(., pattern = word_cn, replacement = word_en)
    }
    if(as.integer(i) %% 100 == 0){cat(i, " out of ", dim(dict_CN_EN)[1], "\n")}
  }
  saveRDS(sep_articles_EN, "output/news_article_2020_sep_EN.rds")
  Sys.setlocale() # restore default locale
}


# better approach:
# translate CN article words with the dictionary using the vector list
# and remerging chunks after translation
if(0){ # takes around 45 min.
  Sys.setlocale(locale = "Chinese") # fix character encoding
  art_words_EN <- art_words
  words_cn_to_en <- function(words_cn){ # translate one title, subtitle, or content chunk
      words_en <- modify(words_cn, ~ {
                        if(is.character(.x)){
                          word_en <- dict_CN_EN[dict_CN_EN[[1]] == .x,2] # find translation
                          str_replace(.x, pattern = .x, replacement = word_en)
                        }})
      if(words_en != "") return(words_en)
  }
  str_not_zero <- function(x){nchar(x) !=0} # nonempty character condition
  for(i in names(art_words)){
    for(j in 1:length(art_words[[i]])){
      art_words_EN[[i]][[j]] <- modify_if(art_words[[i]][[j]], str_not_zero,
                                ~ words_cn_to_en(.x))
      if(j %% 50 == 0){cat(i, j, " out of ", length(art_words[[i]]), "\n")}
    }
    art_words_EN[[i]] <- modify(art_words_EN[[i]], ~ str_c(., collapse = " "))
  }
  saveRDS(art_words_EN, "output/news_article_2020_sep_EN_v2.rds")
  Sys.setlocale() # restore default locale
}



