### Language Processing (legacy code for translating articles from scratch)

## only makes sense if you created or updated the dictionary before
## otherwise not all will be translated

## Tasks (repeat for each article file)
# 0. Preparation
# 1. Separate articles into vectors of words and check for old version
# 2. Translate all articles word for word (for quant analysis only!)

RUN_TRANSLATION <- FALSE # set to true only if you have several hours to translate everything
# if there are many new articles
OVERWRITE <- FALSE # set to true if you suspect the old translation is corrupt

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


# install/load packages for translating with the dictionary
require("stringr") #install.packages("stringr")

#####################################################################

# find list of article files
wdir <- here::here()
files <- list.files(paste0(wdir, "/data"))
files <- files[grep("article_data", files)] 

# load dictionary
paste0(wdir, "/output/dictionary.rds") %>%
  readRDS  %>%  
  modify_if(is.factor, as.character) -> dict_CN_EN 


### repeat for each article file

for(i in files){
  # read article data and delete duplicates
  
  cat("#### beginning processing of file:", i, "####\n")
  articles <- readRDS(paste0(wdir, "/data/", i))%>%
    remove_duplicates()
  
  
  #####################################################################
  ## 1. Separate articles into vectors of words and check for old version
  
  # make a list that returns the articles separated into word vectors
  vec_articles <- insert_spaces(articles,
                                analyzer = "jiebaR",
                                # different options for "analyzer" : "default",
                                # "hmm", "jiebaR", "fmm","coreNLP"
                                nature = FALSE, # recognizes word nature
                                nosymbol = TRUE, # eliminates symbols
                                returnType = "vector" # default is insert spaces
  )
  
  # check if there are existing old translations
  old_files <- list.files(paste0(wdir, "/output"))
  old_files <- old_files[
    grep("processed_articles.*_EN.rds", old_files)]
  if(length(old_files) != 0 & !OVERWRITE ){
    current <- str_sub(i, start = 14, end = 25)
    if(grepl(current, old_files)){
      cat("found existing translation. stopping. to overwrite set flag in translate_articles.R \n")
      next
    }
  }
  
  
  #####################################################################
  # 2. Translate all articles word for word (for quant analysis only!)
  
  # translate CN article words with the dictionary using the vector list
  # and remerging chunks after translation
  if(RUN_TRANSLATION){ # might take over one hour
    Sys.setlocale(locale = "Chinese") # fix character encoding
    vec_articles_EN <- translate_articles(vec_articles)
    saveRDS(vec_articles_EN,
            paste0("output/processed_articles",
                   str_sub(i, start = 13, end=25),
                   "_EN.rds")
    )
    Sys.setlocale() # restore default locale
  }
  
  
}


