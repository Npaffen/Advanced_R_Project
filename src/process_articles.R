### Language Processing (legacy code for processing the raw articles)

## loads raw scraping output files "article_data"
## creates copies with spaces inserted for further analysis "processed_articles"

## Tasks (repeat for each article file)
# 0. Preparation
# 1. Insert spaces to separate Chinese words for quant analysis


process_articles <- function(){
  
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
  if(0){ # if using coreNLP, HMM
    require("coreNLP") # install.packages("coreNLP") 
    require(rJava) # install.packages("rJava")
    coreNLP::downloadCoreNLP()
    require("HMM") # install.packages("HMM")
  }
  require("jiebaR") # install.packages("jiebaR")
  
  # install/load packages for translating with the dictionary
  require("stringr") #install.packages("stringr")
  
  #####################################################################
  
  # find list of article files
  #wdir <- here::here()
  #files <- list.files(paste0(wdir, "/data"))
  #files <- files[grep("article_data", files)] 
  
  
  ### repeat for each article file
  
  for(i in files){
    # read article data and delete duplicates
    
    message("#### beginning processing of file:", i, "####\n")
    articles <- readRDS(paste0(wdir, "/data/", i)) %>%
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
    saveRDS(sep_articles, paste0("output/processed_articles",
                                 str_sub(i, start = 13, end=25),
                                 "_CN.rds"))
    message("sucessfully processed", i, "\n")
    
    
  }
  
  
}