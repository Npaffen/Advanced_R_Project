### Language Processing (legacy code for processing the raw articles)

## loads raw scraping output files "article_data"
## creates copies with spaces inserted for further analysis "processed_articles"

## Tasks (repeat for each article file)
# 0. Preparation
# 1. Insert spaces to separate Chinese words for quant analysis


process_articles <- function(year, page_num){

  #####################################################################
  # 0. Preparation

  # source self-written functions
  source(str_c(here::here(), "R/app/functions.R", sep = "/"))
   # load/install required packages
  # require(devtools) # install.packages("devtools") # for installing packages
  # install/load packages for separating Chinese words
  # "Rwordseg" by Jian Li (2019) (https://github.com/lijian13/Rwordseg)
  # "coreNLP" by Arnold Taylor and Lauren Tilton (2016)
  # devtools::install_github("lijian13/Rwordseg")
  if(0){ # if using coreNLP, HMM
    # install.packages("coreNLP")
    # install.packages("rJava")
    coreNLP::downloadCoreNLP()
    # install.packages("HMM")
  }
  # install.packages("jiebaR")
  # install/load packages for translating with the dictionary
  #install.packages("stringr")

  #####################################################################


  # define target raw article data file
  file <- paste0("article_data_", year, "_page_",page_num, ".rds")

  # read article data and delete duplicates

  message("#### beginning processing of file: ", file, " ####\n")
  articles <- readRDS(paste0(wdir, "/data/", file)) %>%
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
                               str_sub(file, start = 13, end=25),
                               "_CN.rds"))
  message("successfully processed ", file, "\n")


}
