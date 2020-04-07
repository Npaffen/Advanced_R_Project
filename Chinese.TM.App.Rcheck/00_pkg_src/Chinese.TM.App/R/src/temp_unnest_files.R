wdir <- here::here()
files <- list.files(paste0(wdir, "/output"))
files <- files[grep("EN", files)] 
for(i in files){
  cat("#### beginning translation of file:", i, "####\n")
  articles <- readRDS(paste0(wdir, "/output/", i))%>%
    remove_duplicates()
  vec_articles <- insert_spaces(articles,
                                analyzer = "jiebaR",
                                # different options for "analyzer" : "default",
                                # "hmm", "jiebaR", "fmm","coreNLP"
                                nature = FALSE, # recognizes word nature
                                nosymbol = TRUE, # eliminates symbols
                                returnType = "vector" # default is insert spaces
  )
  Sys.setlocale(locale = "Chinese") # fix character encoding
  
  vec_articles <- vec_articles %>% tidyr::unnest(subtitle) %>% tidyr::unnest(content)
  
  
  saveRDS(vec_articles,
          paste0("output/processed_articles",
                 str_sub(i, start = 19, end=31),
                 "_EN.rds")
  )
  Sys.setlocale() # restore default locale
}

cat("finished translating files:", files, sep = "\n")

