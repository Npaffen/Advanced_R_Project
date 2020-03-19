### All our functions alphabetically ordered

## CONTENT
# delete_numbers(), takes a char vector and deletes all numbers
# extract_dictionary() , extract dictionary of unique words from article words
# insert_spaces(), takes the .rds database and inserts spaces into text
# remove_duplicates(), from a tibble
# request_translation(), contacts the Yandex API to translate the dictionary
# translate_articles(), uses the dictionary to translate the articles
# words_cn_to_en(), translate a title, subtitle, or content chunk


### delete_numbers(), takes a char vector and deletes all numbers
delete_numbers <- function(strings){
  return(str_subset(strings, "^[^[:digit:]]+$"))
}

### extract dictionary of unique words from article words
extract_dictionary <- function(art_words){
  dict_CN <- art_words[,2:4] %>% unlist %>% unique
  return(dict_CN[dict_CN != ""]) # remove empty strings
}


### takes the .rds database and inserts spaces into text
insert_spaces <- function(articles,
                          analyzer = "jiebaR",
                          # different options for "analyzer" : "default",
                          # "hmm", "jiebaR", "fmm","coreNLP"
                          nature = TRUE, # recognizes word nature
                          nosymbol = TRUE, # eliminates symbols
                          returnType = "tm" # default is insert spaces
                          ) {
  modify_if(
    articles[, 1:dim(articles)[2]],
    is.character, # check if is nonzero character
    ~ segmentCN(.,
      analyzer = analyzer,
      nature = nature,
      nosymbol = nosymbol,
      returnType = returnType
    )
  )
}


### remove_duplicates(), from the articles tibble
remove_duplicates <- function(articles){
  return(articles[!duplicated(articles),])
}


### contacts the Yandex API to translate the dictionary
request_translation <- function(dict_CN,
                                api_key,
                                start = 1 # replace 1 here with last step "i" if time-out
                                ){
  dict_EN <- character(length = length(dict_CN)) # create empty English dictionary
  for(i in start:length(dict_CN)){ 
    # translate each single entry, to avoid "contamination" from using many at once
    try(dict_EN[i] <- translate(api_key, text=dict_CN[i], lang="zh-en")$text)
    if(length(dict_EN) == 0){
      write(paste0("request failed at i = ", i,
                   ", retry request_translation() with \"start = ", i, "-1\""),
            stderr())
      }
    if(i %% 100 == 0){cat(i, " out of ", length(dict_CN), "\n")}
  }
  return(data.frame(chinese = dict_CN, english = dict_EN, stringsAsFactors = FALSE))
}




### uses the dictionary to translate the articles
translate_articles <- function(art_words){
  art_words_EN <- art_words # translation target
  str_not_zero <- function(x){nchar(x) !=0} # nonempty character condition
  for(i in names(art_words)[-1]){ 
    for(j in 1:length(art_words[[i]])){
      art_words_EN[[i]][[j]] <- modify_if(art_words[[i]][[j]], str_not_zero,
                                          ~ words_cn_to_en(.x))
      if(j %% 50 == 0){cat(i, j, " out of ", length(art_words[[i]]), "\n")}
    }
    art_words_EN[[i]] <- modify(art_words_EN[[i]], ~ str_c(., collapse = " "))
    # collapse the vectors back to text chunks
    return(art_words_EN)
  }
}



### translate a title, subtitle, or content chunk
words_cn_to_en <- function(words_cn){
  words_en <- modify(words_cn, ~ {
    if(is.character(.x)){
      word_en <- dict_CN_EN[dict_CN_EN[[1]] == .x,2] # find translation
      if(length(word_en) != 0) {
        str_replace(.x, pattern = .x, replacement = word_en)
      } else {.x}
    }})
  if(words_en != "") return(words_en) else return(words_cn)
}
