library(tidyverse)
library(lubridate)
library(glue)
# Updating data as new articles unfold every new day

update_article_data <- function(page_num,
                                write_to_disk = FALSE) {
  stopifnot(page_num %in% c("01", "02"))
  
  path <- paste0("data/article_data_2020_page_", page_num, ".rds")
  
  nms <- str_sub(path, 6, -5)
  
  names(path) <- nms
  
  df <- map(path, read_rds)
  
  last_updated <- map(df, ~ max(.x[["date"]]))
  
  # diff <- map(last_updated, ~ today() - .x)
  
  for (j in seq_along(last_updated)) {
    message(glue("Page {page_num[[j]]}, last updated on {last_updated[[j]]}.\n"))
  }
  
  last_updated <- keep(last_updated, ~ .x < today())
  
  if (!is_empty(last_updated)) {
    dates_to_be_updated <- map(
      last_updated,
      ~ seq.Date(.x + 1, today(), by = 1)
    )
    
    source("src/scrape_article.R")
    
    dat <- map2(
      page_num, dates_to_be_updated,
      ~ scrape_article(.x, .y)
    )
    names(dat) <- nms
    
    for (nm in nms) {
      df[[nm]] <- bind_rows(df[[nm]], dat[[nm]])
    }
    
    if (write_to_disk) {
      walk2(df, names(df), 
            ~ saveRDS(.x, paste0("data/", .y, ".rds")))
    }
    
    pwalk(list(df, dat, names(df)), function(.x, .y, .z) {
      message("successfully updated!!")
      
      cat(
        "nrows of ", "<<", .z, ">>: ",
        "<<", (nrow(.x) - nrow(.y)), ">> ",
        "--> ",
        "<<", nrow(.x), ">>", ".", "\n"
      )
    })
    df
  } else {
    print("It seems that the data set is up-to-date")
  }
}
