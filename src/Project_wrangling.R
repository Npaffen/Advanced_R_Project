pkgs <- c("tidyr", "ggplot2", "dplyr", "purrr", "readr", "stringr", 
          "haven", "tibble", "broom", "forcats", "lubridate", "tidyverse", 
          "readxl", "here", "janitor", "usethis", "scales")

sapply(pkgs, require, character.only = T, quietly = T, warn.conflicts = T)

rm(pkgs)

# Helper functions and creating vectors of names and tidying the
# database which comes as a nested list of daily news articles.
# --------------------------


var_names <- c("Date", "subtitle", "title", "content")  # names for the columns of
# the final data frames

# creats a vector of names for x.

create_names <- function(x, day_or_article = "Day") {
  len <- length(x)
  nms <- paste0(day_or_article, "_", seq(1, len, by = 1))
  nms
}



### takes a list (of one month new article data) and returns a
### tibble with 5 vars

tidy_it <- function(database) {
  
  out <- vector("list", length(database))
  
  for (m in seq_along(out)) out[[m]] <- vector("list", length(database[[m]]))
  
  out <- set_names(out, create_names(out, "Day"))
  
  for (h in seq_along(out)) names(out[[h]]) <- create_names(out[[h]], 
                                                            "article")
  
  for (day in seq_along(database)) {
    for (article in seq_along(database[[day]])) {
      
      out[[day]][[article]] <- set_names(x = database[[day]][[article]], 
                                         nm = create_names(x = database[[day]][[article]], 
                                                           day_or_article = "var")) %>% 
        flatten() %>% as_tibble(x = ., .name_repair = "unique") %>% 
        mutate(var_4 = paste(select(., -contains("var")), collapse = "||")) %>% 
        select(contains("var"))
    }
  }
  
  df <- out %>% flatten() %>% bind_rows(.id = "id")
  
  rm(out)
  
  Day_of_the_month <- map(.x = seq_along(database), 
                          .f = ~rep(
                            glue::glue("Day_{.x}"), length(database[[.x]])) 
                          ) %>%  unlist()
  
  colnames(df) <- c("article_number", var_names)
  
  df$Day_of_the_month <- Day_of_the_month
  
  df <- df %>% mutate(Date = str_extract(Date, "\\d{4}.\\d{2}.\\d{2}")) %>% 
    mutate(Date = lubridate::ymd(str_replace_all(Date, "[^0-9]",  "-")))
  
  return(df)
}

read_and_tidy <- function(x, fun) {
  df <- readRDS(x)
  df <- fun(df)
  return(df)
}

tidy_it2 <- function(database) {
  df <- map_df(database, function(.x) {
    tibble(
      Date = map_chr(.x, 1), 
      subtitle = map_chr(.x, 2), 
      title = map_chr(.x, 3), 
      content = map_chr(.x, function(x) x[[4]] %>% paste0(collapse = "||")))
  })
  # parse Date from the Date column of each tibble.
  df <- mutate(.data = df, 
               Date = str_extract(Date, "\\d{4}.\\d{2}.\\d{2}")) %>% 
      mutate(.data = ., 
             Date = lubridate::ymd(str_replace_all(Date, "[^0-9]", "-")))
  return(df)
}



paths <- fs::dir_ls("data/")
names(paths) <- str_sub(paths, start = -16, end = -5)

# using function tidy_it
df <- suppressMessages(purrr::map(paths[-c(11, 12)], read_and_tidy, fun = tidy_it))

# using function tidy_it2
df_2 <- purrr::map(paths[-c(11, 12)], read_and_tidy, fun = tidy_it2)


##### ----- write files into .csv or .rds


# fs::dir_create('output/tidy_it')
# fs::dir_create('output/tidy_it2')

walk2(.x = df, 
      .y = names(df), 
      ~write_rds(.x, paste0('output/tidy_it/', .y, '.rds')))

walk2(.x = df_2, 
      .y = names(df_2), 
      ~write_rds(.x, paste0('output/tidy_it2/', .y, '.rds')))

# all in one

news_article_2020 <- df_2 %>% bind_rows()

write_rds(news_article_2020, 'output/new_article_2020.rds')








