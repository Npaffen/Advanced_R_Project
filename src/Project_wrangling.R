
# load packages -----------------------------------------------------------


pkgs <- c(
  "tidyr", "ggplot2", "dplyr", "purrr", "readr", "stringr",
  "tibble", "lubridate", "here", "fs"
)

sshhh_library <- function(pkg) {
  suppressWarnings(suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  ))
}


invisible(sapply(pkgs, sshhh_library))


rm(pkgs, sshhh_library)


# write helper functions ------------------------------------------------------

# Helper functions for creating vectors of names and tidying the
# database which comes as a nested list of daily news articles.

# creats a vector of names for x.

create_names <- function(x, name_prefix) {
  len <- length(x)
  nms <- paste0(name_prefix, "_", seq(1, len, by = 1))
  nms
}



# tidy_it() takes a list (of one month news article dataset) & returns a tibble.

tidy_it <- function(database) {
  out <- vector("list", length(database))

  for (m in seq_along(out)) {
    out[[m]] <- vector("list", length(database[[m]]))
  }
  out <- set_names(out, create_names(out, "Day"))

  for (h in seq_along(out)) {
    names(out[[h]]) <- create_names(out[[h]], "article")
  }

  for (day in seq_along(database)) {
    for (article in seq_along(database[[day]])) {
      out[[day]][[article]] <- database[[day]][[article]] %>%
        set_names(create_names(x = ., name_prefix = "var")) %>%
        flatten() %>%
        as_tibble(x = ., .name_repair = "unique") %>%
        mutate(var_4 = paste(select(., -contains("var")), collapse = "||")) %>%
        select(contains("var"))
    }
  }

  df <- out %>%
    flatten() %>%
    bind_rows(.id = "id")

  rm(out)

  Day_of_the_month <- map(
    .x = seq_along(database),
    .f = ~ rep(
      glue::glue("Day_{.x}"), length(database[[.x]])
    )
  ) %>% unlist()

  colnames(df) <- c("article_number", "date_and_page_no", "subtitle", "title", "content")

  df$Day_of_the_month <- Day_of_the_month

  df <- df %>%
    mutate(date_and_page_no = str_remove_all(date_and_page_no, "\\D*")) %>%
    mutate(date = lubridate::ymd(str_sub(date_and_page_no, end = -3)), 
           page_no = str_sub(date_and_page_no, -2, -1)) %>% 
    select(date, subtitle, title, content, page_no, date_and_page_no, everything())
  return(df)
}


# even faster

tidy_it2 <- function(database) {
  df <- map_df(database, function(x) {
    tibble(
      date_and_page_no = map_chr(x, 1),
      subtitle = map_chr(x, 2),
      title = map_chr(x, 3),
      content = map_chr(x, function(xx) paste0(xx[[4]], collapse = "||"))
    )
  })
  # parse date from the date column of each tibble.
  df <- mutate(
    .data = df,
    date_and_page_no = str_remove_all(date_and_page_no, "\\D*")
  ) %>%
    mutate(
      .data = .,
      date = lubridate::ymd(str_sub(date_and_page_no, end = -3)),
      page_no = str_sub(date_and_page_no, -2, -1)
    ) %>% select(date, subtitle, title, content, page_no, everything())
  return(df)
}


# convert lists into tidy tibbles -----------------------------------------

read_and_tidy <- function(x, fun) {
  df <- readRDS(x)
  df <- fun(df)
  return(df)
}

paths_names <- c("database_m01", "database_m02", "database_m03", 
           "database_m04", "database_m05", "database_m06", 
           "database_m07", "database_m08", "database_m09", 
           "database_m10", "database_m11", "database_m12")



paths <- paste0("data/", paths_names, '.rds')

names(paths) <- paths_names

# using function tidy_it
df <- suppressMessages(
  purrr::map(paths, read_and_tidy, fun = tidy_it)
)

# using function tidy_it2
df_2 <- purrr::map(paths, read_and_tidy, fun = tidy_it2)


##### ----- write files into .csv or .rds


# fs::dir_create('output/tidy_it')
# fs::dir_create('output/tidy_it2')

walk2(
  .x = df,
  .y = names(df),
  ~ write_rds(.x, paste0("output/tidy_it/", .y, ".rds"))
)

walk2(
  .x = df_2,
  .y = names(df_2),
  ~ write_rds(.x, paste0("output/tidy_it2/", .y, ".rds"))
)

# all in one

news_article_2020 <- bind_rows(df_2)

write_rds(news_article_2020, "output/news_article_2020.rds")




# extras  -------------------------------------------------------



# pattern <- paste0(
#   c(
#     not_digit_1 = "\\D*",
#     yyyy.mm.dd = "(\\d{4}).(\\d{2}).(\\d{2})",
#     not_digit_2 = "\\D*",
#     page_no = "(\\d{1,})",
#     not_digit_3 = "\\D*"
#   ),
#   collapse = ""
# )
# 
# replacement <- paste0(
#   c("yyyy-mm-dd" = "\\1-\\2-\\3",
#   page_no = "|\\4"
# ), collapse = "")
# 
# str_remove_all(df[, 1], "\\D*") %>% str_sub(end = -3)
# 
# str_replace(df[, 1], pattern, replacement)
