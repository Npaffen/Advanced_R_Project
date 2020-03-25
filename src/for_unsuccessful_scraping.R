### Unsuccessful scraping

# article_url_unsuccessful <- tibble(
#  id = setdiff(article_urls$cols, article_data_ok$id),
#  date = article_urls$date[article_urls$cols %in% id], 
#  cols_url = article_urls$cols_url[article_urls$cols %in% id],
#  article_url=article_urls$article_url[article_urls$cols %in% id]
#  )

# missing <- map(new_missing$cols_url, safely_slowly_extract)
# 
# names(missing) <- new_missing$cols
# 
# missing <- missing %>% transpose()
# 
# is_ok_missing <- missing$error %>% map_lgl(is_null)
# 
# missing_ok <- bind_rows(missing$result[is_ok_missing], .id = 'id')



# saveRDS(article_data_ok, 'data/article_data_ok.rds')

# final_article <- bind_rows(article_data_ok, missing_ok)
# 
# final_article <- final_article %>% arrange(id)