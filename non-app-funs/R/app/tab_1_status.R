### First Tab: Database Status #########################

# show earliest article info
try(minart <- min(processed_articles_2019_page_01_EN$date))
if(exists("minart")){
  output$earliest_article <- renderValueBox({
    valueBox(
      minart,
      "Earliest complete Article in DB",
      icon = icon("list"),
      color = "purple"
    )
  })
} else {
  output$earliest_article <- renderValueBox({
    valueBox(
      "NA",
      "Earliest complete Article in DB",
      icon = icon("list"),
      color = "purple"
    )
  })

}

# show newest article info
try(maxart <- max(processed_articles_2020_page_01_EN$date))
if(exists("maxart")){
  output$newest_article <- renderValueBox({
    valueBox(
      maxart,
      "Newest complete Article in DB",
      icon = icon("list"),
      color = "yellow"
    )
  })
} else {
  output$newest_article <- renderValueBox({
    valueBox(
      "NA",
      "Newest complete Article in DB",
      icon = icon("list"),
      color = "yellow"
    )
  })
}



output$database_status <- renderTable(database_status)
