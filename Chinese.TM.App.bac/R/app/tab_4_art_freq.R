
### Fourth Tab: Plot article frequency  per day #########################

output$artfreq_desc <- renderText(paste(
  "These plots use the updated data to show patterns in article frequency per day. ",
  "Frequency can vary quite a lot, depending on for example the size of pictures ",
  "or the amount of headlines that have to fit on the first page. We also see less ",
  "outliers in frequency on the second page. Generally, there seems to be a seasonality ",
  "of articles, with less articles appearing over the spring festival and summer holidays."
))

output$art_freq191 <- renderPlot(
  render_frequency(file = processed_articles_2019_page_01_CN,
                   file_name = "processed_articles_2019_page_01_CN"))
output$art_freq192 <- renderPlot(
  render_frequency(file = processed_articles_2019_page_02_CN,
                   file_name = "processed_articles_2019_page_02_CN"))
output$art_freq201 <- renderPlot(
  render_frequency(file = processed_articles_2020_page_01_CN,
                   file_name = "processed_articles_2020_page_01_CN"))
output$art_freq202 <- renderPlot(
  render_frequency(file = processed_articles_2020_page_02_CN,
                   file_name = "processed_articles_2020_page_02_CN"))

# output plots in tabs
output$artfreqs <- renderUI({
  tabBox(title = "Articles per Day",id= "artfreqtab",
         tabPanel("19-1", plotOutput("art_freq191")),
         tabPanel("19-2", plotOutput("art_freq192")),
         tabPanel("20-1", plotOutput("art_freq201")),
         tabPanel("20-2", plotOutput("art_freq202"))
  )
})
