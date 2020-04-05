### This will control the dynamic server reactions to input.

## Structure
# 0. Preparation
# 1. Server Function
#     - Sidebar
#       - First Tab: Database Status
#       - Second Tab: Loaded Data Files
#       - Third Tab: Updating
#       - Fourth Tab: Simple Descriptives
#     - Other


#################################################################
# 0. Preparation
library(shiny)
require(ggplot2)
source("src/functions.R")


function(input, output, session){
  
  ###############################################################
  # Sidebar

  ############################
  # First Tab: Database Status

  output$earliest_article <- renderValueBox({
    valueBox(
      min(processed_articles_2019_page_01_EN$date),
      "Earliest Article in DB",
      icon = icon("list"),
      color = "purple"
    )
  })
  
  output$newest_article <- renderValueBox({
    valueBox(
      max(processed_articles_2020_page_01_EN$date),
      "Newest Article in DB",
      icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$database_status <- renderTable(database_status)
  

  ############################
  # Second Tab: Loaded Data Files
  output$loaded_out <- renderPrint({
    print(loaded_files)
  })

  output$text_loadedfiles <- renderText(
    " Restart app to reload files in the /output and /data folders."
  )
  
  ############################
  # Third Tab: Updating
  
  output$updating_text <- renderValueBox({
    valueBox(
      paste(Sys.Date() - max(processed_articles_2020_page_01_EN$date)),
      "daily edition(s) can be updated ",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  ############################
  # Fourth Tab: Simple Descriptives
  
  # article frequency  per day
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
  
  
  
  ###############################################################
  # other
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}
