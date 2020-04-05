### This will control the dynamic server reactions to input.

## Structure
# 0. Preparation
# 1. Server Function
#     - Sidebar
#       - First Tab: Database Status
#       - Second Tab: Loaded Data Files
#       - Third Tab: Updating
#     - Other


#################################################################
# 0. Preparation
library(shiny)


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
  
  
  
  ###############################################################
  # other
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}
