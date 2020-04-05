### This will control the dynamic server reactions to input.

## Structure
# 0. Preparation
# 1. Server Function
#     - Sidebar
#       - First Tab: Database Status
#       - Second Tab: Loaded Data Files
#       - Third Tab: Updating
#       - Fourth Tab: Plot article frequency  per days
#     - Other
# 2. Try Updating


#################################################################
# 0. Preparation
library(shiny)
require(ggplot2)
source("src/functions.R")
source("src/updating_articles_app.R")

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
      paste(Sys.Date() - max(article_data_2020_page_01$date)),
      "days can be updated ",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  vals <- reactiveValues() # stored values
  vals <- update_is_long <- FALSE
  
  confirm_update <- function() { # Confirmation pop-up
    modalDialog(
      span('A long update is pending, do you have time?'),
      footer = tagList(
        modalButton("No"),
        actionButton("button_long_update", "Yes")
      )
    )
  }
  
  observeEvent(input$run_update,{ # if button update is pressed
    output$update_report <- renderUI({
      HTML( # output capture borrowed from https://stackoverflow.com/a/40711365
        paste(capture.output(type = "message", expr = { 
          message(capture.output(type = "output", expr = {
            request <- eval(as.name(paste0(
              "article_data_2020_page_",
              input$request_page_num)))
            if((Sys.Date() - max(request$date)) == 0){
              message("Data is up to date!")
            } else if(Sys.Date() - max(request$date) > 7){
              message("Data is older than 1 week! Long update?")
              update_is_long <<- TRUE
              showModal(confirm_update()) 
            } else{
              message("Data is recent, will be updated.")
            }
            #update_article_data(page_num = input$request_page_num,
            #                    write_to_disk = TRUE)
            #if(Sys.Date() - max(processed_articles_2020_page_01_EN$date) > 0){
            #  process_articles()
            #}
          }))
        }), collapse="<br>")
      )})
  })
  
  observeEvent(input$button_long_update, {
    output$update_report <- renderUI({
      HTML( # output capture borrowed from https://stackoverflow.com/a/40711365
        paste(capture.output(type = "message", expr = { 
          message(capture.output(type = "output", expr = {
            message("Starting long update...")
            #update_article_data(page_num = input$request_page_num,
            #                    write_to_disk = TRUE)
            #if(Sys.Date() - max(processed_articles_2020_page_01_EN$date) > 0){
            #  process_articles()
            #}
          }))
        }), collapse="<br>")
      )})
    removeModal()
  })
  
  
  



  
  ############################
  # Fourth Tab: Plot article frequency  per day
  
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
  
  
  
  ###########################
  # other
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}

#################################################################
# 1. Try Updating

