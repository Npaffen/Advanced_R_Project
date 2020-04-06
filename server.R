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
require(shinyjs)
require(ggplot2)
source("src/functions.R")
source("src/updating_text_data_app.R")
source("src/update_article_data.R")
source("src/process_articles.R")
source("src/create_dictionary.R")

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
      "days ready for update",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$updating_description <- renderText(
    "Type out the year and page number you wish to update and press the button."
  )
  
  confirm_update <- function() { # Confirmation pop-up
    modalDialog(
      span('A long update is pending, do you have time?'),
      footer = tagList(
        modalButton("No"),
        actionButton("button_long_update", "Yes")
      )
    )
  }
  
  update_in_app <- function(request_year_page){ # initiates update
    year <- substr(request_year_page, 1, 4)
    page <- substr(request_year_page, 6, 7)
    request <- eval(as.name(paste0(
      "article_data_", year,
      "_page_", page)))
    if((Sys.Date() - max(request$date)) == 0){
      message("Data is up to date!")
    } else if(Sys.Date() - max(request$date) > 7){
      message("Data is older than 1 week! Perform long update?")
      showModal(confirm_update())
    } else{
      message("Data is recent, will be updated.")
      withCallingHandlers({
        shinyjs::html("html", "")
        updating_text_data_app(
          target = input$request_year_page,
          api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
          TESTING = FALSE,
          RUN_API = TRUE,
          RUN_TRANSLATION = TRUE
        )
      },
      message = function(m) {
        shinyjs::html(id = "update_report", html = m$message, add = TRUE)
        shinyjs::html(id = "update_report", html = "<br/>", add = TRUE)
      })
    }
  }
  
  observeEvent(input$button_long_update, {
    removeModal()
    withCallingHandlers({
      shinyjs::html("html", "")
      updating_text_data_app(
        target = input$request_year_page,
        api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
        TESTING = FALSE,
        RUN_API = TRUE,
        RUN_TRANSLATION = TRUE
      )
    },
    message = function(m) {
      shinyjs::html(id = "update_report", html = m$message, add = TRUE)
      shinyjs::html(id = "update_report", html = "<br/>", add = TRUE)
    })
  })
  
  observeEvent(input$run_update, { # if button update is pressed
    withCallingHandlers({ # redirect messages to output_report
      shinyjs::html("update_report", "")
      update_in_app(request_year_page = input$request_year_page)
    },
    message = function(m) {
      shinyjs::html(id = "update_report", html = m$message, add = TRUE)
      shinyjs::html(id = "update_report", html = "<br/>", add = TRUE)
    })
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


  
  
  
  