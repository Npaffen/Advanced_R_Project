### This will control the dynamic server reactions to input.

## Structure
# 0. Preparation
# 1. Server Function
#     - Sidebar
#       - First Tab: Database Status
#       - Second Tab: Loaded Data Files
#       - Third Tab: Updating
#       - Fourth Tab: Plot article frequency per days
#       - Fifth Tab: Plot word frequencies
#     - Other
# 2. Try Updating

#################################################################
# 0. Preparation
library(shiny)
require(shinyjs)
require(ggplot2)
source("src/app/functions.R")
source("src/app/updating_text_data_app.R")
source("src/update_article_data.R")
source("src/app/process_articles.R")
source("src/app/create_dictionary.R")
source("src/ts_word_frequency.R")

function(input, output, session){
  
  ###############################################################
  # Sidebar

  ############################
  # First Tab: Database Status
  
  # show earliest article info
  try(minart <- min(processed_articles_2019_page_01_EN$date))
  if(exists("minart")){
    output$earliest_article <- renderValueBox({
      valueBox(
        minart,
        "Earliest Article in DB",
        icon = icon("list"),
        color = "purple"
      )
    })
  } else {
    output$earliest_article <- renderValueBox({
      valueBox(
        "NA",
        "Earliest Article in DB",
        icon = icon("list"),
        color = "purple"
      )
    })
    
  }
  
  # show newesta rticle info
  try(maxart <- max(processed_articles_2020_page_01_EN$date))
  if(exists("maxart")){
    output$newest_article <- renderValueBox({
      valueBox(
        maxart,
        "Newest Article in DB",
        icon = icon("list"),
        color = "yellow"
      )
    })
  } else {
    output$newest_article <- renderValueBox({
      valueBox(
        "NA",
        "Newest Article in DB",
        icon = icon("list"),
        color = "yellow"
      )
      })
    }
    

  
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
  
  # infobox
  output$updating_text <- renderValueBox({
    valueBox(
      paste(Sys.Date() - max(article_data_2020_page_01$date)),
      "days in 2020 ready for update",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  # instruction
  output$updating_description <- renderText(
    "Type out the year and page number you wish to update and press the button:
    Year-Page, e.g. '2020-01' without quotes. In case of unexpected behavior,
    remove processed files in /output and recompile. Removing the dictionary
    is discouraged, recompilation takes up to 8 hours and translation bandwith
    is limited. Best recommendation is re-downloading or reinstalling the app.
    For testing, remove a file and replace it with its .old equivalent."
  )
  
  # confirmation popup
  confirm_update <- function() {
    modalDialog(
      span('A long update is pending, do you have time?'),
      footer = tagList(
        modalButton("No"),
        actionButton("button_long_update", "Yes")
      )
    )
  }
  
  # runs update
  update_in_app <- function(request_year_page){ 
      year <- substr(request_year_page, 1, 4)
      page <- substr(request_year_page, 6, 7)
      request <- paste0("article_data_", year,
                        "_page_", page)
      # check existance of raw files and if needs update
      if(exists(request)){
        request <- eval(as.name(request))
        if((year == 2019 && max(request$date) == as.Date("2019-12-31")) |
           (Sys.Date() - max(request$date) == 0)){
          message(paste("Raw data is up to date! Exiting update."))
        } else if((year == 2019 && max(request$date) - as.Date("2019-12-31") > 7) |
                  (year == 2020 && Sys.Date() - max(request$date) > 7)){
          message(paste("Raw data is older than 1 week! Perform long update?"))
          showModal(confirm_update())
        } else{
          message(paste("Raw data is recent, will update."))
          updating_text_data_app(
            target = input$request_year_page,
            api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
            RUN_API = TRUE,
            RUN_TRANSLATION = TRUE
          )
        }
      } else {
        message(paste("Raw data is missing, please redownload \'article_data_\' ",
                      "file into /data folder or reinstall app"))
      }
      # check existance of dictionary and if needs creation
      if(!exists("dictionary")){
        message(paste("Missing /output/dictionary.rds file!",
                      "Creating from scratch in 5 seconds..."))
        Sys.sleep(5)
        updating_text_data_app(
          target = input$request_year_page,
          api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
          RUN_API = TRUE,
          RUN_TRANSLATION = TRUE
        )
      } else {
        message(paste("Dictionary file found."))
      }
      # check if processed files exists and need update
      request <- paste0("processed_articles_", year, 
                        "_page_", page)
      if(!exists(paste0(request, "_CN")) |
         !exists(paste0(request, "_EN"))){
        message(paste("Processed file(s) missing! Creating from scratch in 5 seconds..."))
        Sys.sleep(5)
        updating_text_data_app(
          target = input$request_year_page,
          api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
          RUN_API = TRUE,
          RUN_TRANSLATION = TRUE
        )
      } else {
        message(paste0("Processed files found."))
      }
  }
  
  # react to long update confirmation
  observeEvent(input$button_long_update, {
    withCallingHandlers({
      shinyjs::html("html", "")
      updating_text_data_app(
        target = input$request_year_page,
        api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
        RUN_API = TRUE,
        RUN_TRANSLATION = TRUE
      )
    },
    message = function(m) {
      shinyjs::html(id = "update_report", html = m$message, add = TRUE)
      shinyjs::html(id = "update_report", html = "<br/>", add = TRUE)
    })
  })
  
  # react to update button pressed
  observeEvent(input$run_update, {
    withCallingHandlers({
      shinyjs::html("html", "")
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
  
  
  ############################
  # Fourth Tab: Plot article frequency  per day
  
  
  output$word_freq1 <- renderPlot(word_freq1)
  output$word_freq2 <- renderPlot(word_freq2)
  output$word_freq3 <- renderPlot(word_freq3)
  
  output$wordfreqs <- renderUI({
    tabBox(title = "Words per Day",id= "wordfreqtab",
           tabPanel(freq_words[[1]], plotOutput("word_freq1")),
           tabPanel(freq_words[[2]], plotOutput("word_freq2")),
           tabPanel(freq_words[[3]], plotOutput("word_freq2"))
    )
  })
  
    
  # output plots in tabs
  

  
  
  ###########################
  # other
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}


  
  
  
  