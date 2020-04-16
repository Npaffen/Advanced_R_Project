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
#       - Sixth Tab: Chinese-English Dictionary
#       - Seventh Tab: Bugs
#     - Other
# 2. Try Updating

#################################################################
# 0. Preparation

library(shiny)
require(shinyjs)
require(ggplot2)
require("DT")
source("app/functions.R")
source("app/updating_text_data_app.R")
source("wrangling/update_article_data.R")
source("app/process_articles.R")
source("app/create_dictionary.R")
source("scraping/ts_word_frequency.R")


function(input, output, session){

  ### Sidebar ############################################################

  ### First Tab: Database Status #########################

  source("app/tab_1_status.R", local=TRUE)

  ### Second Tab: Loaded Data Files #########################

  output$loaded_out <- renderPrint({
    print(loaded_files)
  })

  output$text_loadedfiles <- renderText(
    " Restart app to reload files in the /output and /data folders."
  )

  ### Third Tab: Updating #########################


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
  output$updating_description <- renderText(paste(
    "Type year and page number to update and press the button: ",
    "Year-Page, e.g. '2020-02' without quotes. In case of unexpected behavior, ",
    "remove processed files in /output and recompile. Removing the dictionary ",
    "is discouraged, recompilation takes up to 8 hours and translation bandwith ",
    "is limited. It is recommended then to re-download or reinstall the app. ",
    "For testing, remove e.g. \\data\\article_data_2020_page_02.rds and replace it ",
    "with its .old equivalent."
  ))

  # confirmation popup
  confirm_update <- function() {
    modalDialog(
      span('A long update is pending, do you have time?'),
      footer = tagList(
        modalButton("No"),
        actionButton("button_long_update", "Yes")
      ),
      easyClose = TRUE,
    )
  }


  # react to long update confirmation
  observeEvent(input$button_long_update, {
    removeModal()
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
          RUN_TRANSLATION = TRUE,
          RUN_UPDATE = TRUE
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
        RUN_TRANSLATION = TRUE,
        RUN_UPDATE = TRUE
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
        RUN_TRANSLATION = TRUE,
        RUN_UPDATE = TRUE
      )
    } else {
      message(paste0("Processed files found."))
    }
  }

  # react to long update confirmation
  observeEvent(input$button_long_update, {
    removeModal()
    withCallingHandlers({
      shinyjs::html("html", "")
      updating_text_data_app(
        target = input$request_year_page,
        api_key ="trnsl.1.1.20200315T225616Z.880e92d51073d977.c51f6e74be74a3598a6cc312d721303abb5e846a",
        RUN_API = TRUE,
        RUN_TRANSLATION = TRUE,
        RUN_UPDATE = TRUE
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



  ### Fourth Tab: Plot article frequency  per day #########################

  source("app/tab_4_art_freq.R", local=TRUE)


  ### Fifth Tab: Plot word frequency  per day #########################

  # render plots with example words
  output$word_freq1 <- renderPlot(word_freq1)
  output$word_freq2 <- renderPlot(word_freq2)
  output$word_freq3 <- renderPlot(word_freq3)

  output$wordfreq_description <- renderText(paste(
    "Choose an expression to plot its daily frequency in the translated English articles.  ",
    "Keep in mind this is a rough translation and no substitute for looking at the Chinese ",
    "original texts with a working knowledge of the language. This version only allows single",
    "word searches, so to find 'Xi Jinping', please use 'Jinping' for now.",
    "Optional: Choose a different start date between 2019-01-01 and today. ",
    "Please allow some time for rendering. ",
    "The economic index is NASDAQ inds CNY (NQCN2000CNY), including around 200 Chinese industrial firms traded on the NASDAQ ",
    "more info on: indexes.nasdaqomx.com"
  ))

  #react to make plot button pressed
  observeEvent(input$make_plot, {
    req(input$make_plot)
    word_freq4 <<- render_word(word = input$request_wordfreq,
                               start_date = input$request_worddate)
    output$word_freq4 <- renderPlot(word_freq4)
  })

  # output example plots in tabs
  output$wordfreqs <- renderUI({
    tabBox(title = "Words per Day",id= "wordfreqtab",
           tabPanel(freq_words[[1]], plotOutput("word_freq1")),
           tabPanel(freq_words[[2]], plotOutput("word_freq2")),
           tabPanel(freq_words[[3]], plotOutput("word_freq3")),
           tabPanel(input$request_wordfreq, plotOutput("word_freq4"))
    )
  })

  ### Sixth Tab: Chinese-English Dictionary #########################
  dtbl <- readRDS(paste0(wdir, "/output/dictionary.rds"))
  output$dictionary_table <- DT::renderDT({dtbl})

  output$dictionary_desc <- renderText(paste(
     "Unique expressions identified using a NLP algorithm named 'Jieba'. ",
     "Translation updated through Yandex Translation API.",
     "Please refer to the Chinese for accuracy."
   ))

  ### Seventh Tab: Bugs #########################

  output$bugs_desc <- renderText(paste(
    "14.04.2020 ",
    "So the app currently 'forces a restart' after a successful update, i.e. ",
    "it crashes. But after a restart the update is preserved.",
    "16.04.2020 ",
    "www.quandl.com port 443 ERROR, stops the app from running, but goes away after a few trys."
  ))



  ### other #######################
  # required by Rinno

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

}





