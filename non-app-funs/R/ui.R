
# This will create the user interface.

## Structure
# 0. Preparation
# 1. create parts
#     - Sidebar
#     - Body
#       - First Tab: Database Status
#       - Second Tab: Loaded Data Files
#       - Third Tab: Updating
#       - Fourth Tab: Descriptives
#       - Fifth Tab: Plot word frequencies
#       - Sixth Tab: Chinese-English Dictionary
#       - Seventh Tab: Bugs
#     - Other
# 2. join parts



#################################################################
# 0. Preparation

library(shinydashboard)
library(shiny)
library(shinyjs)
# require("DT")


#################################################################
# 1. Preparation
# create parts

##############################
# sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Data Status", tabName = "data_status", icon = icon("dashboard")),
      menuItem("Loaded Files", tabName = "loadedfiles", icon = icon("dashboard")),
      menuItem("Updating", tabName = "updating", icon = icon("dashboard")),
      menuItem("Article Frequencies", tabName = "artfreq", icon = icon("dashboard")),
      menuItem("Word Frequencies", tabName = "wordfreq", icon = icon("dashboard")),
      menuItem("Dictionary", tabName = "dict", icon = icon("dashboard")),
      menuItem("Bugtracker", tabName = "bugs", icon = icon("dashboard"))
    )
  )

##############################
# body
body <-   dashboardBody(
    tabItems(

      ##############################
      # First tab content
      tabItem(tabName = "data_status",
              h2("Data Status"),
              fluidRow(
                valueBoxOutput("earliest_article"),
                valueBoxOutput("newest_article")
              ),
              fluidRow(
                column(7,
                       tableOutput("database_status")
                )
              )
      ),

      ##############################
      # Second tab content
      tabItem(tabName = "loadedfiles",
              fluidRow(
                textOutput("text_loadedfiles")
              ),
              h2("Loaded Files"),
              fluidRow(
                verbatimTextOutput("loaded_out")
              )
      ),


      ##############################
      # Third tab content

      tabItem(tabName = "updating",
              h2("Updating Text and Economic Data"),
              fluidRow(
                valueBoxOutput("updating_text")
              ),
              fluidRow(
                textOutput("updating_description"),
                textInput("request_year_page","","2020-02"),
                shinyjs::useShinyjs(),
                actionButton("run_update","run update")
                ),
              fluidRow(
                verbatimTextOutput("update_report")
              )
      ),


      ##############################
      # Fourth tab content
      tabItem(tabName = "artfreq",
              fluidRow(
                textOutput("artfreq_desc"),
                uiOutput("artfreqs")
                )
      ),


      ##############################
      # Fifth tab content
      tabItem(tabName = "wordfreq",
              fluidRow(
                uiOutput("wordfreqs"),
                textOutput("wordfreq_description"),
                textInput("request_wordfreq", "", "committee"),
                textInput("request_worddate", "", "2019-01-01"),
              ),
              fluidRow(
                shinyjs::useShinyjs(),
                actionButton("make_plot","make plot"),
                plotOutput("wordfreqs_dyn")
              )
      ),

      ##############################
      # Sixth tab content
      tabItem(tabName = "dict",
              h2("Chinese-English Dictionary"),
              fluidRow(
                textOutput("dictionary_desc"),
                DT::DTOutput("dictionary_table")
              )
      ),

      ##############################
      # Seventh tab content
      tabItem(tabName = "bugs",
              h2("Bugtracker"),
              fluidRow(
                textOutput("bugs_desc")
              )
      )
    )
  )


########################################################
# join parts

dashboardPage(
  dashboardHeader(title = "People's Daily Mining"),
  sidebar,
  body
)
