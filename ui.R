
# This will create the user interface.

## Structure
# 0. Preparation
# 1. create parts
#     - Sidebar
#     - Body
#       - First Tab: Database Status
#       - Second Tab: Loaded Data Files
#       - Third Tab: Updating
#     - Other
# 2. join parts



#################################################################
# 0. Preparation

library(shinydashboard)
library(shiny)



#################################################################
# 1. Preparation
# create parts

##############################
# sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Data Status", tabName = "data_status", icon = icon("dashboard")),
      menuItem("Loaded Files", tabName = "loadedfiles", icon = icon("dashboard")),
      menuItem("Updating", tabName = "updating", icon = icon("dashboard"))
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
