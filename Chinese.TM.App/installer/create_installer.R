### Use RInno to create an installer that doesn't require an R installation!
## this works only on Windows :(

GET <- FALSE # set if you need to install Inno and RInno
if(GET) {
  # Get remotes package
  install.packages("remotes")
  require(remotes)
  # Use install_github to get RInno
  install_github("ficonsulting/RInno")
  # Require Package
  require(RInno)
  # Use RInno to get Inno Setup
  cat("install_inno() this used to work, ",
    "now you can install the Inno Package Manager for Windows from ",
    "https://jrsoftware.org/isdl.php#stable")
}

require(RInno)
require(remotes)

### Create the app!

create_app(
  app_name = "Chinese.TM.App",
  app_dir = getwd(),
  pkgs = c("dplyr", "jiebaR", "magick", "RSelenium",
           "purrr", "stringr", "tidytext", "readr",
           "tidyr", "lubridate", "ggplot2", "Quandl",
           "fredr", "shiny", "shinydashboard", "shinyjs",
           "tibble", "Rwordseg", "stopwords", "quanteda",
           "httr", "xml2", "rvest", "glue", "tidyverse",
           "here", "fs", "rlist"),
  include_R = TRUE, # hope to fix compat issues,

  )

compile_iss()

