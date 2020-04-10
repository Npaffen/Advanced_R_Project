install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("parent_directory")

testthat::with_mock( 
       `usethis:::check_not_nested` = function(path, name) NULL,
       create_package("Chinese.TM.App", rstudio = FALSE, open = FALSE)
   ) #forces R to create the package in the top-level of the project, ignoring any warning