library(shiny)
library(DT)
library(plotly)
library(shinyjs)
library(dplyr)
library(readxl)
library(tools)
library(readr)
library(stringi)
source("logic/categoriser.R")
source("ui.R")
source("server.R")


classification.look.up <- read.csv("data/ViGOCat_classification.csv")

# Run the application 
shinyApp(ui = ui, server = server)
