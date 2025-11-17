#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(shinyjs)
library(plotly)
library(readxl)
library(stringi)

groups <- c("cell adhesion",
            "cell cycle",
            "cell death",
            "cellular component organization",
            "ER/endosome/lysosome related process",
            "gene regulation",
            "immune system process",
            "metabolic process",
            "morphogenesis/development",
            "multicellular organismal process",
            "neural related process",
            "other",
            "protein modification/signaling",
            "receptor related process",
            "response to stimulus",
            "stress response",
            "transport",
            "viral related process"
)
colors <- c("#3ef5f0","#7030a0","#808080","#b4c7e7","#2e75b6","#f8cbad","#ff0000",
            "#ff00ff","#548235","#be73b8","#009999","#f2f2f2","#cccc00","#cfffc4","#ffff00",
            "#ff8000","#00ff00","#00b0f0"
)

source("functions.R")

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
