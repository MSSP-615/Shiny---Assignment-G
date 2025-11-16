# app.R file for British Literary Prizes
# Madeleine Livaudais

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(RColorBrewer)


# Load UI and server
source("ui.R")
source("server.R")

# Run app
shinyApp(ui, server)
