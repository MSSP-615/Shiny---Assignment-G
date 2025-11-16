library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)

# Load dataset
prizes <- read.csv("prizes_final.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # Source modular components
  source("modules/filters.R", local = TRUE)
  source("modules/valueBoxes.R", local = TRUE)
  source("modules/plots.R", local = TRUE)
  source("modules/tables.R", local = TRUE)
  
}
