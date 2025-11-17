library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(RColorBrewer)

output$total_prizes_box <- renderValueBox({
  valueBox(
    value = nrow(prizes),
    subtitle = "Total Prize Entries",
    icon = icon("trophy"),
    color = "yellow"
  )
})

output$winners_box <- renderValueBox({
  valueBox(
    value = nrow(prizes %>% filter(person_role == "winner")),
    subtitle = "Total Winners",
    icon = icon("crown"),
    color = "green"
  )
})

output$shortlisted_box <- renderValueBox({
  valueBox(
    value = nrow(prizes %>% filter(person_role == "shortlisted")),
    subtitle = "Shortlisted Authors",
    icon = icon("list"),
    color = "blue"
  )
})

output$genre_box <- renderValueBox({
  valueBox(
    value = n_distinct(prizes$prize_genre),
    subtitle = "Unique Genres",
    icon = icon("book-open"),
    color = "purple"
  )
})