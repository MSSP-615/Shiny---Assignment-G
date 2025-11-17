library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(RColorBrewer)

# British vs Non-British Table
output$british_table <- renderTable({
  dat <- prizes
  if (input$british_role_filter != "All") dat <- dat[dat$person_role == input$british_role_filter, ]
  aggregate(person_id ~ british_flag, dat, function(x) length(unique(x)))
})

# Gender over time table
output$gender_rep_table <- renderTable({
  dat <- prizes
  if (input$gender_role != "All") dat <- dat[dat$person_role == input$gender_role, ]
  dat <- dat[!duplicated(dat[c("prize_year", "person_id")]), ]
  dat %>%
    group_by(prize_year, gender) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(prize_year) %>%
    mutate(percent = round(100 * n / sum(n), 1))
})

# Ethnicity over time table
output$eth_rep_table <- renderTable({
  dat <- prizes
  if (input$eth_role != "All") dat <- dat[dat$person_role == input$eth_role, ]
  dat <- dat[!duplicated(dat[c("prize_year", "person_id")]), ]
  dat %>%
    group_by(prize_year, ethnicity_macro) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(prize_year) %>%
    mutate(percent = round(100 * n / sum(n), 1))
})

# Genre table
output$genre_table <- renderTable({
  dat <- prizes
  if (input$genre_role != "All") dat <- dat[dat$person_role == input$genre_role, ]
  dat <- dat[!duplicated(dat[c("prize_id", "person_id")]), ]
  var <- input$genre_variable
  dat %>%
    group_by(prize_genre, !!sym(var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(prize_genre) %>%
    mutate(percent = round(100 * n / sum(n), 1))
})
