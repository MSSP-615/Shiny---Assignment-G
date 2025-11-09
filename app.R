# app.R file for British Literary Prizes

library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(shinythemes)

# Load the dataset 
prizes <- read.csv("prizes.csv", stringsAsFactors = FALSE)

# script the user interface 
ui <- navbarPage( "British Literary Prizes Dashboard",
                  
                  theme = shinytheme("lumen"),
                  
                  # Information Tab
                  tabPanel("Information",
                           p("Data regarding British Literary Prizes", style = "font-size: 30px;"), 
                           p('Here you are able to filter through a dataset and learn about the nine major literary prizes in
                             the U.K from 1990 to 2022. Below is a decription of the dataset from the source: ', style = "font-size: 16px;"), 
                           p('The prizes in this dataset were included based on their prestige, longevity, and an eye towards generic
                             representation of various kinds of fiction, poetry, drama, and non-fiction. The nine major literary prizes are:'),
                           tags$ul(
                             tags$li('The James Tait Black Memorial Prize (Fiction and Drama)'), 
                             tags$li('Costa Book Award (Book of the Year, Novel, First Novel, Poetry, Biography, Childrens)'), 
                             tags$li('The Booker Prize'),
                             tags$li('Women’s Prize for Fiction'),
                             tags$li('The Gold Dagger Award'),
                             tags$li('The British Science Fiction Association Award'),
                             tags$li('The T.S. Eliot Prize for Poetry'),
                             tags$li('The Ted Hughes Award for New Work in Poetry'),
                             tags$li('The Baillie Gifford Prize for Non-Fiction')
                           ),
                           p('This dataset contains primary categories of information on individual authors comprising gender, sexuality, 
                             UK residency, ethnicity, geography and details of educational background, including institutions where the 
                             authors acquired their degrees and their fields of study.')
                  ),
                  
                  # Explore the Data Tab
                  tabPanel("Explore the Authors",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Filter Authors by: "),
                               
                               # Buttons: Winner vs Shortlisted
                               radioButtons("result", "Select award result:",
                                            choices = c("Prize Winners" = "winner",
                                                        "Shortlisted for Prize" = "shortlisted"),
                                            inline = TRUE), 
                               
                               # Filter type for Variable 1
                               selectInput("filter_var1", "Choose a filter variable:",
                                           choices = c("Prize Name" = "prize_name",
                                                       "Prize Year" = "prize_year", 
                                                       "Literary Genre" = "prize_genre",
                                                       "Ethnicity" = "ethnicity_macro", 
                                                       "Gender" = "gender", 
                                                       "Sexuality" = "sexuality")),
                               
                               # Dynamic UI: the specific value (updated based on filter_var1)
                               uiOutput("filter_value_ui1"),
                               
                               # Add a second filter type for Variable 2
                               selectInput("filter_var2", "Choose a second filter variable (optional):",
                                           choices = c("None" = "none",
                                                       "Prize Name" = "prize_name",
                                                       "Prize Year" = "prize_year", 
                                                       "Literary Genre" = "prize_genre",
                                                       "Ethnicity" = "ethnicity_macro", 
                                                       "Gender" = "gender", 
                                                       "Sexuality" = "sexuality")),
                               
                               # Dynamic UI: the specific value (updated based on filter_var2)
                               uiOutput("filter_value_ui2")
                             ),
                             
                             mainPanel(
                               h4("Results after filtering:"),
                               DTOutput("authors_table")
                             )
                           )
                  ),
                  
                  tabPanel("Page 2",
                           fluidPage(
                             h3("Analysis 1"),
                             p("Address some questions such as if some ethnicity has more advantage over other? does some institution gives more awards than others?
                                 Is it becaause it is easy to get award in that institution or simply it is more famous? You can add information from outside ")
                           )
                  ),
                  
                  tabPanel("Page 3",
                           fluidPage(
                             h3("Analysis 2"),
                             p("Some more analysis can be added along with visualizations")
                           )
                  )
)

server <- function(input, output, session) {
  
  # Base choices for the filter variables
  base_filter_choices <- c(
    "None" = "none",
    "Prize Name" = "prize_name",
    "Prize Year" = "prize_year", 
    "Literary Genre" = "prize_genre",
    "Ethnicity" = "ethnicity_macro", 
    "Gender" = "gender", 
    "Sexuality" = "sexuality"
  )
  
  # Observe changes in filter_var1 and update choices for filter_var2
  observe({
    # Determine which variable is currently selected in filter_var1
    selected_var1 <- input$filter_var1
    
    # Create the new choices list for filter_var2 by removing the selected_var1
    # We also keep 'None' in the list of choices
    updated_choices_var2 <- base_filter_choices[base_filter_choices != selected_var1 | base_filter_choices == "none"]
    
    # Update the filter_var2 input
    updateSelectInput(session, "filter_var2",
                      choices = updated_choices_var2,
                      selected = input$filter_var2 # Keep current selection if possible
    )
  })
  
  # Dynamically update filter values based on selected variable 1
  output$filter_value_ui1 <- renderUI({
    req(input$filter_var1)
    values <- prizes %>%
      filter(!is.na(.data[[input$filter_var1]])) %>%
      distinct(.data[[input$filter_var1]]) %>%
      pull()
    
    div(style = "margin-left: 20px;",
        selectInput("filter_value1", "Select value 1:", choices = sort(values))
    )
  })
  
  # Dynamically update filter values based on selected variable 2
  output$filter_value_ui2 <- renderUI({
    # Check if a second filter is actually selected before rendering UI
    if (input$filter_var2 != "none") {
      values <- prizes %>%
        filter(!is.na(.data[[input$filter_var2]])) %>%
        distinct(.data[[input$filter_var2]]) %>%
        pull()
      
      div(style = "margin-left: 20px;",
          selectInput("filter_value2", "Select value 2:", choices = sort(values))
      )
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$filter_var1, input$filter_value1, input$result)
    
    data <- prizes %>%
      filter(
        .data[[input$filter_var1]] == input$filter_value1,
        person_role == input$result
      )
    
    if (input$filter_var2 != "none" && !is.null(input$filter_value2)) {
      data <- data %>%
        filter(.data[[input$filter_var2]] == input$filter_value2)
    }
    
    data %>%
      select(name, prize_name, prize_year, prize_genre, person_role, book_title,
             gender, ethnicity_macro, sexuality, degree_institution, degree_field)
  })
  
  # Render table
  output$authors_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
}

shinyApp(ui, server)

