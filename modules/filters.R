# --- Filter UI for Explore tab ---

base_filter_choices <- c(
  "None" = "none",
  "Prize Name" = "prize_name",
  "Prize Year" = "prize_year", 
  "Literary Genre" = "prize_genre",
  "Ethnicity" = "ethnicity_macro", 
  "Gender" = "gender", 
  "Sexuality" = "sexuality"
)

observe({
  selected_var1 <- input$filter_var1
  updated_choices_var2 <- base_filter_choices[base_filter_choices != selected_var1 | base_filter_choices == "none"]
  updateSelectInput(session, "filter_var2", choices = updated_choices_var2, selected = input$filter_var2)
})

output$filter_value_ui1 <- renderUI({
  req(input$filter_var1)
  if (input$filter_var1 == "none") return(NULL)
  values <- prizes %>%
    filter(!is.na(.data[[input$filter_var1]])) %>%
    distinct(.data[[input$filter_var1]]) %>%
    pull()
  
  div(style = "margin-left: 20px;",
      selectInput("filter_value1", "Select value 1:", choices = sort(values))
  )
})

output$filter_value_ui2 <- renderUI({
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

filtered_data <- reactive({
  data <- prizes
  if (input$filter_var1 != "none" && !is.null(input$filter_value1)) {
    data <- data %>% filter(.data[[input$filter_var1]] == input$filter_value1)
  }
  if (input$filter_var2 != "none" && !is.null(input$filter_value2)) {
    data <- data %>% filter(.data[[input$filter_var2]] == input$filter_value2)
  }
  if (!is.null(input$result) && input$result != "all") {
    data <- data %>% filter(person_role == input$result)
  }
  data %>%
    select(name, prize_name, prize_year, prize_genre, person_role, book_title,
           gender, ethnicity_macro, sexuality, degree_institution, degree_field)
})

output$authors_table <- renderDT({
  req(filtered_data())
  datatable(filtered_data(),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE)
})

output$download_filtered <- downloadHandler(
  filename = function() {
    paste0("filtered_authors_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(filtered_data(), file, row.names = FALSE)
  }
)
