# --- Gender Pie Chart ---
output$gender_plot <- renderPlotly({
  data <- prizes %>% filter(!is.na(gender)) %>% count(gender)
  colors <- brewer.pal(n = nrow(data), name = "Set2")
  plot_ly(
    data, labels = ~gender, values = ~n, type = 'pie',
    textinfo = 'label+percent', insidetextorientation = 'radial',
    hoverinfo = 'text', text = ~paste("Gender:", gender, "<br>Count:", n),
    marker = list(colors = colors)
  )
})

# --- Ethnicity Bar Chart ---
output$ethnicity_plot <- renderPlotly({
  data <- prizes %>% filter(!is.na(ethnicity_macro)) %>% count(ethnicity_macro)
  plot_ly(data, x = ~ethnicity_macro, y = ~n, type = 'bar',
          text = ~paste(n), hoverinfo = 'text', marker = list(color = '#0073B7')) %>%
    layout(xaxis = list(title = " ", tickangle = -45),
           yaxis = list(title = "Count"),
           margin = list(b = 100))
})

# --- British vs Non-British Plots ---
prizes$british_flag <- ifelse(
  prizes$ethnicity_macro == "White British" | prizes$uk_residence == "Yes",
  "British / UK-affiliated", "Other"
)

output$british_plot <- renderPlotly({
  dat <- prizes[!duplicated(prizes$person_id), ]
  if (input$british_role_filter != "All") dat <- dat[dat$person_role == input$british_role_filter, ]
  plot_data <- dat %>% count(british_flag)
  colors <- RColorBrewer::brewer.pal(n = nrow(plot_data), name = "Set2")
  plot_ly(plot_data, labels = ~british_flag, values = ~n, type = "pie",
          textinfo = "label+percent", insidetextorientation = "radial",
          hoverinfo = "text", text = ~paste("Group:", british_flag, "<br>Count:", n),
          marker = list(colors = colors))
})

output$british_plot_role <- renderPlotly({
  dat <- prizes[!duplicated(prizes$person_id), ]
  if (input$british_role_filter != "All") dat <- dat[dat$person_role == input$british_role_filter, ]
  plot_data <- dat %>% count(british_flag, person_role)
  plot_ly(plot_data, x = ~british_flag, y = ~n, color = ~person_role, colors = "Set2",
          type = "bar", text = ~paste("Role:", person_role, "<br>Count:", n),
          hoverinfo = "text") %>%
    layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Number of People"))
})

output$genre_plot <- renderPlotly({
  dat <- prizes
  
  if (input$genre_role != "All") {dat <- dat[dat$person_role == input$genre_role, ]}
  
  # Count each author once per prize
  dat <- dat[!duplicated(dat[c("prize_id", "person_id")]), ]
  var <- input$genre_variable
  
  plot_data <- dat %>%
    group_by(prize_genre, !!sym(var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(prize_genre) %>%
    mutate(percent = n / sum(n))
  
  p <- ggplot(plot_data, aes(x = prize_genre, y = percent, fill = !!sym(var))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Representation by Genre",
      x = "Genre",
      y = "Share of Authors",
      fill = var
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p)
})

output$eth_rep_plot <- renderPlotly({
  dat <- prizes
  
  if (input$eth_role != "All") {dat <- dat[dat$person_role == input$eth_role, ]}
  
  dat <- dat[!duplicated(dat[c("prize_year", "person_id")]), ]
  
  plot_data <- dat %>%
    group_by(prize_year, ethnicity_macro) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(prize_year) %>%
    mutate(percent = n / sum(n))
  
  p <- ggplot(plot_data, aes(x = prize_year, y = percent, color = ethnicity_macro)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Ethnicity Representation Over Time",
      x = "Year",
      y = "Share of Authors",
      color = "Ethnicity"
    ) +
    theme_minimal()
  
  ggplotly(p)
})


output$gender_rep_plot <- renderPlotly({
  dat <- prizes
  
  if (input$gender_role != "All") {dat <- dat[dat$person_role == input$gender_role, ]}
  
  dat <- dat[!duplicated(dat[c("prize_year", "person_id")]), ]
  
  plot_data <- dat %>%
    group_by(prize_year, gender) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(prize_year) %>%
    mutate(percent = n / sum(n))
  
  p <- ggplot(plot_data, aes(x = prize_year, y = percent, color = gender)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Gender Representation Over Time",
      x = "Year",
      y = "Share of Authors"
    ) +
    theme_minimal()
  
  ggplotly(p)
})

