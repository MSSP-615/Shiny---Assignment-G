library(shiny)
library(shinydashboard)

# Load your dataset
prizes <- read.csv("prizes_final.csv", stringsAsFactors = FALSE)

ui <- dashboardPage(
  
  dashboardHeader(title = "U.K. Book Prizes"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Story", tabName = "info", icon = icon("info-circle")),
      menuItem("Explore the Data", tabName = "explore", icon = icon("table")),
      menuItem("British vs Non-British Writers", tabName = "british_nonbritish", icon = icon("flag")),
      menuItem("Gender Over Time", tabName = "gender_representation", icon = icon("venus-mars")),
      menuItem("Ethnicity Over Time", tabName = "ethnicity_representation", icon = icon("users")),
      menuItem("Genre Representation", tabName = "genre_rep", icon = icon("layer-group"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ----- Information Tab -----
      tabItem(tabName = "info",
              fluidRow(
                box(width = 12, title = "British Literary Prizes from 1990 to 2022", solidHeader = TRUE, status = "primary",
                    tags$p("Here you are able to filter through a dataset and learn about the nine major literary prizes in the U.K 
                      from 1990 to 2022. The prizes in this dataset were included based on their prestige, longevity, and an 
                      eye towards generic representation of various kinds of fiction, poetry, drama, and non-fiction. The nine 
                      major literary prizes are:"),
                    tags$ul(
                      tags$li("The James Tait Black Memorial Prize (Fiction and Drama)"),
                      tags$li("Costa Book Award (Book of the Year, Novel, First Novel, Poetry, Biography, Childrens)"),
                      tags$li("The Booker Prize"),
                      tags$li("Women’s Prize for Fiction"),
                      tags$li("The Gold Dagger Award"),
                      tags$li("The British Science Fiction Association Award"),
                      tags$li("The T.S. Eliot Prize for Poetry"),
                      tags$li("The Ted Hughes Award for New Work in Poetry"),
                      tags$li("The Baillie Gifford Prize for Non-Fiction")
                    ),
                    tags$p("This dataset contains primary categories of information on individual authors comprising gender, sexuality, 
                      UK residency, ethnicity, geography and details of educational background, including institutions where the authors 
                      acquired their degrees and their fields of study."), 
                    tags$p("Data source:", tags$a(href = "https://data.post45.org/posts/british-literary-prizes/", 
                                                  "Post 45 Data Collective"))
                )
              ), 
              
              fluidRow(
                box(width = 12, title = "First Look", solidHeader = TRUE, status = "primary",
                    fluidRow(
                      valueBoxOutput("total_prizes_box", width = 3),
                      valueBoxOutput("winners_box", width = 3),
                      valueBoxOutput("shortlisted_box", width = 3), 
                      valueBoxOutput("genre_box", width = 3)
                    )
                )
              ), 
              
              fluidRow(
                box(width = 12, title = "Demographic Variable Breakdowns", solidHeader = TRUE, status = "primary",
                    fluidRow(
                      box(width = 6, title = "Gender Distribution", solidHeader = TRUE, status = "info",
                          plotlyOutput("gender_plot", height = "350px")
                      ),
                      box(width = 6, title = "Ethnicity Distribution", solidHeader = TRUE, status = "info",
                          plotlyOutput("ethnicity_plot", height = "350px")
                      )
                    )
                )
              )
      ),
      
      # ----- Explore Tab -----
      tabItem(tabName = "explore",
              fluidRow(
                box(width = 12, title = "Filter Authors by: ", solidHeader = TRUE, status = "info",
                    radioButtons("result", "Select award result:",
                                 choices = c("All" = "all", 
                                             "Prize Winners" = "winner",
                                             "Shortlisted for Prize" = "shortlisted"),
                                 inline = TRUE),
                    
                    selectInput("filter_var1", "Choose a filter variable:",
                                choices = c("None" = "none",
                                            "Prize Name" = "prize_name",
                                            "Prize Year" = "prize_year", 
                                            "Literary Genre" = "prize_genre",
                                            "Ethnicity" = "ethnicity_macro", 
                                            "Gender" = "gender", 
                                            "Sexuality" = "sexuality"),
                                selected = "none"),
                    
                    uiOutput("filter_value_ui1"),
                    
                    selectInput("filter_var2", "Choose a second filter variable (optional):",
                                choices = c("None" = "none",
                                            "Prize Name" = "prize_name",
                                            "Prize Year" = "prize_year", 
                                            "Literary Genre" = "prize_genre",
                                            "Ethnicity" = "ethnicity_macro", 
                                            "Gender" = "gender", 
                                            "Sexuality" = "sexuality")),
                    
                    uiOutput("filter_value_ui2")
                ),
              ),
              fluidRow(
                box(width = 12, title = "Resulting Data Table: ", solidHeader = TRUE, status = "primary",
                    DTOutput("authors_table"),
                    br(), br(),
                    downloadButton("download_filtered", "Download Filtered Data",
                                   style = "background-color:#28a745; color:white; font-weight:bold; border:none;")
                )
              ),
      ),
      
      # ----- British vs Non-British Tab -----
      tabItem(tabName = "british_nonbritish",
        fluidRow(
          box(width = 12, title = "British vs Non-British Writers", solidHeader = TRUE, status = "warning",
            p("This analysis shows how many British/UK-affiliated writers versus non-British writers have been shortlisted or awarded UK book prizes."),
            
            selectInput("british_role_filter", "Filter by Prize Role:",
              choices = c("All", unique(prizes$person_role)), selected = "All"),
            
            tableOutput("british_table"))
        ),
        
        fluidRow(
          box(width = 6, title = "Overall British vs Non-British Unique Authors", solidHeader = TRUE, status = "primary",
            plotlyOutput("british_plot", height = "350px")),
          
          box(width = 6, title = "British vs Non-British Unique Authors by Prize Role", solidHeader = TRUE, status = "primary",
            plotlyOutput("british_plot_role", height = "350px"))
        )
      ), 
      
      # ---------- Gender Representation -----------
      tabItem(
        tabName = "gender_representation",
        
        fluidRow(
          box(width = 12, title = "Trends in Gender Representation Over Time", solidHeader = TRUE, status = "warning",
            p("How has gender representation changed among shortlisted authors and prize winners over time?"),
            selectInput("gender_role", "Prize Role:",
              choices = c("All", unique(prizes$person_role)), selected = "All"))
        ),
        
        fluidRow(
          box(width = 12, title = "Gender Representation Over Time", solidHeader = TRUE, status = "primary",
            plotlyOutput("gender_rep_plot", height = "400px"))
        ),
        
        fluidRow(
          box(width = 12, title = "Gender Summary Table", solidHeader = TRUE, status = "primary",
            tableOutput("gender_rep_table"))
        )
      ),
      
      # ----- Ethnicity Representation -----
      tabItem(tabName = "ethnicity_representation",
        
        fluidRow(
          box(width = 12, title = "Trends in Ethnic Representation Over Time", solidHeader = TRUE, status = "warning",
            p("How has ethnic diversity changed among shortlisted authors and prize winners over time?"),
            selectInput("eth_role", "Prize Role:",
              choices = c("All", unique(prizes$person_role)), selected = "All"))
        ),
        
        fluidRow(
          box(width = 12, title = "Ethnicity Representation Over Time", solidHeader = TRUE, status = "primary",
            plotlyOutput("eth_rep_plot", height = "400px"))
        ),
        
        fluidRow(
          box(width = 12, title = "Ethnicity Summary Table", solidHeader = TRUE, status = "primary",
            tableOutput("eth_rep_table"))
        )
      ),
      
      
      # ----- Genre Representation -----
      tabItem( tabName = "genre_rep",
        
       fluidRow(
          box(width = 12, title = "Representation by Genre", solidHeader = TRUE, status = "warning",
            p("Which genres tend to shortlist or award women and ethnically diverse writers? 
                  Use the controls to explore representation patterns by gender and ethnicity 
                  across literary prize genres."),
            selectInput("genre_variable", "Representation Variable:",
              choices = c("Gender" = "gender", "Ethnicity" = "ethnicity_macro")),
            selectInput("genre_role", "Prize Role:",
              choices = c("All", unique(prizes$person_role)), selected = "All")
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Genre Representation Plot", solidHeader = TRUE, status = "primary",
            plotlyOutput("genre_plot", height = "450px"))
        ),
        
        fluidRow(
          box(width = 12, title = "Representation by Genre (Table)", solidHeader = TRUE, status = "primary",
              tableOutput("genre_table"))
        )
      )
      
    )
  )
)
