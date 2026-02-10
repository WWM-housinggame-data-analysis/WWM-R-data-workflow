# Load necessary libraries ----

## Load for handling file location
library(here)

## Load importing/exporting data
library(readxl)
library(readr)
library(openxlsx)
library(writexl)

## Load for data manipulation
library(sqldf)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)

## Load for data visualisation
library(ggplot2)
library(ggtext)
library(shiny)
library(bslib)
library(plotly)


# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here(file.path(FUNCTION_PATH, "constants.R")))


# Source files ----

## Load required functions

### Load functions required for listing, uploading and exporting data
source(here(file.path(FUNCTION_PATH, "list-upload-export-dbtables.R")))

### Load function containing the preprocessing of data tables coming from the database (i.e. formatting existingm adding existing or calculating new columns)
source(here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))

### Load function containing the transformation of data tables to summary tables (i.e. dropping columns and aggregate tables)
source(here(file.path(FUNCTION_PATH, "table-data.R")))

### Load function containing the transformation of data tables to fit the format required for GP1 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here(file.path(FUNCTION_PATH, "prepare-GP1-data.R")))

### Load functions required to handle dashboard filter actions
source(here(file.path(FUNCTION_PATH, "interact-data.R")))

### Load functions required to setup plotly visualizations
source(here(file.path(FUNCTION_PATH, "create-GP1-plot.R")))


# Data Workflow ----

## Read all tables in the database folders into a single list variable:
##
## list(gamesession_data_list)
##  |
##  |-- list(gamessession_data_session1)
##  |     |
##  |     |-- df(table1)
##  |     |-- df(table2)
##  |     |-- df(table3)
##  |     ...
##  |
##  |-- list(gamessession_data_session2)
##  |     |
##       ...
##  ...
##

gamesession_data_list <- upload_dbtables(RAWDATA_PATH, "housinggame", excel = TRUE, selection = TRUE)

income_dist_list <- list()

for (session_path in names(gamesession_data_list)) {
  income_dist_list[[session_path]] <- preprocess_dbtables(gamesession_data_list[[session_path]])
}

gamesession_paths <- names(income_dist_list)
gamesession_names <- sapply(strsplit(names(income_dist_list), split = "/", fixed = TRUE), function(parts) tail(parts, 1))
names(gamesession_paths) <- gamesession_names

# Shiny App ----

ui <- page_navbar(
  title = "WhereWeMove Dashboard",
  navbar_options = navbar_options(bg = "#2D89C8",
                                  theme = "dark"),
  
  nav_panel(
    title = "Game Play",
    page_sidebar(
      sidebar = sidebar(
        title = "Choices and effects",
        bg = "white",
        accordion(
          multiple = FALSE,   # only one open at a time
          
          accordion_panel("1: Select Game Session",
                          
                          # Input + small reset button
                          layout_columns(col_widths = c(9, 3),
      
                          selectInput("selected_gamesession", "Session:",
                                      names(gamesession_paths),
                                      selected = "housinggame_session_20_251007_VerzekeraarsMasterClass"),
                          
                          actionButton("reset_session", "Reset", class = "btn-outline-secondary btn-sm")
                          )
          ),
          
          accordion_panel("2: Select Table",
                          
                          layout_columns(col_widths = c(9, 3),
                          
                          selectInput("selected_table", "Table:",
                                         c("All", as.character(unique(income_dist_list[[gamesession_paths[names(gamesession_paths) %in% "housinggame_session_20_251007_VerzekeraarsMasterClass"]]][["income_dist_df"]]$group_name))),
                                         selected = "All"),
                          actionButton("reset_table", "Reset", class = "btn-outline-secondary btn-sm")
                          )
          ),
          
          accordion_panel("3: Where players live"),
          
          accordion_panel("4: Player spending",
                          
                          
                          # checkboxGroupInput and its reset
                          layout_columns(col_widths = c(9, 3),
                                         
                                         checkboxGroupInput("bar_segment", "Cost_Types:",
                                                            choices = c("All", names(EXPENSE_BARCOLS)),
                                                            selected = "All"),
                                         actionButton("reset_cost", "Reset", class = "btn-outline-secondary btn-sm")
                          )
          ),
          
          accordion_panel("5: Selected measures"),
          accordion_panel("6: Flood in gameplay"),
          accordion_panel("7: Damage & satisfaction")
        ),
        
        
        # Optional: a global reset all button for the whole sidebar
        div(
          class = "mt-3",
          actionButton("reset_all_filters", "Reset all filters", class = "btn-warning")
        )
        
      ),
      
      mainPanel(width = 10,
        accordion(
          open = c("All Rounds"),
          accordion_panel(
            "All Rounds",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("plot_all"), verbatimTextOutput("debug")),
                        tabPanel("Summary", verbatimTextOutput("summary_all")),
                        tabPanel("Table", tableOutput("table_all"))
            )
          ),
          accordion_panel(
            "Round 1",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("plot_r1")),
                        tabPanel("Summary", verbatimTextOutput("summary_r1")),
                        tabPanel("Table", tableOutput("table_r1"))
            )
          ),
          accordion_panel(
            "Round 2",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("plot_r2")),
                        tabPanel("Summary", verbatimTextOutput("summary_r2")),
                        tabPanel("Table", tableOutput("table_r2"))
            )
          ),
          accordion_panel(
            "Round 3",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("plot_r3")),
                        tabPanel("Summary", verbatimTextOutput("summary_r3")),
                        tabPanel("Table", tableOutput("table_r3"))
            )
          )
        )
      )
    )
  ),
  
  
  
  nav_panel(title = "Game Settings", p("First page content.")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("About WhereWeMove", href = "https://seriousgaming.tudelft.nl/games/")),
    nav_item(tags$a("WhereWeMove info", href = "https://pure.tudelft.nl/ws/portalfiles/portal/180909041/WhereWeMove-Brochure_Final.pdf")),
    nav_item(tags$a("Facilitator website", href = "https://housing-game.tbm.tudelft.nl/housinggame-facilitator/jsp/facilitator/login.jsp")),
    nav_item(tags$a("Player website", href = "https://housing-game.tbm.tudelft.nl/housinggame-player/jsp/player/login.jsp"))
  )
)





server <- function(input, output, session) {
  
  # Reset only the session selectInput
  observeEvent(input$reset_session, {
    # Clear to empty; for selectize inputs, character(0) or NULL works
    updateSelectInput(session, "selected_gamesession", selected = "housinggame_session_20_251007_VerzekeraarsMasterClass")
  })
  
  # Reset only the table selectInput
  observeEvent(input$reset_table, {
    updateSelectInput(session, "selected_table", selected = "All")
  })
  
  # Reset only the checkboxGroupInput
  observeEvent(input$reset_cost, {
    # If your "All" is a semantic choice, reselect it:
    updateCheckboxGroupInput(session, "bar_segment", selected = "All")
  })
  
  # Optional: global "Reset all filters"
  observeEvent(input$reset_all_filters, {
    updateSelectInput(session, "selected_gamesession", selected = "housinggame_session_20_251007_VerzekeraarsMasterClass")
    updateSelectInput(session, "selected_table",      selected = "All")
    updateCheckboxGroupInput(session, "bar_segment",    selected = "All")
  })

  
  income_dist_df <- reactive({income_dist_list[[gamesession_paths[names(gamesession_paths) %in% input$selected_gamesession]]][["income_dist_df"]]})
  
  required_tables <- reactive({as.character(unique(income_dist_df()$group_name))})
  
  selected_table <- reactive({filter_selected_categs(input$selected_table, required_tables())})
  
  selected_bar_segments <- reactive({filter_selected_categs(input$bar_segment, names(EXPENSE_BARCOLS))})
  
  selected_columns <- reactive({EXPENSE_BARCOLS[names(EXPENSE_BARCOLS) %in% selected_bar_segments()]})

  summary_df <- reactive({retrieve_summary_table(income_dist_df(), selected_table())})
  
  
  GP1_plotall_data <- reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "All", fill_values_all) })
  GP1_plot1_data <- reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "1", fill_values_all) })
  GP1_plot2_data <- reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "2", fill_values_all) })
  GP1_plot3_data <- reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "3", fill_values_all) })
  
  # Connect plots
  output$plot_all <- renderPlotly({ create_GP1_plotly(GP1_plotall_data()) })
  output$plot_r1  <- renderPlotly({ create_GP1_plotly(GP1_plot1_data()) })
  output$plot_r2  <- renderPlotly({ create_GP1_plotly(GP1_plot2_data()) })
  output$plot_r3  <- renderPlotly({ create_GP1_plotly(GP1_plot3_data()) })
  
  # Summaries (update based on color_by choice)
  output$summary_all <- renderPrint({ summary(summary_df()) })
  output$summary_r1  <- renderPrint({ summary(summary_df()) })
  output$summary_r2  <- renderPrint({ summary(summary_df()) })
  output$summary_r3  <- renderPrint({ summary(summary_df()) })
  
  # Tables (update based on color_by choice)
  output$table_all <- renderTable({ summary_df() })
  output$table_r1  <- renderTable({ summary_df() })
  output$table_r2  <- renderTable({ summary_df() })
  output$table_r3  <- renderTable({ summary_df() })
  
  observe({
    updateSelectInput(session, "selected_table",
                      choices = c("All", as.character(unique(income_dist_list[[gamesession_paths[names(gamesession_paths) %in% input$selected_gamesession]]][["income_dist_df"]]$group_name))),
    )})
}

shinyApp(ui, server)