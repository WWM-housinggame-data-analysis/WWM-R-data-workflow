# Load necessary libraries ----
library(readxl)
library(readr)
library(openxlsx)

## Load for database manipulation
library(sqldf)

## Load for data manipulation
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)

## Load for excel manipulation
library(writexl)

## Load for data visualisation
library(ggplot2)
library(ggtext)

library(here)
library(shiny)
library(bslib)
library(plotly)

# Set defaults ----
# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Source files ----

# Get the path of the current script
## when you open Rstudio by clinking on .Rproj, default working directory is folder where .Rproj is stored
getwd()

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "list-upload-export-dbtables.R")))
source(here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here(file.path(FUNCTION_PATH, "plot-data.R")))
source(here(file.path(FUNCTION_PATH, "table-data.R")))
source(here(file.path(FUNCTION_PATH, "interact-data.R")))
source(here(file.path(FUNCTION_PATH, "render-plots.R")))
source(here(file.path(FUNCTION_PATH, "prepare-visualize-GP1.R")))


# Data Workflow ----

# Read all tables in the database folder to create accordingly the dataframe tables inside list
gamesession_data_list <- upload_selected_dbtables(RAWDATA_PATH, "housinggame")

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
  bg = "#2D89C8",
  inverse = TRUE,
  
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
                                         
                                         checkboxGroupInput("cost_type", "Cost_Types:",
                                                            choices = c("All", EXPENSE_BARCOLS),
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
      
      mainPanel(
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
    updateCheckboxGroupInput(session, "cost_type", selected = "All")
  })
  
  # Optional: global "Reset all filters"
  observeEvent(input$reset_all_filters, {
    updateSelectInput(session, "selected_gamesession", selected = "housinggame_session_20_251007_VerzekeraarsMasterClass")
    updateSelectInput(session, "selected_table",      selected = "All")
    updateCheckboxGroupInput(session, "cost_type",    selected = "All")
  })

  
  income_dist_reactive <- reactive({income_dist_list[[gamesession_paths[names(gamesession_paths) %in% input$selected_gamesession]]][["income_dist_df"]]})
  
  required_tables <- reactive({as.character(unique(income_dist_reactive()$group_name))})
  
  selected_table <- reactive({filter_selected_categs(input$selected_table, required_tables())})
  
  selected_costtypes <- reactive({filter_selected_categs(input$cost_type, EXPENSE_BARCOLS)})
  

  # Reactive dataset grouped by the chosen color_by variable

  
  grouped_data <- reactive({group_summary_table(income_dist_reactive(), selected_table())})
  
  
  gg_plot <- reactive({prepare_visualize_GP1(income_dist_reactive(), selected_costtypes(), selected_table(), game_round = "All", fill_values_all, fill_labels_all)})
  gg_plot1 <- reactive({prepare_visualize_GP1(income_dist_reactive(), selected_costtypes(), selected_table(), game_round = "1", fill_values_all, fill_labels_all)})
  gg_plot2 <- reactive({prepare_visualize_GP1(income_dist_reactive(), selected_costtypes(), selected_table(), game_round = "2", fill_values_all, fill_labels_all)})
  gg_plot3 <- reactive({prepare_visualize_GP1(income_dist_reactive(), selected_costtypes(), selected_table(), game_round = "3", fill_values_all, fill_labels_all)})
  
  # Connect plots
  
  output$debug <- renderPrint({
    unique(vapply(ggplotly(gg_plot$plot)$plt$x$data, function(tr) tr$name %||% "", character(1)))
  })
  
  output$plot_all <- renderPlotly({render_plots(gg_plot())})
  output$plot_r1  <- renderPlotly({render_plots(gg_plot1())})
  output$plot_r2  <- renderPlotly({render_plots(gg_plot2())})
  output$plot_r3  <- renderPlotly({render_plots(gg_plot3())})
  
  
  # Optional: inspect reactive rows
  output$debug <- renderPrint({
    paste(
      paste("Rows:", nrow(income_dist_reactive())),
      paste("Costs:", length(selected_table())),
      sep = "\n")
  })
  
  # Summaries (update based on color_by choice)
  output$summary_all <- renderPrint({ summary(grouped_data()) })
  output$summary_r1  <- renderPrint({ summary(grouped_data()) })
  output$summary_r2  <- renderPrint({ summary(grouped_data()) })
  output$summary_r3  <- renderPrint({ summary(grouped_data()) })
  
  # Tables (update based on color_by choice)
  output$table_all <- renderTable({ grouped_data() })
  output$table_r1  <- renderTable({ grouped_data() })
  output$table_r2  <- renderTable({ grouped_data() })
  output$table_r3  <- renderTable({ grouped_data() })
  
  observe({
    updateSelectInput(session, "selected_table",
                      choices = c("All", as.character(unique(income_dist_list[[gamesession_paths[names(gamesession_paths) %in% input$selected_gamesession]]][["income_dist_df"]]$group_name))),
    )})
}

shinyApp(ui, server)