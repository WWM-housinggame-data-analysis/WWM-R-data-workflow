# Load necessary libraries ----

## Load for handling file location
library(here)

## Load importing/exporting data
library(readxl)
library(readr)
library(openxlsx)
library(writexl)
library(yaml)

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

source(here(file.path(FUNCTION_PATH, "design-shiny-ui-server.R")))

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

gamesession_data_list <- upload_dbtables(RAWDATA_PATH, "housinggame", excel = FALSE, selection = TRUE)

## Preprocess tables available for each session, being returned in a single list with same  overarching structure as the input gamesession_data_list
preprocess_data_list <- list()

for (session_name in names(gamesession_data_list)) {
  preprocess_data_list[[session_name]] <- preprocess_dbtables(gamesession_data_list[[session_name]], session_name, excel = FALSE)
}

gamesession_selection <- process_config_selection(names(preprocess_data_list), SELECTED_GAMESESSION, fallback = "All")
role_selection <- process_config_selection(as.character(unique(preprocess_data_list[["housinggame_session_20_251007_VerzekeraarsMasterClass"]][["income_dist_df"]]$group_name)),
                                           SELECTED_USERNAME,
                                           fallback = "All")

  


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
                          mod_input_reset_ui("gamesession", "Session")
                          
          ),
          
          accordion_panel("2: Select Table",
                          mod_input_reset_ui("table", "Table")
          ),
          
          accordion_panel("3: Where players live"),
          
          accordion_panel("4: Player spending",
                          
                          
                          # checkboxGroupInput and its reset
                          div(
                            checkboxGroupInput("bar_segment", "Cost_Types:",
                                               choices = c("All", names(EXPENSE_BARCOLS)),
                                               selected = "All"),
                            actionButton("reset_cost", "Reset", class = "btn-outline-secondary btn-sm mt-3")
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
  
  if (identical(gamesession_selection, "All")) {
    gamesession_choices <- reactive(names(preprocess_data_list))
  } else {
    gamesession_choices <- reactive(gamesession_selection)
  }
  
  selected_gamesession <- mod_input_reset_server(
    id = "gamesession",
    default_value = reactive(gamesession_choices()[length(gamesession_choices())]),
    get_choices = gamesession_choices
  )
  
  income_dist_df <- reactive({
    preprocess_data_list[[ selected_gamesession() ]][["income_dist_df"]]
    })
  
  # Add a req() or a safe fallback for the case where income_dist_df() doesn’t yet contain group_names.
  #Why this helps: You’ll never send character(0) to process_config_selection() or the module. The module also won’t try to update until choices are non-empty.
  # role_selection from YAML, falling back to "All" if needed
  role_selection <- reactive({
    df <- income_dist_df()
    groups <- character(0)
    if (!is.null(df) && nrow(df) > 0 && "group_name" %in% names(df)) {
      groups <- as.character(unique(df$group_name))
    }
    process_config_selection(groups, SELECTED_USERNAME, fallback = "All")
  })
  
  
  # Reactive table choices
  
  # table choices: lock to a single role if YAML default is not "All"
  table_choices <- reactive({
    df <- income_dist_df()
    # If no data yet, at least offer "All" to keep module happy
    if (is.null(df) || nrow(df) == 0 || !"group_name" %in% names(df)) {
      return("All")
    }
    
    if (identical(role_selection(), "All")) {
      c("All", as.character(unique(df$group_name)))
    } else {
      # lock to YAML-selected role
      role_selection()
    }
  })
  
  
  selected_table <- mod_input_reset_server(
    id = "table",
    default_value = role_selection,
    get_choices = table_choices
  )
  
  # Reset only the checkboxGroupInput
  observeEvent(input$reset_cost, {
    # If your "All" is a semantic choice, reselect it:
    updateCheckboxGroupInput(session, "bar_segment", selected = "All")
  })
  
  # Optional: global "Reset all filters"
  observeEvent(input$reset_all_filters, {
    if (!is.null(session$userData$gamesession_reset)) session$userData$gamesession_reset()
    if (!is.null(session$userData$table_reset))       session$userData$table_reset()
    updateCheckboxGroupInput(session, "bar_segment",    selected = "All")
  })
  
  #selected_table <- reactive({filter_selected_categs(selected_table(), table_choices())})
  
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
}

shinyApp(ui, server)