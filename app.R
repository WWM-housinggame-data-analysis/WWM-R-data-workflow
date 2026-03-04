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
source(here::here(file.path(FUNCTION_PATH, "constants.R")))


# Source files ----

## Load required functions

### Load functions required for listing, uploading and exporting data
source(here::here(file.path(FUNCTION_PATH, "list-upload-export-dbtables.R")))

### Load function containing the preprocessing of data tables coming from the database (i.e. formatting existingm adding existing or calculating new columns)
source(here::here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))

source(here::here(file.path(FUNCTION_PATH, "design-shiny-ui-server.R")))

### Load function containing the transformation of data tables to summary tables (i.e. dropping columns and aggregate tables)
source(here::here(file.path(FUNCTION_PATH, "table-data.R")))

### Load function containing the transformation of data tables to fit the format required for GP1 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP1-data.R")))

### Load functions required to handle dashboard filter actions
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))

### Load functions required to setup plotly visualizations
source(here::here(file.path(FUNCTION_PATH, "create-GP1-plot.R")))


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
default_role_selection <- process_config_selection(as.character(unique(preprocess_data_list[["housinggame_session_20_251007_VerzekeraarsMasterClass"]][["income_dist_df"]]$group_name)),
                                           SELECTED_USERNAME,
                                           fallback = "All")

  


# Shiny App ----

ui <- bslib::page_navbar(
  title = "WhereWeMove Dashboard",
  navbar_options = bslib::navbar_options(bg = "#2D89C8",
                                  theme = "dark"),
  
  bslib::nav_panel(
    title = "Game Play",
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = "Choices and effects",
        bg = "white",
        bslib::accordion(
          multiple = FALSE,   # only one open at a time
          
          bslib::accordion_panel("1: Select Game Session",
                          mod_input_reset_ui("gamesession", "Session")
                          
          ),
          
          bslib::accordion_panel("2: Select Table",
                          mod_input_reset_ui("table", "Table")
          ),
          
          bslib::accordion_panel("3: Where players live"),
          
          # checkboxGroupInput and its reset
          bslib::accordion_panel("4: Player spending",
                          mod_multicheck_reset_ui("cost_types", "Cost Types:")
                          
          ),
          
          bslib::accordion_panel("5: Selected measures"),
          bslib::accordion_panel("6: Flood in gameplay"),
          bslib::accordion_panel("7: Damage & satisfaction")
        ),
        
        
        # Optional: a global reset all button for the whole sidebar
        shiny::div(
          class = "mt-3",
          shiny::actionButton("reset_all_filters", "Reset all filters", class = "btn-warning")
        )
        
      ),
      
      shiny::mainPanel(width = 10,
                bslib::accordion(
                  open = c("All Rounds"),
                  bslib::accordion_panel(
                    "All Rounds",
            shiny::tabsetPanel(type = "tabs",
                        shiny::tabPanel("Plot", plotly::plotlyOutput("plot_all"), shiny::verbatimTextOutput("debug")),
                        shiny::tabPanel("Summary", shiny::verbatimTextOutput("summary_all")),
                        shiny::tabPanel("Table", shiny::tableOutput("table_all"))
            )
          ),
          bslib::accordion_panel(
            "Round 1",
            shiny::tabsetPanel(type = "tabs",
                        shiny::tabPanel("Plot", plotly::plotlyOutput("plot_r1")),
                        shiny::tabPanel("Summary", shiny::verbatimTextOutput("summary_r1")),
                        shiny::tabPanel("Table", shiny::tableOutput("table_r1"))
            )
          ),
          bslib::accordion_panel(
            "Round 2",
            shiny::tabsetPanel(type = "tabs",
                        shiny::tabPanel("Plot", plotly::plotlyOutput("plot_r2")),
                        shiny::tabPanel("Summary", shiny::verbatimTextOutput("summary_r2")),
                        shiny::tabPanel("Table", shiny::tableOutput("table_r2"))
            )
          ),
          bslib::accordion_panel(
            "Round 3",
            shiny::tabsetPanel(type = "tabs",
                        shiny::tabPanel("Plot", plotly::plotlyOutput("plot_r3")),
                        shiny::tabPanel("Summary", shiny::verbatimTextOutput("summary_r3")),
                        shiny::tabPanel("Table", shiny::tableOutput("table_r3"))
            )
          )
        )
      )
    )
  ),
  
  
  
  bslib::nav_panel(title = "Game Settings", shiny::p("First page content.")),
  bslib::nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    bslib::nav_item(shiny::tags$a("About WhereWeMove", href = "https://seriousgaming.tudelft.nl/games/")),
    bslib::nav_item(shiny::tags$a("WhereWeMove info", href = "https://pure.tudelft.nl/ws/portalfiles/portal/180909041/WhereWeMove-Brochure_Final.pdf")),
    bslib::nav_item(shiny::tags$a("Facilitator website", href = "https://housing-game.tbm.tudelft.nl/housinggame-facilitator/jsp/facilitator/login.jsp")),
    bslib::nav_item(shiny::tags$a("Player website", href = "https://housing-game.tbm.tudelft.nl/housinggame-player/jsp/player/login.jsp"))
  )
)





server <- function(input, output, session) {
  
  if (identical(gamesession_selection, "All")) {
    gamesession_choices <- shiny::reactive(names(preprocess_data_list))
  } else {
    gamesession_choices <- shiny::reactive(gamesession_selection)
  }
  
  selected_gamesession <- mod_input_reset_server(
    id = "gamesession",
    default_value = shiny::reactive(gamesession_choices()[length(gamesession_choices())]),
    get_choices = gamesession_choices
  )
  
  income_dist_df <- shiny::reactive({
    preprocess_data_list[[ selected_gamesession() ]][["income_dist_df"]]
    })
  
  # Add a req() or a safe fallback for the case where income_dist_df() doesn’t yet contain group_names.
  #Why this helps: You’ll never send character(0) to process_config_selection() or the module. The module also won’t try to update until choices are non-empty.
  
  # role_selection from YAML, falling back to "All" if needed
  role_selection <- shiny::reactive({
    df <- income_dist_df()
    groups <- character(0)
    if (!is.null(df) && nrow(df) > 0 && "group_name" %in% names(df)) {
      groups <- as.character(unique(df$group_name))
    }
    process_config_selection(groups, SELECTED_USERNAME, fallback = "All")
  })
  
  
  # Reactive table choices
  
  # table choices: lock to a single role if YAML default is not "All"
  table_choices <- shiny::reactive({
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
  
  # --- Cost Types (checkbox) ---
  
  # Choices reactive (can be dynamic if needed)
  cost_types_choices <- shiny::reactive({
    c("All", names(EXPENSE_BARCOLS))
  })
  
  # Default selection reactive (from config or static)
  # If you have a YAML default like CONFIG$defaults$cost_types, wire it here.
  # Otherwise, default to "All".
  cost_types_default <- shiny::reactive({
    "All"
    # or CONFIG$defaults$cost_types
  })
  
  selected_cost_types <- mod_multicheck_reset_server(
    id            = "cost_types",
    default_values = cost_types_default,   # reactive() returning a vector (e.g., "All" or c("Mortgage payment", ...))
    get_choices    = cost_types_choices,
    all_label      = "All",
    expand_all     = FALSE                  # keep only "All" when All is selected (set TRUE to expand to all)
  )
  
  # Optional: global "Reset all filters"
  shiny::observeEvent(input$reset_all_filters, {
    if (!is.null(session$userData$gamesession_reset)) session$userData$gamesession_reset()
    if (!is.null(session$userData$table_reset))       session$userData$table_reset()
    if (!is.null(session$userData$cost_types_reset))  session$userData$cost_types_reset()
  })
  
  
  selected_bar_segments <- shiny::reactive({
    # selected_cost_types() already normalized. Still filter to known keys.
    sel <- selected_cost_types()
    filter_selected_categs(sel, c("All", names(EXPENSE_BARCOLS)))
  })
  
  selected_columns <- shiny::reactive({
    EXPENSE_BARCOLS[names(EXPENSE_BARCOLS) %in% selected_bar_segments()]
  })
  

  summary_df <- shiny::reactive({retrieve_summary_table(income_dist_df(), selected_table())})
  
  
  GP1_plotall_data <- shiny::reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "All", fill_values_all) })
  GP1_plot1_data <- shiny::reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "1", fill_values_all) })
  GP1_plot2_data <- shiny::reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "2", fill_values_all) })
  GP1_plot3_data <- shiny::reactive({ prepare_GP1_data(income_dist_df(), selected_columns(), selected_table(), game_round = "3", fill_values_all) })
  
  # Connect plots
  output$plot_all <- plotly::renderPlotly({ create_GP1_plotly(GP1_plotall_data()) })
  output$plot_r1  <- plotly::renderPlotly({ create_GP1_plotly(GP1_plot1_data()) })
  output$plot_r2  <- plotly::renderPlotly({ create_GP1_plotly(GP1_plot2_data()) })
  output$plot_r3  <- plotly::renderPlotly({ create_GP1_plotly(GP1_plot3_data()) })
  
  # Summaries (update based on color_by choice)
  output$summary_all <- shiny::renderPrint({ summary(summary_df()) })
  output$summary_r1  <- shiny::renderPrint({ summary(summary_df()) })
  output$summary_r2  <- shiny::renderPrint({ summary(summary_df()) })
  output$summary_r3  <- shiny::renderPrint({ summary(summary_df()) })
  
  # Tables (update based on color_by choice)
  output$table_all <- shiny::renderTable({ summary_df() })
  output$table_r1  <- shiny::renderTable({ summary_df() })
  output$table_r2  <- shiny::renderTable({ summary_df() })
  output$table_r3  <- shiny::renderTable({ summary_df() })
}

shiny::shinyApp(ui, server)