
# ------------------------------------------------------------
# Script: ./app.R
# Purpose: Run WhereWeMove Shiny Dashboard.
# Details: Current dashboard version support visuals for GP2 (only)
#
# Working directory:
#   Project root (see here::here())
#
# Inputs:
#   - data/raw/*.csv
#
# Outputs:
#   - data/raw/*.xlsx
#   - data/preprocessed/*.xlsx
#
# How to run: Click on "Run App" or run  ```Rscript -e "shiny::runApp('.', host='0.0.0.0', port=3838)"``` in the terminal.
#
# Author: João Guimarães
# Created: 2026-04-10
# Maintainer: Juliette Cortes Arevalo and Alex Verbraeck
# ------------------------------------------------------------

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

### Load function containing the transformation of data tables to fit the format required for GP2 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP2-data.R")))

### Load functions required to handle dashboard filter actions
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))

### Load functions required to setup plotly visualizations
source(here::here(file.path(FUNCTION_PATH, "create-GP2-plot.R")))


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

## Preprocess tables available for each session. Preprocessed tables are returned in a single list with sameoverarching structure as the input gamesession_data_list
preprocess_data_list <- list()

for (session_name in names(gamesession_data_list)) {
  preprocess_data_list[[session_name]] <- preprocess_dbtables(gamesession_data_list[[session_name]], session_name, excel = FALSE)
}

## Define default game session

default_gamesession <- process_config_selection(names(preprocess_data_list), SELECTED_GAMESESSION, fallback = SELECT_ALL)


# Shiny App ----

ui <- bslib::page_navbar(
  title = HEADER_TITLE,
  navbar_options = APP_NAVBAR_OPTIONS,
  
  bslib::nav_panel(
    title = HEADER_TAB1,
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = SIDEBAR1_TITLE,
        bg = SIDEBAR1_BACKCOLOR,
        bslib::accordion(
          multiple = EXPAND_MULTIPLE_ACCORDIONS,
          
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
        RESET_ALL_BUTTON
        
      ),
      
      shiny::mainPanel(width = MAIN_PANEL_WIDTH,
                       bslib::accordion(
                         open = DEFAULT_OPEN_ACCORDIONS,
                         make_round_panel(ROUND_ACCORDION_ID[1], names(ROUND_ACCORDION_ID)[1]),
                         make_round_panel(ROUND_ACCORDION_ID[2], names(ROUND_ACCORDION_ID)[2]),
                         make_round_panel(ROUND_ACCORDION_ID[3], names(ROUND_ACCORDION_ID)[3]),
                         make_round_panel(ROUND_ACCORDION_ID[4], names(ROUND_ACCORDION_ID)[4])
                       )
      )
    )
  ),
  
  
  bslib::nav_panel(title = "Game Settings", shiny::p("First page content.")),
  bslib::nav_spacer(),
  REFS_HEADER_TAB
)



server <- function(input, output, session) {

  # --- centralize selection + derived data
  gs <- make_gamesession_reactives(
    preprocess_data_list    = preprocess_data_list,
    gamesession_selection   = default_gamesession,  # SELECT_ALL or vector from config
    id = "gamesession"                              # matches your UI module id
  )

  # Keep names for readability
  selected_gamesession <- gs$selected_gamesession
  income_dist_df       <- gs$income_dist_df

  
  role_table <- make_role_table_reactives(
    income_dist_df = income_dist_df,    # reactive returned from previous helper
    selected_username = SELECTED_USERNAME,
    id = "table"
  )
  
  role_selection <- role_table$role_selection
  table_choices  <- role_table$table_choices
  selected_table <- role_table$selected_table
  
  
  selected_cost_types <- make_cost_types_reactive(id = "cost_types")
    
  # global "Reset all filters"
  add_global_reset_observer(input, session)
  

  summary_df <- shiny::reactive({retrieve_GP2_summary_table(income_dist_df(), selected_table())})
  
  
  GP2_plotall_data <- shiny::reactive({ retrieve_GP2_plot_data(income_dist_df(), selected_cost_types(), selected_table(), game_round = SELECT_ALL, fill_values_all) })
  GP2_plot1_data <- shiny::reactive({ retrieve_GP2_plot_data(income_dist_df(), selected_cost_types(), selected_table(), game_round = "1", fill_values_all) })
  GP2_plot2_data <- shiny::reactive({ retrieve_GP2_plot_data(income_dist_df(), selected_cost_types(), selected_table(), game_round = "2", fill_values_all) })
  GP2_plot3_data <- shiny::reactive({ retrieve_GP2_plot_data(income_dist_df(), selected_cost_types(), selected_table(), game_round = "3", fill_values_all) })
  
  # Connect plots
  output$plot_all <- plotly::renderPlotly({ create_GP2_plotly(GP2_plotall_data()) })
  output$plot_r1  <- plotly::renderPlotly({ create_GP2_plotly(GP2_plot1_data()) })
  output$plot_r2  <- plotly::renderPlotly({ create_GP2_plotly(GP2_plot2_data()) })
  output$plot_r3  <- plotly::renderPlotly({ create_GP2_plotly(GP2_plot3_data()) })
  
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