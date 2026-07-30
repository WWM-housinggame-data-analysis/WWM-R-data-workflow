
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

### Load functions containing the preprocessing of data tables coming from the database (i.e. formatting existing adding existing or calculating new columns)
source(here::here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))

### Load functions for designing shiny ui to be deployed
source(here::here(file.path(FUNCTION_PATH, "design-shiny-ui.R")))

### Load functions for making shiny data dynamic
source(here::here(file.path(FUNCTION_PATH, "make-data-reactive.R")))

### Load function containing the transformation of data tables to fit the format required for GP2 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP2-data.R")))

### Load functions required to handle dashboard user interactivity
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))

### Load functions required to create GP2 plotly visualizations
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

##R/list-upload-export-dbtables.R
gamesession_data_list <- upload_dbtables(RAWDATA_PATH, "housinggame", excel = FALSE)

## Preprocess tables available for each session. Preprocessed tables are returned in a single list with sameoverarching structure as the input gamesession_data_list
preprocess_data_list <- list()
income_dist_list <- list()

for (session_name in names(gamesession_data_list)) {
  
  ##R/preprocess-dbtables.R
  preprocess_data_list[[session_name]] <- preprocess_selected_dbtables(gamesession_data_list[[session_name]], session_name, excel = FALSE)
  
  income_dist_list[[session_name]] <- retrieve_GP2_dataframe(preprocess_data_list[[session_name]][["playerround"]][, INCOME_DIST_ALLCOLS])
}



# Shiny App ----

## Define default game session

##R/interact-data.R
default_gamesession <- process_config_selection(names(preprocess_data_list), SELECTED_GAMESESSION, fallback = SELECT_ALL)


## Design Dashboard User Interface
### All local functions stored in R/design-shiny-ui.R
### Explanations for arguments with CONSTANTS assigned to them is provided in constants.R

ui <- bslib::page_navbar(
  
  ## Header settings
  title = HEADER_TITLE,
  navbar_options = APP_NAVBAR_OPTIONS,
  
  ## GP2 Panel Settings
  bslib::nav_panel(
    title = HEADER_TAB1,
    bslib::page_sidebar(
      
      ## Sidebar Settings
      sidebar = bslib::sidebar(
        title = SIDEBAR1_TITLE,
        bg = SIDEBAR1_BACKCOLOR,
        bslib::accordion(
          multiple = EXPAND_MULTIPLE_ACCORDIONS,
          
          ## Game Session Filter Details
          bslib::accordion_panel(SESSION_ACCORDION_TITLE,
                          mod_input_reset_ui(SESSION_ACCORDION_VALUE, SESSION_ACCORDION_LABEL)
                          ),
          
          ## Table Group Filter Details
          bslib::accordion_panel(GROUP_ACCORDION_TITLE,
                                 mod_input_reset_ui(GROUP_ACCORDION_VALUE, GROUP_ACCORDION_LABEL)
                                 ),
          
          ## Players' Game Address Filter Details
          bslib::accordion_panel(ADDRESS_ACCORDION_TITLE
                                 ),
          
          ## Players' Costs Filter Details (Used to segment bars)
          bslib::accordion_panel(SEGMENT_ACCORDION_TITLE,
                          mod_multicheck_reset_ui(SEGMENT_ACCORDION_VALUE, SEGMENT_ACCORDION_LABEL)
                          
          ),
          
          ## Measures Filter Details
          bslib::accordion_panel(MEASURES_ACCORDION_TITLE),
          
          ## Flood Status Filter Details
          bslib::accordion_panel(FLOOD_ACCORDION_TITLE),
          
          ## Players'Satisfaction Filter Details
          bslib::accordion_panel(SATISFACTION_ACCORDION_TITLE)
        ),
        
        
        ## Button to reset all sidebar filters to default
        RESET_ALL_BUTTON
        
      ),
      
      ## Main Panel Settings where visuals are displayed for the data collected across the whole game session and split per rounds.
      ## This part of the UI design is dependent on the input data and retrieved by the server output (argument UI_ROUNDS_RENDERING).
      shiny::mainPanel(width = MAIN_PANEL_WIDTH,
                       shiny::uiOutput(UI_ROUNDS_RENDERING)
      )
    )
  ),
  
  ## Game Settings Panel Settings
  bslib::nav_panel(title = HEADER_TAB2, shiny::p("First page content.")),
  
  ## Move References Menu to the right corner
  bslib::nav_spacer(),
  
  ## References Menu Settings
  REFS_HEADER_TAB
)



server <- function(input, output, session) {

  # --- centralize selection + derived data
  gs <- make_gamesession_reactives(
    session_data_list     = income_dist_list,
    gamesession_selection = default_gamesession,  # SELECT_ALL or vector from config
    id = "gamesession"                              # matches your UI module id
  )

  # Keep names for readability
  selected_gamesession <- gs$selected_gamesession
  income_dist_df       <- gs$selected_session_df

  
  role_table <- make_role_table_reactives(
    income_dist_df = income_dist_df,    # reactive returned from previous helper
    selected_username = SELECTED_USERNAME,
    id = "table"
  )
  
  role_selection <- role_table$role_selection
  table_choices  <- role_table$table_choices
  selected_table <- role_table$selected_table
  
  
  # ---- Dynamic rounds (IDs + labels) ----
  round_ids <- make_rounds_reactive(income_dist_df)
  
  
  # ---- Dynamic UI ----
  output[[UI_ROUNDS_RENDERING]] <- shiny::renderUI({
    make_round_panels(round_ids())
  })
  
  
  selected_cost_types <- make_cost_types_reactive(id = "cost_types")
    
  # global "Reset all filters"
  add_global_reset_observer(input, session)
  

  shiny::observe({
    
    shiny::req(length(round_ids()) > 0)
    
    interm_rids <- gsub(ROUND_ACCORDION_IDPREF, "", round_ids()[ round_ids() != SELECT_ALL])
    
    lapply(unname(round_ids()), function(rid) {
      
      local({

        plot_id    <- paste0("plot_", rid)
        summary_id <- paste0("summary_", rid)
        table_id   <- paste0("table_", rid)
        
        
        rid_value = if (rid == SELECT_ALL) SELECT_ALL else gsub(ROUND_ACCORDION_IDPREF, "", rid)
        
        
        plot_data <- shiny::reactive({
          retrieve_GP2_plot_data(
            income_dist_df(),
            selected_cost_types(),
            selected_table(),
            game_round = rid_value,
            interm_rounds = interm_rids,
            fill_values_all
          )
        })
        
        summary_tables <- shiny::reactive({
          retrieve_GP2_summary_tables(
            income_dist_df(),
            selected_cost_types(),
            selected_table(),
            game_round = rid_value,
            interm_rounds = interm_rids)
          })
        
        num_summary_df <- shiny::reactive({summary_tables()$num_df})
        kval_summary_df <- shiny::reactive({summary_tables()$kval_df})
        
        output[[plot_id]] <- plotly::renderPlotly({
          create_GP2_plotly(plot_data())
        })
        
        output[[summary_id]] <- shiny::renderPrint({
          summary(num_summary_df())
        })
        
        output[[table_id]] <- shiny::renderTable({
          kval_summary_df()
        })
      })
    })
  })
}

shiny::shinyApp(ui, server)