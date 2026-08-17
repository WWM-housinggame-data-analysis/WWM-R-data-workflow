
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
library(shinyjs)
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

source(here::here(file.path(FUNCTION_PATH, "question-data-workflows.R")))

### Load functions containing the preprocessing of data tables coming from the database (i.e. formatting existing adding existing or calculating new columns)
source(here::here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))

### Load functions for designing shiny ui to be deployed
source(here::here(file.path(FUNCTION_PATH, "design-shiny-ui.R")))

### Load functions for making shiny data dynamic
source(here::here(file.path(FUNCTION_PATH, "make-data-reactive.R")))

### Load function containing the transformation of data tables to fit the format required for GP2 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP2-data.R")))

### Load function containing the transformation of data tables to fit the format required for GP3 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP3-data.R")))

### Load functions required to handle dashboard user interactivity
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))

### Load functions required to create GP2 plotly visualizations
source(here::here(file.path(FUNCTION_PATH, "create-GP2-plot.R")))

### Load functions required to create GP3 plotly visualizations
source(here::here(file.path(FUNCTION_PATH, "create-GP3-plot.R")))

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

available_gamesessions <- basename(list_matching_subfolders(RAWDATA_PATH, GAMESESSION_FLAG))

## Define default game session

##R/interact-data.R
gamesession_choice <- process_dashboard_choice(available_gamesessions, SELECTED_GAMESESSION, fallback = SELECT_ALL)

gamesession_options <- process_dashboard_choice(available_gamesessions, SELECTED_GAMESESSION, fallback = SELECT_ALL, return_choice = FALSE)

question_choice <- process_dashboard_choice(AVAILABLE_QUESTIONS, SELECTED_QUESTION, fallback = SELECT_ALL)

question_options <- process_dashboard_choice(AVAILABLE_QUESTIONS, SELECTED_QUESTION, fallback = SELECT_ALL, return_choice = FALSE)

cost_options <- c(SELECT_ALL, names(COST_BAR_SEGMENTS))

default_question_option <- question_options[1]

default_gamesession_option <- gamesession_options[length(gamesession_options)]

default_cost_option <- SELECT_ALL



## Preprocess tables available for each session. Preprocessed tables are returned in a single list with sameoverarching structure as the input gamesession_data_list
gamesession_data_list <- list()
preprocess_data_list <- list()
dashboard_data_list <- vector(mode = "list", length = length(question_options))
names(dashboard_data_list) <- question_options

for (session_name in available_gamesessions) {
  
  gamesession_data_list[[session_name]] <- upload_dbtables(RAWDATA_PATH, session_name, excel = FALSE)
  
  if (identical(question_choice, "GP2")) {
    
    preprocess_data_list[[session_name]] <- question_preprocessing_workflow[[question_choice]]$get_preprocessed_data(gamesession_data_list[[session_name]], session_name)

  } else {
    preprocess_data_list[[session_name]] <- question_preprocessing_workflow[[SELECT_ALL]]$get_preprocessed_data(gamesession_data_list[[session_name]], session_name)
  }
  
  if ("GP2" %in% question_options) {
    dashboard_data_list[["GP2"]][[session_name]] <- retrieve_GP2_dataframe(preprocess_data_list[[session_name]][["playerround"]][, INCOME_DIST_ALLCOLS])
  }
  
  if ("GP3" %in% question_options) {
    dashboard_data_list[["GP3"]][[session_name]] <- retrieve_GP3_dataframe(preprocess_data_list[[session_name]][["measures_combined"]])
  }
}



# Shiny App ----

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
          bslib::accordion_panel(QUESTION_ACCORDION_TITLE,
                                 mod_input_reset_ui(QUESTION_ACCORDION_VALUE, QUESTION_ACCORDION_LABEL)
          ),
          
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
          bslib::accordion_panel(
            GP2_SEGMENT_ACCORDION_TITLE,
            
            shiny::conditionalPanel(
              condition = paste0("input['", QUESTION_ACCORDION_VALUE, "-input_value'] == '", "GP2", "'"),
              
              mod_multicheck_reset_ui(
                GP2_SEGMENT_ACCORDION_VALUE,
                GP2_SEGMENT_ACCORDION_LABEL
              )
            )
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
  
  dashboard_data_reactive <- shiny::reactive({dashboard_data_list})
  
  # --- centralize selection + derived data
  question_filtered_output <- filter_data_reactive(
    data_reactive  = dashboard_data_reactive,
    options        = question_options,  # SELECT_ALL or vector from config
    default_option = default_question_option,
    id             = "question"                              # matches your UI module id
  )
  
  # --- centralize selection + derived data
  gamesession_filtered_output <- filter_data_reactive(
    data_reactive  = question_filtered_output$filtered_data_reactive,
    options        = gamesession_options ,  # SELECT_ALL or vector from config
    default_option = default_gamesession_option,
    id             = "gamesession"                              # matches your UI module id
  )

  # Keep names for readability
  selected_question         <- question_filtered_output$filter_choice_reactive
  selected_gamesession      <- gamesession_filtered_output$filter_choice_reactive
  question_session_df       <- gamesession_filtered_output$filtered_data_reactive
  
  selected_table <- make_table_choice_reactive(
    reactive_df = question_session_df,    # reactive returned from previous helper
    table_choice = SELECTED_TABLEGROUP,
    id = "table"
  )
  
  # ---- Dynamic rounds (IDs + labels) ----
  round_ids <- make_rounds_reactive(question_session_df)
  
  selected_cost_types <- make_multicheck_filter_reactive(id = "cost_types",
                                                         get_choice = shiny::reactive({default_cost_option}),
                                                         get_options = shiny::reactive({cost_options}),
                                                         all_label = SELECT_ALL,
                                                         expand_all = FALSE)

  # global "Reset all filters"
  add_global_reset_observer(input, session)
  
  
  shiny::observe({
    
    selected_dashboard_workflow <- shiny::reactive({
      
      req(selected_question())
      
      question_dashboard_workflow[[selected_question()]]
      
    })
    
    shinyjs::toggle(
      "cost_types_container",
      condition = selected_dashboard_workflow()$show_cost_filter
    )
    
    shiny::req(length(round_ids()) > 0)
    
    interm_rids <- gsub(ROUND_ACCORDION_IDPREF, "", round_ids()[ round_ids() != SELECT_ALL])
    
    lapply(unname(round_ids()), function(rid) {
      
      local({
        
        plot_id    <- paste0("plot_", rid)
        summary_id <- paste0("summary_", rid)
        table_id   <- paste0("table_", rid)
        
        
        rid_value = if (rid == SELECT_ALL) SELECT_ALL else gsub(ROUND_ACCORDION_IDPREF, "", rid)
        
        plot_data <- shiny::reactive({
          
          selected_dashboard_workflow()$get_plot_data(
            df = question_session_df(),
            selected_table = selected_table(),
            selected_cost_types = selected_cost_types(),
            game_round = rid_value,
            interm_rounds = interm_rids
          )
          
        })
        
        plot_height <- shiny::reactive({selected_dashboard_workflow()$adjust_plotly_height(plot_data()$barlevels)})
        
        summary_data <- shiny::reactive({
          
          selected_dashboard_workflow()$get_summary_table(
            df = question_session_df(),
            selected_table = selected_table(),
            selected_cost_types = selected_cost_types(),
            game_round = rid_value,
            interm_rounds = interm_rids
          )
          
        })
        
        stats_data <- shiny::reactive({
          
          selected_dashboard_workflow()$get_stats_table(
            df = question_session_df(),
            selected_table = selected_table(),
            selected_cost_types = selected_cost_types(),
            game_round = rid_value,
            interm_rounds = interm_rids
          )
          
        })
        
        # ---- Dynamic UI ----
        output[[UI_ROUNDS_RENDERING]] <- shiny::renderUI({
          make_round_panels(round_ids(),
                            plot_height = plot_height())
        })
          
        output[[plot_id]] <- plotly::renderPlotly({
          (selected_dashboard_workflow()$render_plot(plot_data()))
        })
        
        output[[summary_id]] <- shiny::renderPrint({
          summary(summary_data())
        })
        
        output[[table_id]] <- shiny::renderTable({
          stats_data()
        })
      })
    })
  })
}

shiny::shinyApp(ui, server)