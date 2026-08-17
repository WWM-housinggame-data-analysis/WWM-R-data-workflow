# ------------------------------------------------------------
# Script: R/question-data-workflows.R
# Purpose:
#
# Details:
#
# Usage:
#   source("R/question-data-workflows.R")
#
# Exposed functions:
#
# Dependencies:
#
# Notes:
# ------------------------------------------------------------

# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))


# Source files ----

## Load required functions

### Load functions containing the preprocessing of data tables coming from the database (i.e. formatting existing adding existing or calculating new columns)
source(here::here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))

### Load function containing the transformation of data tables to fit the format required for GP2 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP2-data.R")))

### Load function containing the transformation of data tables to fit the format required for GP3 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP3-data.R")))

### Load functions required to create GP2 plotly visualizations
source(here::here(file.path(FUNCTION_PATH, "create-GP2-plot.R")))

### Load functions required to create GP3 plotly visualizations
source(here::here(file.path(FUNCTION_PATH, "create-GP3-plot.R")))



question_preprocessing_workflow <- list(
  
  GP2 = list(
    
    get_preprocessed_data = function(gamesession_data_tables, session_name) {
      
      ##R/preprocess-dbtables.R
      preprocess_selected_dbtables(gamesession_data_tables, session_name, excel = FALSE)
      
    }
  ),
  
  All = list(
    
    get_preprocessed_data = function(gamesession_data_tables, session_name) {
      
      ##R/preprocess-dbtables.R
      preprocess_data_tables <- preprocess_selected_dbtables(gamesession_data_tables, session_name, excel = FALSE)
      
      preprocess_extra_dbtables_GP3(preprocess_data_tables, session_name, excel = FALSE)
      
    }
  )
)

question_dashboard_workflow <- list(
  
  GP2 = list(
    
    get_plot_data = function(
    df,
    selected_table,
    selected_cost_types,
    game_round,
    interm_rounds
    ) {
      
      retrieve_GP2_plot_data(
        df,
        selected_cost_types,
        selected_table,
        game_round,
        interm_rounds,
        fill_values_all
      )
      
    },
    
    get_summary_table = function(
    df,
    selected_table,
    selected_cost_types,
    game_round,
    interm_rounds
    ) {
      
      retrieve_GP2_summary_tables(
        df,
        selected_cost_types,
        selected_table,
        game_round,
        interm_rounds
      )
      
    },
    
    get_stats_table = function(
    df,
    selected_table,
    selected_cost_types,
    game_round,
    interm_rounds
    ) {
      
      retrieve_GP2_stats_tables(
        df,
        selected_cost_types,
        selected_table,
        game_round,
        interm_rounds
      )
      
    },
    
    render_plot = create_GP2_plotly,
    
    adjust_plotly_height = adjust_GP2_plotly_height,
    
    summary_type = "gp2",
    
    show_cost_filter = TRUE
    
  ),
  
  GP3 = list(
    
    get_plot_data = function(
    df,
    selected_table,
    selected_cost_types,
    game_round,
    interm_rounds
    ) {
      
      retrieve_GP3_plot_data(
        df,
        selected_table,
        game_round,
        interm_rounds
      )
      
    },
    
    get_summary_table = function(
    df,
    selected_table,
    selected_cost_types,
    game_round,
    interm_rounds
    ) {
      
      retrieve_GP3_summary_tables(
        df,
        selected_table,
        game_round,
        interm_rounds
      )
      
    },
    
    get_stats_table = function(
    df,
    selected_table,
    selected_cost_types,
    game_round,
    interm_rounds
    ) {
      
      retrieve_GP3_stats_tables(
        df,
        selected_table,
        game_round,
        interm_rounds
      )
      
    },
    
    render_plot = create_GP3_plotly,
    
    adjust_plotly_height = adjust_GP3_plotly_height,
    
    summary_type = "gp3",
    
    show_cost_filter = FALSE
    
  )
  
)