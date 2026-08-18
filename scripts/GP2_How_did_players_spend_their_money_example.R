
# ------------------------------------------------------------
# Script: scripts/GP2_How did players spend their money_example.R
# Purpose: Example script to run data analysis and visualization for GP2 in RStudio
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
#   - data/results/*.png
#
# How to run:
#   Rscript "scripts/GP2_How did players spend their money_example.R"
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
library(webshot2)
library(htmlwidgets)
library(rstudioapi)



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

available_gamesessions <- basename(list_matching_subfolders(RAWDATA_PATH, GAMESESSION_FLAG))

gamesession_data_list <- list()
preprocess_data_list <- list()


for (session_name in available_gamesessions) {
  gamesession_data_list[[session_name]] <- upload_dbtables(RAWDATA_PATH, session_name, excel = FALSE)
  
  ## Preprocess tables available for each session. Preprocessed tables are returned in a single list with same overarching structure as the input gamesession_data_list
  preprocess_data_list[[session_name]] <- preprocess_selected_dbtables(gamesession_data_list[[session_name]], session_name, excel = FALSE)
}

## Select game session for analysis
selected_gamesession <- available_gamesessions[length(available_gamesessions)]

## Select table group for analysis. To select all define with SELECT_ALL
selected_table <- SELECT_ALL

## Select cost types to be included in analysis. To select all define with SELECT_ALL
selected_cost_types <- SELECT_ALL

## Retrieve income distribution data frame to be used for data visualization
income_dist_df <- retrieve_GP2_dataframe(preprocess_data_list[[selected_gamesession]][["playerround"]][, INCOME_DIST_ALLCOLS])

round_ids <- get_round_ids(income_dist_df)

interm_rids <- gsub(ROUND_ACCORDION_IDPREF, "", round_ids[round_ids != SELECT_ALL])

## Retrieve summary table for data plotted in analysis for GP2
GP2_summary_df <- retrieve_GP2_summary_tables(income_dist_df, selected_cost_types, selected_table, game_round = SELECT_ALL, interm_rounds = interm_rids)

# Export Summary table
write.csv(GP2_summary_df, file.path(RESULTS_PATH, "GP2_sumstats.csv"), row.names = FALSE, quote = FALSE)

## Retrieve data to be plotted in analysis for GP2.
## Data is retrieved for cost type and table group selection defined above.
## Data representative of the whole game session and of each game round is retrieved, respectively.
GP2_plotall_data <- retrieve_GP2_plot_data(income_dist_df, selected_cost_types, selected_table, game_round = SELECT_ALL, interm_rounds = interm_rids, fill_values_all)
GP2_plot1_data <- retrieve_GP2_plot_data(income_dist_df, selected_cost_types, selected_table, game_round = "1", interm_rounds = interm_rids, fill_values_all)
GP2_plot2_data <- retrieve_GP2_plot_data(income_dist_df, selected_cost_types, selected_table, game_round = "2", interm_rounds = interm_rids, fill_values_all)
GP2_plot3_data <- retrieve_GP2_plot_data(income_dist_df, selected_cost_types, selected_table, game_round = "3", interm_rounds = interm_rids, fill_values_all)

## Save GP2 plot in main directory and display it in RStudio viewer. Always add date and time to generate new figure
save_and_view_GP2_plot(GP2_plotall_data, file = file.path(RESULTS_PATH, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_GP2_plot.png")), CONFIG[["plotly"]][["GP2"]][["script"]], vheight = 1100)
