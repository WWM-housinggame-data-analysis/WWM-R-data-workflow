
# ------------------------------------------------------------
# Script: scripts/GP3_How_did_welfare_type_affect_players_choices_example.R
# Purpose: Example script to run data analysis and visualization for GP3 in RStudio
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
#   Rscript "scripts/GP3_How_did_welfare_type_affect_players_choices_example.R"
#
# Authors: Juliette Cortes Arevalo, Ines Dattatreya, João Guimarães
# Created: 2026-07-13
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
library(base64enc)



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

### Load function containing the transformation of data tables to fit the format required for GP3 plotly visualization (i.e. dropping columns, aggregate and pivoting tables)
source(here::here(file.path(FUNCTION_PATH, "prepare-GP3-data.R")))

### Load functions required to handle dashboard filter actions
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))

### Load functions required to setup plotly visualizations
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

gamesession_data_list <- upload_dbtables(RAWDATA_PATH, "housinggame", excel = FALSE)

## Preprocess tables available for each session. Preprocessed tables are returned in a single list with same overarching structure as the input gamesession_data_list
preprocess_data_list <- list()

for (session_name in names(gamesession_data_list)) {
  preprocess_data_list[[session_name]] <- preprocess_selected_dbtables(gamesession_data_list[[session_name]], session_name, excel = FALSE)
  preprocess_data_list[[session_name]] <- preprocess_extra_dbtables_GP3(preprocess_data_list[[session_name]], session_name, excel = FALSE)
}

## Select game session for analysis
selected_gamesession <- names(preprocess_data_list)[length(names(preprocess_data_list))]

## Select table group for analysis. To select all define with SELECT_ALL
selected_table <- SELECT_ALL

## Select cost types to be included in analysis. To select all define with SELECT_ALL
selected_measure_types <- SELECT_ALL

## Retrieve income distribution data frame to be used for data visualization
measures_combined_df <- retrieve_GP3_dataframe(preprocess_data_list[[selected_gamesession]][["measures_combined"]])

round_ids <- get_round_ids(measures_combined_df)

interm_rids <- gsub(ROUND_ACCORDION_IDPREF, "", round_ids[round_ids != SELECT_ALL])

## Retrieve summary table for data plotted in analysis for GP2
# GP2_summary_df <- retrieve_GP2_summary_tables(income_dist_df, selected_cost_types, selected_table, game_round = SELECT_ALL)$num_df
# 
# # Export Summary table
# write.csv(GP2_summary_df, file.path(RESULTS_PATH, "GP2_sumstats.csv"), row.names = FALSE, quote = FALSE)

## Retrieve data to be plotted in analysis for GP2.
## Data is retrieved for cost type and table group selection defined above.
## Data representative of the whole game session and of each game round is retrieved, respectively.

GP3_plotall_data <- retrieve_GP3_plot_data(measures_combined_df, selected_table, selected_measure_types, game_round = SELECT_ALL, interm_rounds = interm_rids)
GP3_plot1_data <- retrieve_GP3_plot_data(measures_combined_df, selected_table, selected_measure_types, game_round = "1", interm_rounds = interm_rids)
GP3_plot2_data <- retrieve_GP3_plot_data(measures_combined_df, selected_table, selected_measure_types, game_round = "2", interm_rounds = interm_rids)
GP3_plot3_data <- retrieve_GP3_plot_data(measures_combined_df, selected_table, selected_measure_types, game_round = "3", interm_rounds = interm_rids)

## Save GP2 plot in main directory and display it in RStudio viewer
save_and_view_GP3_plot(GP3_plotall_data, file = file.path(RESULTS_PATH, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_GP3_plot.png")),  vheight = 1100)
