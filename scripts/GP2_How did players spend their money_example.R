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

selected_gamesession <- names(preprocess_data_list)[length(names(preprocess_data_list))]
selected_table <- SELECT_ALL
selected_cost_types <- SELECT_ALL

income_dist_df <- preprocess_data_list[[selected_gamesession]][[PREPROCESSED_DBTABLES]]

summary_df <- retrieve_summary_table(income_dist_df, selected_table)
  
GP1_plotall_data <- prepare_GP1_data(income_dist_df, selected_cost_types, selected_table, game_round = SELECT_ALL, fill_values_all)
GP1_plot1_data <- prepare_GP1_data(income_dist_df, selected_cost_types, selected_table, game_round = "1", fill_values_all)
GP1_plot2_data <- prepare_GP1_data(income_dist_df, selected_cost_types, selected_table, game_round = "2", fill_values_all)
GP1_plot3_data <- prepare_GP1_data(income_dist_df, selected_cost_types, selected_table, game_round = "3", fill_values_all)

save_and_view_GP1_plot(GP1_plotall_data, vheight = 1100)
