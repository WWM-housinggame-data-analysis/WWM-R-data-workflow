# R/preprocess-dbtables.R
# ---------------------------------------------------------------
# Load constants and helper components
# ---------------------------------------------------------------

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

gamesession_data_list <- upload_dbtables(RAWDATA_PATH, "housinggame", excel = FALSE)

## Preprocess tables available for each session. Preprocessed tables are returned in a single list with same overarching structure as the input gamesession_data_list
preprocess_data_list <- list()

for (session_name in names(gamesession_data_list)) {
  preprocess_data_list[[session_name]] <- preprocess_dbtables(gamesession_data_list[[session_name]], session_name, excel = FALSE)
}

dbtable_list <- preprocess_data_list[[3]]
session_name <- names(preprocess_data_list)[3]
  
## Unpack into global environment
unpack_dbtable_list(dbtable_list, "_df")

## Check constants used in preprocessing exist
stopifnot("Default variable SELECTED_DBTABLES not found in R/constants.R" = exists(deparse(substitute(SELECTED_DBTABLES))),
          "Default variable WELFARE_LABEL_COL not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABEL_COL))),
          "Default variable CALCULATED_COSTS_DIFF not found in R/constants.R" = exists(deparse(substitute(REPORTED_CALCULATED_COSTS_DIFFCOL))),
          "Default variable TOTAL_DAMAGE_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_DAMAGE_COL))),
          "Default variable INCOME_GRP_COL not found in R/constants.R" = exists(deparse(substitute(INCOME_GRP_COL))),
          "Default variable TOTAL_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_COSTS_COL))),
          "Default variable CALCULATED_SPENDABLE_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_SPENDABLE_COL))),
          "Default variable SPENDABLE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(SPENDABLE_DIFFCOL))),
          "Default variable INCOME_LIVING_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(INCOME_LIVING_DIFFCOL))),
          "Default variable HOUSEMOVING_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(HOUSEMOVING_DIFFCOL))))

## check SELECTED_DBTABLES needed for preprocessing are all found in dbtable_list
stopifnot("Missing dbtables needed for preprocessing" = all(c("housemeasure", "playerround", "personalmeasure", "measuretype") %in% names(dbtable_list)))


# Step 2: Data Preparations ---------------------------------------------------
#Add if the player implemented house or personal measures after flood experience (either river or rain damage)in the previous round
#Control if exclude or not pre-existing house measures or initial house measures already implemented when the player buys and moves into a house
exclude_initialhousemeasure <- FALSE  # TRUE = keep only initialhousemeasure = 0; FALSE = ignore this filter
initialhousemeasure_cond <- if (exclude_initialhousemeasure) paste0("is_initialhousemeasure", " = 0 AND ") else NULL
player_code_cond <- paste0("player_code", " IS NOT NULL")
round_number_cond <- paste0("groupround_round_number", " > 0")

# Keep this action on hold until the code review is concluded
# playerround_temp_df <- playerround_df
# playerround_temp_df[,"groupround_round_number"] <- playerround_temp_df[,"groupround_round_number"] + 1
# 
# playerround_temp_df <- sqldf::sqldf(unnull_dbcol_sqlquery(playerround_temp_df, "cost_fluvial_damage", coal_val = 0))
# playerround_temp_df <- sqldf::sqldf(unnull_dbcol_sqlquery(playerround_temp_df, "cost_pluvial_damage", coal_val = 0))
# playerround_temp_df <- sqldf::sqldf(unnull_dbcol_sqlquery(playerround_temp_df, "total_damage_costs", coal_val = 0))
# 
# 
# housemeasure_filtered_df <- sqldf::sqldf(left_join_sqlquery(housemeasure_df, c("player_code", "groupround_round_number"),
#                                                             playerround_temp_df, c("player_code", "groupround_round_number"),
#                                                             kept_dbtable1_cols = c("gamesession_name", "id", "measuretype_id",
#                                                                                    "group_name", "player_code", "house_code",
#                                                                                    "groupround_round_number", "round_income",
#                                                                                    "short_alias", "cost_absolute",
#                                                                                    "satisfaction_delta_once",
#                                                                                    "pluvial_protection_delta",
#                                                                                    "fluvial_protection_delta",
#                                                                                    "is_initialhousemeasure"),
#                                                             kept_dbtable2_cols = c("cost_fluvial_damage",
#                                                                                    "cost_pluvial_damage",
#                                                                                    "total_damage_costs"),
#                                                             is_where = TRUE,
#                                                             where_cond = paste(
#                                                               paste0("dbtable1.", c(initialhousemeasure_cond,
#                                                                                     player_code_cond,
#                                                                                     round_number_cond)
#                                                               ),
#                                                               collapse = " AND ")
# )
# )
# 
# 
# housemeasure_filtered_df <- sqldf::sqldf(select_sqlquery(housemeasure_filtered_df,
#                                                          names(housemeasure_filtered_df)[names(housemeasure_filtered_df) %in% "is_initialhousemeasure" == F])
#                                          )
 

housemeasure_filtered_df <- sqldf::sqldf(select_sqlquery(housemeasure_df, c("id", "measuretype_id", "group_name",
                                                                            "player_code", "house_code",
                                                                            "groupround_round_number", "round_income",
                                                                            "short_alias", "cost_absolute",
                                                                            "satisfaction_delta_once",
                                                                            "pluvial_protection_delta",
                                                                            "fluvial_protection_delta"),
                                                         is_where = TRUE,
                                                         where_cond = paste(c(initialhousemeasure_cond,
                                                                              player_code_cond),
                                                                            collapse = " AND ")
)
)

housemeasure_filtered_df <- sqldf::sqldf(rename_cols_sqlquery(housemeasure_filtered_df, "cost_absolute", "measure_cost"))

 
# personalmeasure_filtered_df <- sqldf::sqldf(left_join_sqlquery(personalmeasure_df, c("player_code", "groupround_round_number"),
#                                                             playerround_temp_df, c("player_code", "groupround_round_number"),
#                                                             kept_dbtable1_cols = c("gamesession_name", "id", "measuretype_id",
#                                                                                    "group_name", "player_code", "house_code",
#                                                                                    "groupround_round_number", "round_income",
#                                                                                    "short_alias", "calculated_costs",
#                                                                                    "satisfaction_delta_once",
#                                                                                    "pluvial_protection_delta",
#                                                                                    "fluvial_protection_delta",
#                                                                                    "is_initialhousemeasure"),
#                                                             kept_dbtable2_cols = c("cost_fluvial_damage",
#                                                                                    "cost_pluvial_damage",
#                                                                                    "total_damage_costs")
#                                                             )
#                                             )
# 
# 
# personalmeasure_filtered_df <- sqldf::sqldf(select_sqlquery(personalmeasure_filtered_df,
#                                                          names(personalmeasure_filtered_df)[names(personalmeasure_filtered_df) %in% "is_initialhousemeasure" == F])
#                                          )


personalmeasure_filtered_df <- sqldf::sqldf(select_sqlquery(personalmeasure_df, c("id", "measuretype_id", "group_name",
                                                                                  "player_code", "house_code",
                                                                                  "groupround_round_number", "round_income",
                                                                                  "short_alias", "calculated_costs",
                                                                                  "satisfaction_delta_once",
                                                                                  "pluvial_protection_delta",
                                                                                  "fluvial_protection_delta")))

personalmeasure_filtered_df <- sqldf::sqldf(rename_cols_sqlquery(personalmeasure_filtered_df, "calculated_costs", "measure_cost"))


# Add a source column to each measures table and combine them
measures_combined_df <- sqldf::sqldf(union_all_sqlquery(personalmeasure_filtered_df, housemeasure_filtered_df,
                                                        source_col = "source", source_label_dbtable1 ="personalmeasure_filtered", source_label_dbtable2 = "housemeasure_filtered")
                                     )
  

# Step 3: Variables to plot calculation ---------------------------------------------------

#  Define the order to plot the measures

measuretype_df <- sqldf::sqldf(left_join_sqlquery(measuretype_df, match_dbtable1_cols = "short_alias",
                                                  MEASURETEXT_DF, match_dbtable2_cols = "short_alias",
                                                  kept_dbtable1_cols = c("short_alias", "cost_absolute", "cost_percentage_income", "cost_percentage_house"),
                                                  kept_dbtable2_cols = c(MEASURE_COSTREF_COL, MEASURE_COSTPLOT_COL, MEASURE_ICONS_COL))
)

measuretype_df <- sqldf::sqldf(sort_dbtable_sqlquery(measuretype_df, MEASURE_COSTPLOT_COL))
measuretype_df <- sqldf::sqldf(sort_dbtable_sqlquery(measuretype_df, "cost_absolute", asc = FALSE))


# Keep this action on hold until the code review is concluded
# measuretype_df <- measuretype_df %>%
#   mutate(
#     cost_absolute = ifelse(cost_absolute == 0, 1, 2
#     )
#   )

# Step 1: Aggregate counts per round and measure type
measures_combined_counts <- measures_combined %>%
  group_by(groupround_round_number, short_alias) %>%
  summarise(count = n(), .groups = "drop")

#create a new column in R that concatenates the absolute cost (if non‑zero) and the percentage cost (if non‑zero) together with the cost reference. 
measuretype <- measuretype %>%
  mutate(
    cost_info = case_when(
      cost_absolute != 0 ~ paste0(cost_absolute/1000, "k"),
      cost_percentage_income != 0 ~ paste0(cost_percentage_income, "% income"),
      cost_percentage_house != 0 ~ paste0(cost_percentage_house, "% house cost"),
      TRUE ~ "No cost"
    )
  )

# # Assuming measures_combined and measuretype data frames are in your R session
# # Start from your measures_combined data frame
# measures_combined_counts <- measures_combined %>%
#   mutate(
#     # Your three requested cases:
#     case_both_prot_prev_total = (pluvial_protection_delta > 0 &
#                                    fluvial_protection_delta > 0 &
#                                    prev_total_damage > 0),
#     
#     case_pluvial_prev_pluvial = (pluvial_protection_delta > 0 &
#                                    prev_cost_pluvial_damage > 0),
#     
#     case_fluvial_prev_fluvial = (fluvial_protection_delta > 0 &
#                                    prev_cost_fluvial_damage > 0),
#     
#     # Remaining = NOT in any of the three conditions
#     case_remaining = !(
#       case_both_prot_prev_total |
#         case_pluvial_prev_pluvial |
#         case_fluvial_prev_fluvial
#     )
#   ) %>%
#   # Group per round and measure type
#   group_by(groupround_round_number, short_alias) %>%
#   summarise(
#     # Total implementations per round × measure type
#     count_total_implementations             = n(),
#     
#     # Three bucket counts per round × measure type
#     count_both_protection_prev_total_damage = sum(case_both_prot_prev_total,   na.rm = TRUE),
#     count_pluvial_prev_pluvial_damage       = sum(case_pluvial_prev_pluvial,   na.rm = TRUE),
#     count_fluvial_prev_fluvial_damage       = sum(case_fluvial_prev_fluvial,   na.rm = TRUE),
#     
#     # Remaining bucket
#     count_remaining                         = sum(case_remaining,              na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   # ---- Optional sanity check columns (per round × measure type) ----
# mutate(
#   check_sum = count_both_protection_prev_total_damage +
#     count_pluvial_prev_pluvial_damage +
#     count_fluvial_prev_fluvial_damage +
#     count_remaining,
#   all_good  = (check_sum == count_total_implementations))
# 
# Keep your preferred plotting order (reverse of measuretype$short# Keep your preferred plotting order (reverse of measuretype$short_alias)
measures_combined_counts$short_alias <- factor(
  measures_combined_counts$short_alias,
  levels = rev(measuretype$short_alias)
)
# Ensure every row in measures_combined_counts has the correct icon according to its short_alias
measures_combined_counts <- measures_combined_counts %>%
  left_join(measuretype %>% select(short_alias, icons_path,cost_info), by = "short_alias")

# On Windows, this should open the image in your default viewer
shell.exec(normalizePath(measures_combined_counts$icons_path[1], winslash = "/", mustWork = TRUE))

# Set the factor level order based on groupround_round_number
measures_combined_counts$groupround_round_number <- factor(
  measures_combined_counts$groupround_round_number,
  levels = rev(sort(unique(measures_combined_counts$groupround_round_number)))# 1 → 2 → 3 …
)

# Improvements distribution specification ---------------------------------------------------
# Create a list with the tables used in the calculation
list_improv_dist <- list(
  measures_combined = measures_combined,
  measures_combined_counts = measures_combined_counts,
  measuretype = measuretype,
  personalmeasure = personalmeasure,
  housemeasure = housemeasure,
  questionscore = questionscore,
  questionitem = questionitem,
  initialhousemeasure = initialhousemeasure,
  house = house,
  housegroup = housegroup,
  group = group,
  groupround = groupround,
  player = player
)