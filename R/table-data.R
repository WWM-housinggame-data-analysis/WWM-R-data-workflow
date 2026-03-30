#R/table-data
# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "transform-data.R")))

retrieve_summary_table <- function(df, selected_table) {
  
  group_col <- update_bar_groupcol(df, selected_table)
  
  df <- create_GP1_xlabels(df, group_col)
  
  pivoted_mean_df <- retrieve_mean_table(df, GP1_XLABEL_COL, COST_TABLE_ENTRIES)
  
  summary_df <- pivoted_mean_df |>
    dplyr::select(-tidyselect::all_of("column_name")) |>
    tidyr::pivot_wider(names_from = "mean_label", values_from = "mean_value") |>
    as.data.frame()
  
  return(summary_df)
  
}