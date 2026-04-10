#R/prepare-GP1-data.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))
source(here::here(file.path(FUNCTION_PATH, "transform-data.R")))


# Reactive plot based on user input
retrieve_GP1_plot_data <- function(df, selected_cost_types, selected_table, game_round, fill_values_all) {
  
  # selected_cost_types() already normalized. Still filter to known keys.
  selected_bar_segments <- update_bar_segments(selected_cost_types)
  
  selected_table <- update_table_groups(df, selected_table)
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_bar_segments) > 0, length(selected_table) > 0)
  
  selected_bar_groupcol <- update_bar_groupcol(df, selected_table)
  
  # Build xlabels on the row-level data
  df <- filter_tables(df, selected_bar_groupcol, selected_table)
  
  df <- create_GP1_xlabels(df, selected_bar_groupcol)
  
  df <- filter_game_rounds(df, game_round)

  # satisfaction series
  scatter_df <- retrieve_mean_table(df, GP1_XLABEL_COL, COST_SCATTER_LINE)
  
  # stacked costs
  bar_df <- retrieve_mean_table(df, GP1_XLABEL_COL, selected_bar_segments)
  
  xlevels <- levels(bar_df[, GP1_XLABEL_COL])
  
  # Make factor levels consistent between both data frames
  bar_df[, GP1_XLABEL_COL] <- factor(bar_df[, GP1_XLABEL_COL])
  scatter_df[, GP1_XLABEL_COL] <- factor(scatter_df[, GP1_XLABEL_COL],
                                         levels = xlevels)
  
  
  # Convert bars to k for left axis
  bar_df <- bar_df |>
    dplyr::mutate(
      mean_k = mean_value / K_FACTOR
    )
  
  list(
    bar_df                = bar_df,     # has xlabels, cost_type, mean_value, n, ...
    scatter_df            = scatter_df,       # has xlabels, ave_satisfaction, series
    selected_bar_segments = selected_bar_segments,
    xlevels               = xlevels
  )
}


retrieve_GP1_summary_table <- function(df, selected_table, group_col = GP1_XLABEL_COL, pivoted_cols = COST_TABLE_ENTRIES) {
  
  selected_table <- update_table_groups(df, selected_table)
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_table) > 0)
  
  group_col <- update_bar_groupcol(df, selected_table)
  
  df <- create_GP1_xlabels(df, group_col)
  
  pivoted_mean_df <- retrieve_mean_table(df, group_col, pivoted_cols)
  
  summary_df <- pivoted_mean_df |>
    dplyr::select(-tidyselect::all_of("column_name")) |>
    tidyr::pivot_wider(names_from = "mean_label", values_from = "mean_value") |>
    as.data.frame()
  
  return(summary_df)
  
}