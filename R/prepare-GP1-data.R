# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here(file.path(FUNCTION_PATH, "plot-data.R")))

# Reactive plot based on user input
prepare_GP1_data <- function(df, selected_columns, selected_table, game_round, fill_values_all) {
  
  # Guard against empty states
  req(nrow(df) > 0, length(selected_columns) > 0)
  
  if (identical(selected_table, "All")) {
    selected_table <- as.character(unique(df$group_name))
  }
  
  group_col <- update_group_col(df, selected_table)
  
  # Build xlabels on the row-level data
  if (identical(group_col, "player_code")) {
    
    df <- df %>% filter(group_name %in% selected_table) %>% droplevels()
  }
  
  df <- create_GP1_xlabels(df, group_col)
  
  if (game_round %in% INTERM_ROUNDS) {
    
    df <- df %>% filter(groupround_round_number %in% game_round) %>% droplevels()
  }
  
  # satisfaction series
  scatter_df <- retrieve_mean_table(df, "xlabels", "satisfaction_total", "Average total satisfaction")
  
  # stacked costs
  
  selected_bar_segments <- names(EXPENSE_BARCOLS)[EXPENSE_BARCOLS %in% selected_columns]
  
  bar_df <- retrieve_mean_table(df, "xlabels", selected_columns, selected_bar_segments)
  
  # x order (critical for consistent stacking + line alignment)
  bar_xlevels <- if (is.factor(bar_df$xlabels)) levels(bar_df$xlabels) else unique(bar_df$xlabels)
  
  scatter_xlevels <- if (is.factor(scatter_df$xlabels)) levels(scatter_df$xlabels) else unique(scatter_df$xlabels)
  
  stopifnot(identical(bar_xlevels, scatter_xlevels))
  
  xlevels <- scatter_xlevels
  
  # Ensure ordering matches for all traces
  bar_df <- bar_df %>%
    mutate(xlabels = factor(xlabels, levels = xlevels)) %>%
    arrange(xlabels)
  
  scatter_df <- scatter_df %>%
    mutate(xlabels = factor(xlabels, levels = xlevels)) %>%
    arrange(xlabels)
  
  # Convert bars to k for left axis # ---- ensure negatives for "spent savings" (EDIT this code to match your real cost_type) ----
  bar_df <- bar_df %>%
    mutate(
      #mean_value = if_else(cost_type == "spent_savings", -abs(mean_value), mean_value),
      mean_k = mean_value / K_FACTOR
    )
  
  list(
    bar_df                = bar_df,     # has xlabels, cost_type, mean_value, n, ...
    scatter_df            = scatter_df,       # has xlabels, ave_satisfaction, series
    selected_bar_segments = selected_bar_segments,
    xlevels               = xlevels
  )
}
