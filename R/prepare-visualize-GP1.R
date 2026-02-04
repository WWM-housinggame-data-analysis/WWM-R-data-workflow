# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here(file.path(FUNCTION_PATH, "plot-data.R")))

# Reactive plot based on user input
prepare_visualize_GP1 <- function(df, stacked_vec, selected_table, game_round, fill_values_all, fill_labels_all) {
  
  # Guard against empty states
  req(nrow(df) > 0, length(stacked_vec) > 0)
  
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
  ave_data <- retrieve_mean_table(df, "xlabels", "satisfaction_total", "Average total satisfaction")
  
  # stacked costs
  summary_df <- retrieve_summary_table(df, stacked_vec, "xlabels")
  
  # x order (critical for consistent stacking + line alignment)
  bar_xlevels <- if (is.factor(summary_df$xlabels)) levels(summary_df$xlabels) else unique(summary_df$xlabels)
  
  scatter_xlevels <- if (is.factor(ave_data$xlabels)) levels(ave_data$xlabels) else unique(ave_data$xlabels)
  
  # Ensure ordering matches for all traces
  df <- df %>%
    mutate(xlabels = factor(xlabels, levels = xlevels)) %>%
    arrange(xlabels)
  
  ave <- ave %>%
    mutate(xlabels = factor(xlabels, levels = xlevels)) %>%
    arrange(xlabels)
  
  # Convert bars to k for left axis # ---- ensure negatives for "spent savings" (EDIT this code to match your real cost_type) ----
  df <- df %>%
    mutate(
      #mean_value = if_else(cost_type == "spent_savings", -abs(mean_value), mean_value),
      mean_k = mean_value / K_FACTOR
    )
  
  list(
    summary_df  = summary_df,     # has xlabels, cost_type, mean_value, n, ...
    ave_data    = ave_data,       # has xlabels, ave_satisfaction, series
    stacked_vec = stacked_vec,
    xlevels     = xlevels
  )
}
