# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here(file.path(FUNCTION_PATH, "plot-data.R")))

# Reactive plot based on user input
prepare_visualize_GP1 <- function(plot_data, stacked_vec, selected_table, game_round, fill_values_all, fill_labels_all) {
  
  # Guard against empty states
  req(nrow(plot_data) > 0, length(stacked_vec) > 0)
  
  group_col <- update_group_col(plot_data, selected_table)
  
  # Build xlabels on the row-level data
  if (identical(group_col, "player_code")) {
    
    plot_data <- plot_data %>% filter(group_name %in% selected_table) %>% droplevels()
  }
  
  plot_data <- create_GP1_xlabels(plot_data, group_col)
  
  if (game_round %in% INTERM_ROUNDS) {
    
    plot_data <- plot_data %>% filter(groupround_round_number %in% game_round) %>% droplevels()
  }
  
  # satisfaction series
  ave_data <- retrieve_average_vector(plot_data, "xlabels", "satisfaction_total", "ave_satisfaction") %>%
    mutate(series = "Average total satisfaction")
  
  # stacked costs
  plot_data <- retrieve_pivot_table(plot_data, stacked_vec)
  summary_df <- retrieve_summary_table(plot_data, "xlabels")
  
  # x order (critical for consistent stacking + line alignment)
  xlevels <- if (is.factor(summary_df$xlabels)) levels(summary_df$xlabels) else unique(summary_df$xlabels)
  
  # keep only colors/labels for selected stacks
  fill_values <- fill_values_all[names(fill_values_all) %in% stacked_vec]
  fill_labels <- fill_labels_all[names(fill_labels_all) %in% stacked_vec]
  
  list(
    summary_df  = summary_df,     # has xlabels, cost_type, mean_value, n, ...
    ave_data    = ave_data,       # has xlabels, ave_satisfaction, series
    stacked_vec = stacked_vec,
    xlevels     = xlevels,
    fill_values = fill_values,
    fill_labels = fill_labels
  )
}
