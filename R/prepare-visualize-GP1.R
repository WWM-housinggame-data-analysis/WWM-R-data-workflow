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
  
  ave_data <- retrieve_average_vector(plot_data, "xlabels", "satisfaction_total", "ave_satisfaction")
  
  ave_data <- ave_data %>% mutate(series = "Average total satisfaction")
  
  plot_data <- retrieve_pivot_table(plot_data, stacked_vec)
  
  summary_df <- retrieve_summary_table(plot_data, "xlabels")
  
  bar_total <- summary_df %>%
    group_by(xlabels) %>%
    summarise(
      colsum = sum(mean_value),
      .groups    = "drop"
    ) %>%
    as.data.frame
  
  max_cost <- max(bar_total$colsum,        na.rm = TRUE)
  max_sat  <- max(ave_data$ave_satisfaction, na.rm = TRUE)
  
  if (!is.finite(max_cost) || max_cost == 0) max_cost <- 1
  if (!is.finite(max_sat)  || max_sat  == 0) max_sat  <- 1
  
  scale_factor <- max_cost / max_sat
  ave_data$ave_satisfaction_scaled <- ave_data$ave_satisfaction * scale_factor
  
  create_GP1_barplot(summary_df, ave_data, stacked_vec, fill_values_all, fill_labels_all, scale_factor)
  
}