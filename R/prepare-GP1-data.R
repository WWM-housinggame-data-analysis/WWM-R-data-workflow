#R/prepare-GP1-data.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))
source(here::here(file.path(FUNCTION_PATH, "transform-data.R")))


# Reactive plot based on user input
prepare_GP1_data <- function(df, selected_cost_types, selected_table, game_round, fill_values_all) {
  
  
  # Normalize ROUND NUMBER — avoids mixed numeric/character axis
  df[, ROUND_NUMBER_COL] <- as.character(df[, ROUND_NUMBER_COL])
  
  
  # selected_cost_types() already normalized. Still filter to known keys.
  selected_bar_segments <-
    COST_BAR_SEGMENTS[names(COST_BAR_SEGMENTS) %in% filter_selected_categs(selected_cost_types,
                                                                           c(SELECT_ALL, names(COST_BAR_SEGMENTS)))]
  names(selected_bar_segments) <-
    names(COST_BAR_SEGMENTS)[names(COST_BAR_SEGMENTS) %in% filter_selected_categs(selected_cost_types,
                                                                           c(SELECT_ALL, names(COST_BAR_SEGMENTS)))]
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_bar_segments) > 0)
  
  selected_bar_groupcol <- update_bar_groupcol(df, selected_table)
  
  # Build xlabels on the row-level data
  df <- filter_tables(df, selected_bar_groupcol, selected_table)
  
  df <- create_GP1_xlabels(df, selected_bar_groupcol)
  
  
  # Normalize xlabels BEFORE filtering rounds
  df[, GP1_XLABEL_COL] <- as.character(df[, GP1_XLABEL_COL])
  
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
