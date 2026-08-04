#R/prepare-GP2-data.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))
source(here::here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here::here(file.path(FUNCTION_PATH, "format-add-cols.R")))


retrieve_GP3_dataframe <- function(df) {
  
  # -----------------------------------------------------------
  # tidyverse operations
  # -----------------------------------------------------------
  
  ## Convert INCOME_DIST_CATEGCOLS to factor
  df <- df |>
    dplyr::mutate_at(MEASURE_COMBINED_CATEGCOLS, as.factor)
  
  
  ## Append income_grp labels based on round_income to dataframe
  df <- append_income_grp(df, INCOME_GRP_COL)
  
  
  ## Convert columns not in INCOME_DIST_CATEGCOLS nor INCOME_GRP_COL to numeric
  df <- df |>
    dplyr::mutate_at(
      names(df)[!(names(df) %in% c(MEASURE_COMBINED_CATEGCOLS, INCOME_GRP_COL))],
      as.numeric
    )
  
  return(df)
}


retrieve_GP3_plot_data <- function(df, selected_table, selected_measure_types, game_round, interm_rounds) {
  
  df <- filter_game_rounds(df, game_round, interm_rounds)
  
  selected_table <- translate_table_selection(df, selected_table)
  
  selected_barseg_col <- update_grouping_choice(df, selected_table)
  
  selected_bar_groupcol <- MEASURE_ALIAS_COL
  
  # Build xlabels on the row-level data
  df <- filter_tables(df, selected_barseg_col, selected_table)
  
  # selected_cost_types() already normalized. Still filter to known keys.
  df <- create_GP3_barseg_labels(df, selected_barseg_col)
  
  selected_bar_groups <- update_selected_features(selected_measure_types, MEASURE_BAR_GROUPS)
  
  df <- df %>% dplyr::filter(.data[[MEASURE_ALIAS_COL]] %in% selected_bar_groups)
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_bar_groups) > 0, length(selected_table) > 0)
  
  n_df <- retrieve_n_table(df, c(ROUND_NUMBER_COL, MEASURE_ALIAS_COL, MEASURE_ICONS_COL, COST_INFO_COL, GP3_BARGEGLABEL_COL), "id")
  
  n_df <- create_GP3_ylabels(n_df)
  
  ylevels <- levels(n_df[, GP3_YLABEL_COL])
  
  list(
    n_df                = n_df,
    ylevels             = ylevels
  )
}

# # Reactive plot based on user input
# retrieve_GP2_plot_data <- function(df, selected_cost_types, selected_table, game_round, interm_rounds, fill_values_all) {
#   
#   # selected_cost_types() already normalized. Still filter to known keys.
#   selected_bar_segments <- update_selected_features(selected_cost_types)
#   
#   selected_table <- translate_table_selection(df, selected_table)
#   
#   # Guard against empty states
#   shiny::req(nrow(df) > 0, length(selected_bar_segments) > 0, length(selected_table) > 0)
#   
#   selected_bar_groupcol <- update_bar_groupcol(df, selected_table)
#   
#   # Build xlabels on the row-level data
#   df <- filter_tables(df, selected_bar_groupcol, selected_table)
#   
#   df <- filter_game_rounds(df, game_round, interm_rounds)
#   
#   df <- create_GP2_xlabels(df, selected_bar_groupcol)
# 
#   # satisfaction series
#   scatter_df <- retrieve_mean_table(df, GP2_XLABEL_COL, COST_SCATTER_LINE)
#   
#   # stacked costs
#   bar_df <- retrieve_mean_table(df, GP2_XLABEL_COL, selected_bar_segments)
#   
#   xlevels <- levels(bar_df[, GP2_XLABEL_COL])
#   
#   # Make factor levels consistent between both data frames
#   bar_df[, GP2_XLABEL_COL] <- factor(bar_df[, GP2_XLABEL_COL])
#   scatter_df[, GP2_XLABEL_COL] <- factor(scatter_df[, GP2_XLABEL_COL],
#                                          levels = xlevels)
#   
#   
#   # Convert bars to k for left axis
#   bar_df <- bar_df |>
#     dplyr::mutate(
#       mean_k = mean_value / K_FACTOR
#     )
#   
#   list(
#     bar_df                = bar_df,     # has xlabels, cost_type, mean_value, n, ...
#     scatter_df            = scatter_df,       # has xlabels, ave_satisfaction, series
#     selected_bar_segments = selected_bar_segments,
#     xlevels               = xlevels
#   )
# }


# retrieve_GP2_summary_tables <- function(df, selected_cost_types, selected_table, game_round, interm_rounds, selected_bar_groupcol = GP2_XLABEL_COL, pivoted_cols = COST_TABLE_ENTRIES) {
#   
#   # selected_cost_types() already normalized. Still filter to known keys.
#   selected_bar_segments <- update_selected_features(selected_cost_types)
#   
#   selected_table <- translate_table_selection(df, selected_table)
#   
#   # Guard against empty states
#   shiny::req(nrow(df) > 0, length(selected_bar_segments) > 0, length(selected_table) > 0)
#   
#   selected_bar_groupcol <- update_bar_groupcol(df, selected_table)
#   
#   # Build xlabels on the row-level data
#   df <- filter_tables(df, selected_bar_groupcol, selected_table)
#   
#   df <- create_GP2_xlabels(df, selected_bar_groupcol)
#   
#   df <- filter_game_rounds(df, game_round, interm_rounds)
#   
#   pivoted_mean_df <- retrieve_mean_table(df, selected_bar_groupcol, pivoted_cols)
#   
#   num_summary_df <- pivoted_mean_df |>
#     dplyr::select(-tidyselect::all_of("column_name")) |>
#     tidyr::pivot_wider(names_from = "mean_label", values_from = "mean_value") |>
#     as.data.frame()
#   
#   kval_summary_df <- pivoted_mean_df |>
#     dplyr::select(-tidyselect::all_of("column_name")) |>
#     dplyr::mutate(
#       mean_value = paste0(mean_value / K_FACTOR, names(K_FACTOR))
#     ) |>
#     tidyr::pivot_wider(names_from = "mean_label", values_from = "mean_value") |>
#     as.data.frame()
#   
#   list(num_df = num_summary_df,
#        kval_df = kval_summary_df)
#   
# }