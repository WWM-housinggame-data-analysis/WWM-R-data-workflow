#R/prepare-GP2-data.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))
source(here::here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here::here(file.path(FUNCTION_PATH, "format-add-cols.R")))


retrieve_GP2_dataframe <- function(df) {
  
  # -----------------------------------------------------------
  # tidyverse operations
  # -----------------------------------------------------------
  
  ## Convert INCOME_DIST_CATEGCOLS to factor
  df <- df |>
    dplyr::mutate_at(INCOME_DIST_CATEGCOLS, as.factor)
  
  
  ## Append income_grp labels based on round_income to dataframe
  df <- append_income_grp(df, INCOME_GRP_COL)
  
  
  ## Convert columns not in INCOME_DIST_CATEGCOLS nor INCOME_GRP_COL to numeric
  df <- df |>
    dplyr::mutate_at(
      names(df)[!(names(df) %in% c(INCOME_DIST_CATEGCOLS, INCOME_GRP_COL))],
      as.numeric
    )
  
  
  ## Calculate the round costs to check the spendable income
  df <- append_total_costs(df, TOTAL_COSTS_COL)
  
  
  ## Calculate the spendable income
  df <- append_spendable_income_cols(df, CALCULATED_SPENDABLE_COL, SPENDABLE_DIFFCOL)
  
  
  ## Calculate income - living costs
  df <- append_income_living_diff(df, INCOME_LIVING_DIFFCOL)
  
  
  ## Calculate  "profit - spent savings house moving"
  df <- append_housemoving_diff(df, HOUSEMOVING_DIFFCOL)
  
  return(df)
}

process_GP2_dataframe <- function(df, selected_cost_types, selected_table, game_round, interm_rounds) {
  
  df <- filter_game_rounds(df, game_round, interm_rounds)
  
  selected_table <- translate_table_selection(df, selected_table)
  
  selected_bar_groupcol <- update_grouping_choice(df, selected_table)
  
  # Build xlabels on the row-level data
  df <- filter_tables(df, selected_bar_groupcol, selected_table)
  
  df <- create_GP2_xlabels(df, selected_bar_groupcol)
  
  # selected_cost_types() already normalized. Still filter to known keys.
  selected_bar_segments <- update_selected_features(selected_cost_types, COST_BAR_SEGMENTS)
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_bar_segments) > 0, length(selected_table) > 0)
  
  list(
    df                    = df,     # has xlabels, cost_type, mean_value, n, ...
    selected_bar_segments = selected_bar_segments,
    selected_bar_groupcol = selected_bar_groupcol
  )
  
}

# Reactive plot based on user input
retrieve_GP2_plot_data <- function(df, selected_cost_types, selected_table, game_round, interm_rounds, fill_values_all) {
  
  processed_list <- process_GP2_dataframe(df, selected_cost_types, selected_table, game_round, interm_rounds)
  df <- processed_list$df
  selected_bar_segments <- processed_list$selected_bar_segments

  # satisfaction series
  scatter_df <- retrieve_mean_table(df, GP2_XLABEL_COL, COST_SCATTER_LINE)
  
  # stacked costs
  bar_df <- retrieve_mean_table(df, GP2_XLABEL_COL, selected_bar_segments)
  
  xlevels <- levels(bar_df[, GP2_XLABEL_COL])
  
  # Make factor levels consistent between both data frames
  bar_df[, GP2_XLABEL_COL] <- factor(bar_df[, GP2_XLABEL_COL])
  scatter_df[, GP2_XLABEL_COL] <- factor(scatter_df[, GP2_XLABEL_COL],
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


retrieve_GP2_summary_tables <- function(df, selected_cost_types, selected_table, game_round, interm_rounds, selected_bar_groupcol = GP2_XLABEL_COL, pivoted_cols = COST_TABLE_ENTRIES) {
  
  processed_list <- process_GP2_dataframe(df, selected_cost_types, selected_table, game_round, interm_rounds)
  df <- processed_list$df
  selected_bar_groupcol <- processed_list$selected_bar_groupcol
  
  pivoted_mean_df <- retrieve_mean_table(df, selected_bar_groupcol, pivoted_cols)
  
  num_summary_df <- pivoted_mean_df |>
    dplyr::select(-tidyselect::all_of("column_name")) |>
    tidyr::pivot_wider(names_from = "mean_label", values_from = "mean_value") |>
    as.data.frame()
  
  kval_summary_df <- pivoted_mean_df |>
    dplyr::select(-tidyselect::all_of("column_name")) |>
    dplyr::mutate(
      mean_value = paste0(mean_value / K_FACTOR, names(K_FACTOR))
    ) |>
    tidyr::pivot_wider(names_from = "mean_label", values_from = "mean_value") |>
    as.data.frame()
  
  list(num_df = num_summary_df,
       kval_df = kval_summary_df)
  
}