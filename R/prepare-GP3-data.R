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


process_GP3_dataframe <- function(df, selected_table, game_round, indiv_rounds) {
  
  df <- filter_game_rounds(df, game_round, indiv_rounds)
  
  selected_table <- translate_table_selection(df, selected_table)
  
  selected_barseg_col <- update_grouping_choice(df, selected_table)
  
  selected_bar_groupcol <- MEASURE_ALIAS_COL
  
  # Build xlabels on the row-level data
  df <- filter_tables(df, selected_barseg_col, selected_table)
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_table) > 0)
  
  list(
    df                  = df,     # has xlabels, cost_type, mean_value, n, ...
    selected_barseg_col = selected_barseg_col
  )
  
}


retrieve_GP3_plot_data <- function(df, selected_table, game_round, indiv_rounds) {
  
  processed_list <- process_GP3_dataframe(df, selected_table, game_round, indiv_rounds)
  df <- processed_list$df
  selected_barseg_col <- processed_list$selected_barseg_col
  
  df <- create_GP3_barseg_labels(df, selected_barseg_col)

  n_df <- retrieve_n_table(df, c(ROUND_NUMBER_COL, MEASURE_ALIAS_COL, MEASURE_ICONS_COL, COST_INFO_COL, GP3_BARGEGLABEL_COL), "id")
  
  if (identical(game_round, SELECT_ALL)) {
    
    n_df <- retrieve_most_frequent_round(n_df, c(MEASURE_ALIAS_COL, GP3_BARGEGLABEL_COL), ROUND_NUMBER_COL, "N", FREQUENT_ROUND_COL)
  
    n_df <- aggregate_all_rounds(n_df, c(MEASURE_ALIAS_COL, MEASURE_ICONS_COL, COST_INFO_COL, GP3_BARGEGLABEL_COL), "N", FREQUENT_ROUND_COL)
    
  } else {
    
    n_df[, FREQUENT_ROUND_COL] <- NA
  }
  
  n_df <- create_GP3_ylabels(n_df)
  
  n_df <- n_df |>
    dplyr::group_by(.data[[GP3_YLABEL_COL]]) |>
    dplyr::mutate(measure_total_N = sum(N, na.rm = TRUE)) |>
    dplyr::ungroup()|>
    as.data.frame() |>
    droplevels()
  
  ylevels <- levels(n_df[, GP3_YLABEL_COL])
  
  list(
    n_df            = n_df,
    barlevels       = ylevels,
    grouping_choice = selected_barseg_col
  )
}


retrieve_GP3_summary_tables <- function(df, selected_table, game_round, indiv_rounds) {

  processed_list <- process_GP3_dataframe(df, selected_table, game_round, indiv_rounds)
  
  df <- processed_list$df

  selected_group_col <- processed_list$selected_barseg_col
  
  n_df <- retrieve_n_table(df, c(ROUND_NUMBER_COL, MEASURE_ALIAS_COL, COST_INFO_COL, selected_group_col), "id")
  
  if (identical(game_round, SELECT_ALL)) {
    
    n_df <- retrieve_most_frequent_round(n_df, c(MEASURE_ALIAS_COL, selected_group_col), ROUND_NUMBER_COL, "N", FREQUENT_ROUND_COL)
    
    n_df <- aggregate_all_rounds(n_df, c(MEASURE_ALIAS_COL, COST_INFO_COL, selected_group_col), "N", FREQUENT_ROUND_COL)
    
  } else {
    
    n_df[, FREQUENT_ROUND_COL] <- NA
  }
  
  
  droplevels(n_df)

}

retrieve_GP3_stats_tables <- function(df, selected_table, game_round, indiv_rounds) {
  
  processed_list <- process_GP3_dataframe(df, selected_table, game_round, indiv_rounds)

  df <- processed_list$df
  
  selected_group_col <- processed_list$selected_barseg_col
  
  n_df <- retrieve_n_table(df, c(ROUND_NUMBER_COL, MEASURE_ALIAS_COL, COST_INFO_COL, selected_group_col), "id")
  
  if (identical(game_round, SELECT_ALL)) {
    
    n_df <- retrieve_most_frequent_round(n_df, c(MEASURE_ALIAS_COL, selected_group_col), ROUND_NUMBER_COL, "N", FREQUENT_ROUND_COL)
    
    n_df <- aggregate_all_rounds(n_df, c(MEASURE_ALIAS_COL, COST_INFO_COL, selected_group_col), "N", FREQUENT_ROUND_COL)
    
  } else {
    
    n_df[, FREQUENT_ROUND_COL] <- NA
  }
  
  droplevels(n_df)
  
}