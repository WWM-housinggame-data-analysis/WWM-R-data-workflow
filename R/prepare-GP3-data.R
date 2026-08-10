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


process_GP3_dataframe <- function(df, selected_table, game_round, interm_rounds) {
  
  df <- filter_game_rounds(df, game_round, interm_rounds)
  
  selected_table <- translate_table_selection(df, selected_table)
  
  selected_barseg_col <- update_grouping_choice(df, selected_table)
  
  selected_bar_groupcol <- MEASURE_ALIAS_COL
  
  # selected_bar_groups <- update_selected_features(selected_measure_types, MEASURE_BAR_GROUPS)
  # 
  # df <- df %>% dplyr::filter(.data[[MEASURE_ALIAS_COL]] %in% selected_bar_groups)
  
  # Build xlabels on the row-level data
  df <- filter_tables(df, selected_barseg_col, selected_table)
  
  # Guard against empty states
  shiny::req(nrow(df) > 0, length(selected_table) > 0)
  
  list(
    df                  = df,     # has xlabels, cost_type, mean_value, n, ...
    selected_barseg_col = selected_barseg_col
  )
  
}


retrieve_GP3_plot_data <- function(df, selected_table, game_round, interm_rounds) {
  
  processed_list <- process_GP3_dataframe(df, selected_table, game_round, interm_rounds)
  df <- processed_list$df
  selected_barseg_col <- processed_list$selected_barseg_col
  
  df <- create_GP3_barseg_labels(df, selected_barseg_col)

  n_df <- retrieve_n_table(df, c(ROUND_NUMBER_COL, MEASURE_ALIAS_COL, MEASURE_ICONS_COL, COST_INFO_COL, GP3_BARGEGLABEL_COL), "id")
  
  n_df <- create_GP3_ylabels(n_df)
  
  ylevels <- levels(n_df[, GP3_YLABEL_COL])
  
  list(
    n_df                = n_df,
    ylevels             = ylevels
  )
}


retrieve_GP3_summary_tables <- function(df, selected_table, game_round, interm_rounds) {

  processed_list <- process_GP3_dataframe(df, selected_table, game_round, interm_rounds)
  df <- processed_list$df

  retrieve_n_table(df, c(ROUND_NUMBER_COL, MEASURE_ALIAS_COL, COST_INFO_COL), "id")

}