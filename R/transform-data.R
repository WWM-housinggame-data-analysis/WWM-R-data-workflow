# R/transform-data.R
# Filter and prepare just before plotting

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "interact-data.R")))

# Build xlabels on the row-level data
filter_tables <- function(df, groupcol, selected_table) {
  
  shiny::req(nrow(df) > 0)
  
  if (identical(groupcol, PLAYER_CODE_COL)) {
    df <- df |>
      dplyr::filter(.data[[TABLE_GROUPCOL]] %in% selected_table) |>
      droplevels()
  }
  
  shiny::req(nrow(df) > 0)
  
  return(df)
}


# Build xlabels on the row-level data
create_GP2_xlabels <- function(df, group_col) {
  
  if (identical(group_col, INCOME_GRP_COL)) {
    
    df <- df |>
      dplyr::mutate(
        !!GP2_XLABEL_COL := factor(
          paste(WELFARE_LABELS[match(.data[[group_col]], names(WELFARE_LABELS))], .data[[group_col]], sep = LINEBREAK),
          levels = paste(WELFARE_LABELS, names(WELFARE_LABELS), sep = LINEBREAK)
        )
      )
    
  } else if (identical(group_col, PLAYER_CODE_COL)) {
    
    df <- df |>
      dplyr::mutate(
        !!GP2_XLABEL_COL := factor(
          paste(.data[[group_col]], .data[[INCOME_GRP_COL]], sep = LINEBREAK),
          levels = paste(.data[[PLAYER_CODE_COL]][match(names(WELFARE_LABELS), .data[[INCOME_GRP_COL]])], names(WELFARE_LABELS), sep = LINEBREAK)
        )
      )
  }
  
  return(df)
}


# Build xlabels on the row-level data
create_GP3_ylabels <- function(df) {
  
  df <- df |>
    dplyr::mutate(
      !!GP3_YLABEL_COL := factor(
        paste(.data[[MEASURE_ALIAS_COL]], paste0("(", .data[[COST_INFO_COL]], ")"), sep = " - "),
        levels = paste(MEASURE_ALIASES,
                       paste0("(", .data[[COST_INFO_COL]][match(MEASURE_ALIASES, .data[[MEASURE_ALIAS_COL]])] , ")"),
                       sep = " - ")
      )
    )
  
  return(df)
}


# Build xlabels on the row-level data
create_GP3_barseg_labels <- function(df, group_col) {
  
  if (identical(group_col, INCOME_GRP_COL)) {
    
    df <- df |>
      dplyr::mutate(
        !!GP3_BARGEGLABEL_COL := factor(
          paste(WELFARE_LABELS[match(.data[[group_col]], names(WELFARE_LABELS))], .data[[group_col]], sep = " - "),
          levels = paste(WELFARE_LABELS, names(WELFARE_LABELS), sep = " - ")
        )
      )
    
  } else if (identical(group_col, PLAYER_CODE_COL)) {
    
    df <- df |>
      dplyr::mutate(
        !!GP3_BARGEGLABEL_COL := factor(
          paste(.data[[group_col]], .data[[INCOME_GRP_COL]], sep = " - "),
          levels = paste(.data[[PLAYER_CODE_COL]][match(names(WELFARE_LABELS), .data[[INCOME_GRP_COL]])], names(WELFARE_LABELS), sep = " - ")
        )
      )
  }
  
  return(df)
}

filter_game_rounds <- function(df, game_round, interm_rounds) {
  
  shiny::req(nrow(df) > 0)
  
  if (game_round %in% interm_rounds) {
    
    df <- df |>
      dplyr::filter(.data[[ROUND_NUMBER_COL]] %in% game_round) |>
      droplevels()
  }
  
  shiny::req(nrow(df) > 0)
  
  return(df)
}

retrieve_n_table <- function(df, group_cols, id_col = "player_code") {
  
  if (identical(group_cols, id_col)) {
    n_df <- df |>
      dplyr::select(tidyselect::all_of(id_col)) |>
      dplyr::summarise(N = dplyr::n()) |>
      as.data.frame()
    
  } else {
    n_df <- df |>
      dplyr::select(tidyselect::all_of(c(group_cols, id_col))) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
      dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
      as.data.frame()
  }
  return(n_df)
}

retrieve_pivot_table <- function(df, selected_columns, column_name, column_value) {
  
  pivoted_df <- df |>
    
    tidyr::pivot_longer(cols = tidyselect::where(is.numeric), names_to = column_name, values_to = column_value) |>
    
    dplyr::mutate(!!column_name := factor(.data[[column_name]])) |>
    
    dplyr::filter(.data[[column_name]] %in% selected_columns) |>
    
    droplevels() |>
    
    dplyr::mutate(
      !!column_name := forcats::fct_relevel(.data[[column_name]], selected_columns)
    )
  
  return(pivoted_df)
}

retrieve_mean_table <- function(df, group_col, pivoted_cols) {
  
  if (is.null(pivoted_cols)) {
    names(pivoted_cols) <- pivoted_cols
    
  } else {
    names(pivoted_cols)[is.na(names(pivoted_cols))] <- pivoted_cols[is.na(names(pivoted_cols))]
  }
  
  lookup <- tibble::enframe(pivoted_cols, name = "mean_label", value = "column_name")
  
  pivoted_df <- retrieve_pivot_table(df, pivoted_cols, "column_name", "column_value")
  
  mean_df <- pivoted_df |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_col, "column_name")))) |>
    dplyr::summarise(
      mean_value = round(mean(.data[["column_value"]], na.rm = TRUE), 2),
      N          = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(lookup, by = "column_name") |>
    dplyr::arrange(.data[[group_col]]) |>
    as.data.frame()
  
  # x order (critical for consistent stacking + line alignment)
  xlevels <- if (is.factor(mean_df[, group_col])) levels(mean_df[,group_col]) else unique(mean_df[,group_col])
  
  # Ensure ordering matches for all traces
  mean_df <- mean_df |>
    dplyr::mutate(!!group_col := factor(.data[[group_col]], levels = xlevels)) |>
    dplyr::arrange(.data[[group_col]])
  
  return(mean_df)
}


