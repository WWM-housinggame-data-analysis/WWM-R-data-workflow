# R/transform-data.R
# Filter and prepare just before plotting

# Build xlabels on the row-level data
create_GP1_xlabels <- function(plot_data, group_col) {
  
  if (identical(group_col, "income_grp")) {
    
    plot_data <- plot_data |>
      dplyr::mutate(
        xlabels = factor(
          paste(WELFARE_LABELS[match(rlang::.data[[group_col]], names(WELFARE_LABELS))], rlang::.data[[group_col]], sep = "<br>"),
          levels = paste(WELFARE_LABELS, names(WELFARE_LABELS), sep = "<br>")
        )
      )
    
  } else if (identical(group_col, "player_code")) {
    
    plot_data <- plot_data |>
      dplyr::mutate(
        xlabels = factor(
          paste(rlang::.data[[group_col]], rlang::.data[["income_grp"]], sep = "<br>"),
          levels = paste(rlang::.data[["player_code"]][match(names(WELFARE_LABELS), rlang::.data[["income_grp"]])], names(WELFARE_LABELS), sep = "<br>")
        )
      )
  }
  
  return(plot_data)
}

retrieve_n_table <- function(df, group_col, id_col = "player_code") {
  
  if (identical(group_col, id_col)) {
    n_df <- df |>
      dplyr::select(tidyselect::all_of(id_col)) |>
      dplyr::summarise(N = dplyr::n())
    
  } else {
    n_df <- df |>
      dplyr::select(tidyselect::all_of(c(group_col, id_col))) |>
      dplyr::group_by(rlang::.data[[group_col]]) |>
      dplyr::summarise(N = dplyr::n())
  }
  return(n_df)
}

retrieve_pivot_table <- function(df, selected_columns, column_name, column_value) {
  
  pivoted_df <- df |>
    
    tidyr::pivot_longer(cols = tidyselect::where(is.numeric), names_to = column_name, values_to = column_value) |>
    
    dplyr::mutate(!!column_name := factor(rlang::.data[[column_name]])) |>
    
    dplyr::filter(rlang::.data[[column_name]] %in% selected_columns) |>
    
    droplevels() |>
    
    dplyr::mutate(
      !!column_name := forcats::fct_relevel(rlang::.data[[column_name]], selected_columns)
    )
  return(pivoted_df)
}

retrieve_mean_table <- function(df, group_col, in_cols, out_cols) {
  
  stopifnot(length(in_cols) == length(out_cols))
  
  names(in_cols) <- out_cols
  
  lookup <- tibble::enframe(in_cols, name = "mean_label", value = "column_name")
  
  pivoted_df <- retrieve_pivot_table(df, in_cols, "column_name", "column_value")
  
  mean_df <- pivoted_df |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_col, "column_name")))) |>
    
    dplyr::summarise(
      mean_value = round(mean(rlang::.data[["column_value"]], na.rm = TRUE), 2),
      N          = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(lookup, by = "column_name") |>
    dplyr::arrange(xlabels) |>
    as.data.frame()
  
  return(mean_df)
}


