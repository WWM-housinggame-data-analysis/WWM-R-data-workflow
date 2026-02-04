# Filter and prepare just before plotting

# Build xlabels on the row-level data
create_GP1_xlabels <- function(plot_data, group_col) {
  
  if (identical(group_col, "income_grp")) {
    
    plot_data <- plot_data %>%
      mutate(
        xlabels = factor(
          paste(WELFARE_LABELS[match(.data[[group_col]], names(WELFARE_LABELS))], .data[[group_col]], sep = "<br>"),
          levels = paste(WELFARE_LABELS, names(WELFARE_LABELS), sep = "<br>")
        )
      )
    
  } else if (identical(group_col, "player_code")) {
    
    plot_data <- plot_data %>%
      mutate(
        xlabels = factor(
          paste(.data[[group_col]], .data[["income_grp"]], sep = "<br>"),
          levels = paste(.data[["player_code"]][match(names(WELFARE_LABELS), .data[["income_grp"]])], names(WELFARE_LABELS), sep = "<br>")
        )
      )
  }
  
  return(plot_data)
}

retrieve_n_table <- function(df, group_col, id_col = "player_code") {
  
  if (identical(group_col, id_col)) {
    n_df <- df %>%
      select(all_of(id_col)) %>%
      summarise(N = n())
    
  } else {
    n_df <- df %>%
      select(all_of(c(group_col, id_col))) %>%
      group_by(.data[[group_col]]) %>%
      summarise(N = n())
  }
  return(n_df)
}

retrieve_pivot_table <- function(df, selected_columns, column_name, column_value) {
  
  pivoted_df <- df %>%
    
    pivot_longer(cols = where(is.numeric), names_to = column_name, values_to = column_value) %>%
    
    mutate(!!column_name := factor(.data[[column_name]])) %>%
    
    filter(.data[[column_name]] %in% selected_columns) %>%
    
    droplevels() %>%
    
    mutate(
      !!column_name := forcats::fct_relevel(.data[[column_name]], selected_columns)
    )
  return(pivoted_df)
}

retrieve_mean_table <- function(df, group_col, in_cols, out_cols) {
  
  stopifnot(length(in_cols) == length(out_cols))
  
  names(in_cols) <- out_cols
  
  lookup <- enframe(in_cols, name = "mean_label", value = "column_name")
  
  pivoted_df <- retrieve_pivot_table(df, in_cols, "column_name", "column_value")
  
  mean_df <- pivoted_df %>%
    group_by(across(all_of(c(group_col, "column_name")))) %>%
    
    summarise(
      mean_value = round(mean(.data[["column_value"]], na.rm = TRUE), 2),
      N          = n(),
      .groups = "drop"
    ) %>%
    left_join(lookup, by = "column_name") %>%
    arrange(xlabels) %>%
    as.data.frame()
  
  return(mean_df)
}


