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

retrieve_n_table <- function(plot_data, group_col) {
  
  if (identical(group_col, "player_code")) {
    n_data <- plot_data %>%
      select(player_code) %>%
      summarise(N = n())
    
  } else {
    n_data <- plot_data %>%
      select(all_of(c(group_col, "player_code"))) %>%
      group_by(.data[[group_col]]) %>%
      summarise(N = n())
  }
  return(n_data)
}

retrieve_pivot_table <- function(plot_data, stacked_vec, stacked_name, stacked_value) {
  plot_data <- plot_data %>%
    pivot_longer(cols = where(is.numeric), names_to = stacked_name, values_to = stacked_value) %>%
    mutate(!!stacked_name := factor(.data[[stacked_name]])) %>%
    filter(.data[[stacked_name]] %in% stacked_vec) %>%
    droplevels() %>%
    mutate(
      !!stacked_name := forcats::fct_relevel(.data[[stacked_name]], stacked_vec)
    )
  return(plot_data)
}

# Pre-aggregate: mean and count per bar segment (round_income × cost_type)
retrieve_summary_table <- function(plot_data, stacked_vec, group_col) {
  
  pivoted_data <- retrieve_pivot_table(plot_data, stacked_vec, "cost_type", "cost_value")
  
  summary_df <- pivoted_data %>%
    group_by(across(all_of(c(group_col, "cost_type")))) %>%
    summarise(
      mean_value = round(mean(cost_value, na.rm = TRUE), 2),
      n          = n(),
      .groups    = "drop"
    ) %>%
    as.data.frame()
  
  
  return(summary_df)
}

retrieve_average_vector <- function(plot_data, group_col, in_cols, out_cols, out_labels) {
  
  stopifnot(length(in_cols) == length(out_cols))
  
  pivoted_data <- retrieve_pivot_table(plot_data, in_cols, "scatter_type", "scatter_value")
  
  ave_data <- pivoted_data %>%
    group_by(across(all_of(c(group_col, "scatter_type")))) %>%
    
    summarise(
      mean_value = round(mean(scatter_value, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(xlabels)  %>%
    mutate(label = out_labels[out_cols]) %>%
    as.data.frame()
  
  return(ave_data)
}

