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

retrieve_average_vector <- function(plot_data, group_col, in_cols, out_cols) {
  
  stopifnot(length(in_cols) == length(out_cols))
  
  ave_data <- plot_data %>%
    group_by(.data[[group_col]]) %>%
    
    summarise(
      across(all_of(in_cols), ~ round(mean(.x, na.rm = TRUE), 2),
             .names = paste0("{", deparse(substitute(out_cols)), "[match(.col, ", deparse(substitute(in_cols)), ")]}")
             ),
      .groups = "drop"
    ) %>%
    arrange(xlabels) %>%
    as.data.frame()
  
  return(ave_data)
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

retrieve_pivot_table <- function(plot_data, stacked_vec) {
  plot_data <- plot_data %>%
    pivot_longer(cols = where(is.numeric), names_to = "cost_type", values_to = "cost_value") %>%
    mutate(cost_type = factor(cost_type)) %>%
    filter(cost_type %in% stacked_vec) %>%
    droplevels() %>%
    mutate(
      cost_type  = forcats::fct_relevel(cost_type, stacked_vec),
      cost_value = as.numeric(gsub(",", "", as.character(cost_value))) # safe numeric
    )
  return(plot_data)
}

# Pre-aggregate: mean and count per bar segment (round_income × cost_type)
retrieve_summary_table <- function(plot_data, group_col) {
  
  summary_df <- plot_data %>%
    group_by(across(all_of(c(group_col, "cost_type")))) %>%
    summarise(
      mean_value = round(mean(cost_value, na.rm = TRUE), 2),
      n          = n(),
      .groups    = "drop"
    ) %>%
    as.data.frame()
  
  
  return(summary_df)
}

