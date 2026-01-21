# Build plot on the aggregated data (geom_col)
create_barplot <- function(summary_df, stacked_vec, fill_values_all, fill_labels_all, xlabels) {
  gp <- ggplot(summary_df) +
    
    geom_col(aes(x = income_grp, y = mean_value, fill = cost_type),
             position = "stack", na.rm = TRUE, width = w) +
    
    scale_fill_manual(
      name = "Round costs",
      values = fill_values_all[stacked_vec],
      labels = fill_labels_all[stacked_vec]
    ) +
    
    guides(fill = guide_legend(title = "Round costs")) +
    scale_y_continuous(labels = function(y) y / 1000, name = "Game Currency (k)") +
    scale_x_discrete(name = "Round income (k) \n Players per class", labels = xlabels) +
    
    theme_minimal() +
    theme(axis.text.x = element_markdown(angle = 0, hjust = 0.5)) ##takes rich html
  
  return(list(plot = gp, data = summary_df, barfill = stacked_vec))
}

# Reactive plot based on user input
get_costs_barplot <- function(input_data_reactive, stacked_vars_reactive, selected_players_reactive) {
    
    # Pull the latest data and selection from the reactives
    plot_data   <- input_data_reactive()
    stacked_vec <- stacked_vars_reactive()
    selected_players_vec <-selected_players_reactive()
    
    # Guard against empty states
    req(nrow(plot_data) > 0, length(stacked_vec) > 0)
    
    xlabels <- paste(sort(unique(plot_data$round_income/1000)), "k", sep="")
    
    
    plot_data <- retrieve_pivot_table(plot_data)
    
    summary_df <- retrieve_summary_table(plot_data)
    
    create_barplot(summary_df, stacked_vec, fill_values_all, fill_labels_all, xlabels)
  
}

create_hovering <- function(data_fill, hovering_datalist) {

  data_fill$customdata <- do.call(cbind, hovering_datalist)
  
  data_fill$hovertemplate <- "<b>%{fullData.name}</b><br>"
  
  if ("value_k" %in% names(hovering_datalist)) {
    
    data_fill$hovertemplate <- paste0(data_fill$hovertemplate,
                                      paste0("Mean: %{customdata[",
                                             which(names(hovering_datalist) %in% "value_k") - 1,
                                             "]:.2f}k<br>"))
  }
  
  if ("n_vec" %in% names(hovering_datalist)){
    
    data_fill$hovertemplate <- paste0(data_fill$hovertemplate,
                                      paste0("N: %{customdata[",
                                             which(names(hovering_datalist) %in% "n_vec") - 1,
                                             "]}"))
  }
  
  data_fill$hovertemplate <- paste0(data_fill$hovertemplate, "<extra></extra>")
  
  return(data_fill)
  
}