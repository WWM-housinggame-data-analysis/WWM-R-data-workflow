w = 0.9
INTERM_ROUNDS <- as.character(1:3)

# Build plot on the aggregated data (geom_col)
create_barplot <- function(summary_df, ave_data, stacked_vec, fill_values_all, fill_labels_all, group_col, xlabels) {
  
  ave_data$Index <- seq_len(nrow(ave_data))
  
  gp <- ggplot(summary_df) +
    
    geom_col(aes(x = .data[[group_col]], y = mean_value, fill = cost_type),
             position = "stack", na.rm = TRUE, width = w) +
    
    geom_line(data = ave_data %>% select(ave_Spendable),
      aes(y = ave_Spendable), #, color = "ave_Spendable"),
      linewidth = 1.2) +
    
    scale_color_manual(
      name = "Round Spendable \n Income",
      values = c(
        "ave_Spendable" = "black"),
      labels = c(
        "ave_Spendable" = "Round income - costs") 
    ) +
    
    scale_fill_manual(
      name = "Round costs",
      values = fill_values_all[stacked_vec],
      labels = fill_labels_all[stacked_vec]
    ) +
    
    guides(fill = guide_legend(title = "Round costs"),
           color = guide_legend(title = "Round Spendable Income")) +
             
    scale_y_continuous(labels = function(y) y / 1000,
                       name = "Game Currency (k)") +
      
    scale_x_discrete(name = "Round income (k) \n Players per class",
                     labels = xlabels) +
    
    theme_minimal() +
    theme(axis.text.x = element_markdown(angle = 0, hjust = 0.5)) ##takes rich html
  
  return(list(plot = gp, data = summary_df, barfill = stacked_vec))
}

# Reactive plot based on user input
get_costs_barplot <- function(input_data_reactive, input_ave_reactive, stacked_vars_reactive, selected_table_reactive, game_round, fill_values_all, fill_labels_all) {
  
  # Pull the latest data and selection from the reactives
  plot_data <- input_data_reactive()
  ave_data <- input_ave_reactive()
  stacked_vec <- stacked_vars_reactive()
  selected_table <- selected_table_reactive()
  
  # Guard against empty states
  req(nrow(plot_data) > 0, length(stacked_vec) > 0)
  
  group_col <- reactive({update_group_col(plot_data, selected_table)})
  
  xlabels <- reactive({
    
    if (identical(group_col(), "income_grp")) {
      
      paste(sort(unique(plot_data$round_income/1000)), "k", sep="")
      
    } else if (identical(group_col(), "player_code")) {
      
      sort(unique(plot_data$player_code))
      
    }})
  
  if (game_round %in% INTERM_ROUNDS) {
    plot_data <- reactive({plot_data %>% filter(groupround_round_number %in% game_round)})
    ave_data <- reactive({ave_data %>% filter(groupround_round_number %in% game_round)})
  } else {
    plot_data <- reactive({plot_data})
    ave_data <- reactive({ave_data})
  }
  
  selected_players <- reactive({as.character(unique(plot_data()$player_code))})
  
  plot_data <- reactive({retrieve_pivot_table(plot_data(), selected_players(), stacked_vec)})
  
  summary_df <- reactive({retrieve_summary_table(plot_data(), group_col())})
  
  create_barplot(summary_df(), ave_data(), stacked_vec, fill_values_all, fill_labels_all, group_col(), xlabels())
  
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