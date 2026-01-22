# Filter and prepare just before plotting

retrieve_pivot_table <- function(plot_data, selected_players_vec, stacked_vec) {
  plot_data <- plot_data %>%
    filter(player_code %in% selected_players_vec) %>%
    droplevels() %>%
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
      mean_value = mean(cost_value, na.rm = TRUE),
      n          = n(),
      .groups    = "drop"
    )
  
  return(summary_df)
}

retrieve_average_table <- function(plot_data) {
  plot_data %>%
    group_by(income_grp) %>%
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
      ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
      ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
      ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
      ave_measures = round(mean(cost_house_measures_bought, na.rm = TRUE), 2),
      ave_satisfaction = round(mean(cost_personal_measures_bought, na.rm = TRUE), 2),
      ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
      ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    ) %>%
    ungroup()
}

retrieve_n_table <- function(plot_data) {
  plot_data %>%
  select(income_grp, player_code) %>%
  group_by(income_grp) %>%
  summarise(N = n()) %>%
  ungroup()
}