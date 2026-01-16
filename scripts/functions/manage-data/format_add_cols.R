TYPE_COST_COLS <- c("cost_fluvial_damage", "cost_pluvial_damage")

PREV_COST_COLS <- c("cost_house_measures_bought", "cost_personal_measures_bought")

REACT_COST_COLS <- c("calculated_costs_personal_measures", "calculated_costs_house_measures")

ALL_COST_COLS <- c("living_costs", "cost_taxes", "spent_savings_for_buying_house",
                   "mortgage_payment", "cost_house_measures_bought", "cost_personal_measures_bought",
                   "cost_fluvial_damage", "cost_pluvial_damage")

DF_NAME <- "income_dist_df"

report_missing_cols <- function(df, cols) {
  if (any(cols %in% names(df)) == FALSE) {
    stop(paste("All expected collumns", paste(cols[cols %in% names(df) == FALSE], collapse = ", "), "missing in", DF_NAME))
  } else if (all(cols %in% names(df)) == FALSE) {
    warning(paste("Collumns", paste(cols[cols %in% names(df) == FALSE], collapse = ", "), "missing in", DF_NAME))
  }
}

calculate_costs_measures_difference <- function(income_dist_df) {
  
  report_missing_cols(income_dist_df, PREV_COST_COLS)
  report_missing_cols(income_dist_df, REACT_COST_COLS)
  
  income_dist_df[, "calculated_costs_measures_difference"] <-
    rowSums(income_dist_df[names(income_dist_df) %in% PREV_COST_COLS], na.rm = TRUE) -
    rowSums(income_dist_df[names(income_dist_df) %in% REACT_COST_COLS], na.rm = TRUE)
  
  return(income_dist_df)
}
  

# CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to df_income_dist

calculate_total_damage_costs <- function(income_dist_df) {
  
  report_missing_cols(income_dist_df, TYPE_COST_COLS)
  
  income_dist_df[,"total_damage_costs"] <- rowSums(income_dist_df[names(income_dist_df) %in% TYPE_COST_COLS], na.rm = TRUE)
  
  return(income_dist_df)
}
  

# Calculate the round costs to check the spendable income
# "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
# If either column has NA, the sum will also be NA unless the sum is done this way

calculate_total_costs <- function(income_dist_df) {
  
  report_missing_cols(income_dist_df, ALL_COST_COLS)
  
  income_dist_df[, "calculated_costs"] <- rowSums(income_dist_df[, ALL_COST_COLS], na.rm = TRUE) 
  
  return(income_dist_df)
}
  
# Calculate the spendable income

calculate_spendable_income <- function(income_dist_df) {
  
  income_dist_df <- income_dist_df %>% arrange(player_code, groupround_round_number)
  
  income_dist_df$calculated_spendable <- income_dist_df$spendable_income
  
  expected_players <- unique(income_dist_df[, "player_code"])
  
  found_players <- income_dist_df %>%
                      filter(groupround_round_number %in% 0) %>%
                      pull(player_code)
  
  
  if (any(expected_players %in% found_players) == FALSE) {
    
    stop(paste("Missing Round Number 0 value detected for players", paste(expected_players[expected_players %in% found_players == FALSE], collapse = ", ")))
    
  } else {
    
    # for (i in which(income_dist_df$groupround_round_number != "0")) {
    #   
    #   income_dist_df$calculated_spendable[i] <- rowSums(income_dist_df$calculated_spendable[i-1],
    #                                                     income_dist_df$round_income[i],
    #                                                     income_dist_df$profit_sold_house[i],
    #                                                     -income_dist_df$calculated_costs[i],
    #                                                     na.rm = TRUE)   }
    
    
    income_dist_df[income_dist_df[, "groupround_round_number"] %in% 0 == FALSE, "calculated_spendable"] <-
      rowSums(cbind(income_dist_df[which(income_dist_df[, "groupround_round_number"] %in% 0 == FALSE) - 1, "calculated_spendable"],
                    income_dist_df[income_dist_df[, "groupround_round_number"] %in% 0 == FALSE, "round_income"],
                    income_dist_df[income_dist_df[, "groupround_round_number"] %in% 0 == FALSE, "profit_sold_house"],
                    income_dist_df[income_dist_df[, "groupround_round_number"] %in% 0 == FALSE, "calculated_costs"]), na.rm = TRUE)
  }
  
  
  income_dist_df$calculated_difference_spendable <- income_dist_df$spendable_income - income_dist_df$calculated_spendable
  
  return(income_dist_df)
  
}
  # Calcule the reference dataset with all players average
  ## mapply safely substracts ingnoring NAs in either column 
  ## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
  

  
  # income_dist_df$income_minus_living <- mapply(
  #   function(income, cost) sum(income, -cost, na.rm = TRUE),
  #   income_dist_df$round_income,
  #   income_dist_df$living_costs
  # )
  # 
    

  # income_dist_df$profit_minus_spent_savings_house_moving <- mapply(
  #   function(profit, spent) sum(profit, -spent, na.rm = TRUE),
  #   income_dist_df$profit_sold_house,
  #   income_dist_df$spent_savings_for_buying_house
  # )
  # 

