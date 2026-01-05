format_income_dist <- function(income_dist_dbtable) {
  
  categ_vars <- c("gamesession_name", "group_name", "playerround_id", "player_id", "groupround_id",
                  "groupround_round_number", "house_code", "player_code", "welfaretype_id",
                  "community_name", "fluvial_house_delta", "pluvial_house_delta" )

  
  income_dist_formatted <- income_dist_dbtable %>% mutate_at(categ_vars, as.factor)
  
  income_dist_formatted$round_income_grp <- factor(income_dist_formatted$round_income)
  
  categ_vars <- c(categ_vars, "round_income_grp")
  
  income_dist_formatted <- income_dist_formatted %>%
                           mutate_at(names(income_dist_formatted)[!(names(income_dist_formatted) %in% categ_vars)],
                                     as.numeric)
  
  dataset_date <- str_extract(unique(income_dist_formatted$gamesession_name), "\\d+")

  if (dataset_date == "2409") {
    income_dist_formatted$calculated_costs_measures_difference <- income_dist_formatted$cost_house_measures_bought -
      (income_dist_formatted$calculated_costs_personal_measures + income_dist_formatted$calculated_costs_house_measures)
  } else {
    income_dist_formatted$calculated_costs_measures_difference <- (income_dist_formatted$cost_house_measures_bought +  income_dist_formatted$cost_personal_measures_bought) -
      (income_dist_formatted$calculated_costs_personal_measures + income_dist_formatted$calculated_costs_house_measures)
  }

  # CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to df_income_dist
  if (all(c("cost_fluvial_damage", "cost_pluvial_damage") %in% names(income_dist_formatted))) {
    income_dist_formatted$total_damage_costs <- rowSums(
      income_dist_formatted[, c("cost_fluvial_damage", "cost_pluvial_damage")],
      na.rm = TRUE
    )
  } else {
    warning("cost_fluvial_damage and/or cost_pluvial_damage missing in income_dist_formatted.")
  }

  # Calculate the round costs to check the spendable income
  # "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
  # If either column has NA, the sum will also be NA unless the sum is done this way
  
  cost_vars <- c("living_costs", "cost_taxes", "spent_savings_for_buying_house",
                 "mortgage_payment", "cost_house_measures_bought", "cost_personal_measures_bought",
                 "cost_fluvial_damage", "cost_pluvial_damage")
  
  income_dist_formatted$calculated_costs <- rowSums(income_dist_formatted[, cost_vars], na.rm = TRUE) 
  
  # Calculate the spendable income
  income_dist_formatted$calculated_spendable <- income_dist_formatted$spendable_income
  
  if (1 %in% which(income_dist_formatted$groupround_round_number != "0") == TRUE) {
    stop("Missing Round NUmber 0 detected")
    
  } else {
    for (i in which(income_dist_formatted$groupround_round_number != "0")) {
      
      income_dist_formatted$calculated_spendable[i] <- sum(income_dist_formatted$calculated_spendable[i-1],
                                                    income_dist_formatted$round_income[i],
                                                    income_dist_formatted$profit_sold_house[i],
                                                    -income_dist_formatted$calculated_costs[i],
                                                    na.rm = TRUE)   }
  }
  
  
  income_dist_formatted$calculated_difference_spendable <- income_dist_formatted$spendable_income - income_dist_formatted$calculated_spendable
  
  # Calcule the reference dataset with all players average
  ## mapply safely substracts ingnoring NAs in either column 
  ## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
  
  income_dist_formatted$income_minus_living<- mapply(
    function(income, cost) sum(income, -cost, na.rm = TRUE),
    income_dist_formatted$round_income,
    income_dist_formatted$living_costs
  )
  
  income_dist_formatted$profit_minus_spent_savings_house_moving <- mapply(
    function(profit, spent) sum(profit, -spent, na.rm = TRUE),
    income_dist_formatted$profit_sold_house,
    income_dist_formatted$spent_savings_for_buying_house
  )
  
  return(income_dist_formatted)
}

