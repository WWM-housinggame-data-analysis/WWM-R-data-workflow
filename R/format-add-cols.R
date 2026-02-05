# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))


TYPE_COST_COLS <- c("cost_fluvial_damage", "cost_pluvial_damage")

CALCULATED_COLS <- c("calculated_costs_personal_measures", "calculated_costs_house_measures")

names(CALCULATED_COLS) <- c("cost_house_measures_bought", "cost_personal_measures_bought")

ALL_COST_COLS <- c("living_costs", "cost_taxes", "spent_savings_for_buying_house",
                   "mortgage_payment", "cost_house_measures_bought", "cost_personal_measures_bought",
                   "cost_fluvial_damage", "cost_pluvial_damage")

DF_NAME <- "income_dist_df"

report_missing_cols <- function(df, in_cols, out_cols) {
  
  any_col <- TRUE
  
  if (any(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("(All) expected collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in ", DF_NAME, ". Column(s) ", 
                   paste(out_cols, collapse = ", "),
                   " cannot be added to this dataframe."
                   )
            )
    
    any_col <- FALSE
            
  } else if (all(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("Expected Collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in ", DF_NAME, ". They will not be used in Calculating collumn(s) ",
                   paste(out_cols, collapse = ", "), "."
                   )
            )
  }
  
  return(any_col)
}

# CHANGES vjcortesa-3: Corrected the calculation of the personal measure with the last_sold price instead of the mortgage_payment*10
#calculate the costs of the personal measures bough
retrieve_personalmeasure_calculated_costs <- function(personalmeasure) {
  
  personalmeasure$calculated_costs <- rowSums(cbind(personalmeasure$cost_absolute,
                                                 (personalmeasure$cost_percentage_income / 100) * personalmeasure$round_income,
                                                 (personalmeasure$cost_percentage_house / 100) * personalmeasure$last_sold_price),
                                           na.rm = TRUE)
  
  return(personalmeasure)
}

retrieve_personalmeasure_cumulative <- function(personalmeasure) {


  #calculate the cumulative of the personal measures to compare it against the cost of house measures bought
  personalmeasure_cumulative <- personalmeasure %>%
    arrange(player_code, groupround_round_number) %>%   # ensure proper order
    group_by(player_code, groupround_round_number) %>%  # group by player and round
    #add up costs within each round for each player (since you may have multiple rows per round)
    summarise(calculated_costs_personal_measures = sum(calculated_costs),# sum across rows in the round
              total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
              .groups = "drop"
    ) %>% 
    #ensure cumulative totals are calculated separately for each player
    mutate(
      difference = calculated_costs_personal_measures - total_bought_measures
    ) %>%
    group_by(player_code) %>%
    arrange(groupround_round_number) %>%
    # compute the running total across rounds
    mutate(
      cum_costs       = cumsum(calculated_costs_personal_measures),
      cum_difference  = cumsum(difference)
    )
  
  return(personalmeasure_cumulative)
}

retrieve_housemeasure_cumulative <- function(housemeasure) {
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
  housemeasure_cumulative <- housemeasure %>%
    arrange(player_code, groupround_round_number) %>%   # ensure proper order
    group_by(player_code, groupround_round_number) %>%  # group by player and round
    #add up costs within each round for each player (since you may have multiple rows per round)
    summarise(
      # sum only cost_absolute where initialhousemeasure == FALSE
      calculated_costs_house_measures = sum(
        ifelse(is_initialhousemeasure, 0, cost_absolute)
      ),
      total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
      .groups = "drop"
    ) %>%
    #ensure cumulative totals are calculated separately for each player
    mutate(
      difference = calculated_costs_house_measures - total_bought_measures
    ) %>%
    group_by(player_code) %>%
    arrange(groupround_round_number) %>%
    # compute the running total across rounds
    mutate(
      cum_costs       = cumsum(calculated_costs_house_measures),
      cum_difference  = cumsum(difference)
    )
  
  return(housemeasure_cumulative)
}


calculate_costs_measures_difference <- function(income_dist_df) {
  
  any_col <- report_missing_cols(income_dist_df, CALCULATED_COLS, "calculated_costs_measures_difference")
  any_col <- any_col * report_missing_cols(income_dist_df, names(CALCULATED_COLS), "calculated_costs_measures_difference")
  
  if(any_col) {
    
    col_cross <- as.logical(names(CALCULATED_COLS) %in% names(income_dist_df) * names(CALCULATED_COLS) %in% names(income_dist_df))
    
    calc_cols <- CALCULATED_COLS[col_cross]
    db_cols <- names(CALCULATED_COLS)[col_cross]
    
    income_dist_df[, "calculated_costs_measures_difference"] <-
      rowSums(income_dist_df[names(income_dist_df) %in% db_cols], na.rm = TRUE) -
      rowSums(income_dist_df[names(income_dist_df) %in% calc_cols], na.rm = TRUE)
  }
  
  return(income_dist_df)
}
  

# CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to df_income_dist

calculate_total_damage_costs <- function(income_dist_df) {
  
  any_col <- report_missing_cols(income_dist_df, TYPE_COST_COLS, "total_damage_costs")
  
  if(any_col) {
    income_dist_df[,"total_damage_costs"] <- rowSums(income_dist_df[names(income_dist_df) %in% TYPE_COST_COLS], na.rm = TRUE)
  }
  
  return(income_dist_df)
}


# Calculate the round costs to check the spendable income
# "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
# If either column has NA, the sum will also be NA unless the sum is done this way

calculate_total_costs <- function(income_dist_df) {
  
  any_col <- report_missing_cols(income_dist_df, ALL_COST_COLS, "calculated_costs")
  
  if(any_col) {
    income_dist_df[, "calculated_costs"] <- rowSums(income_dist_df[names(income_dist_df) %in% ALL_COST_COLS], na.rm = TRUE) 
  }
  
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
                    -income_dist_df[income_dist_df[, "groupround_round_number"] %in% 0 == FALSE, "calculated_costs"]), na.rm = TRUE)
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

