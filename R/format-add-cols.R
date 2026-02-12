# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here(file.path(FUNCTION_PATH, "constants.R")))

# Functions ----

## Report missing cols for a given dataframe
report_missing_cols <- function(df, in_cols, out_cols) {
  
  # Check DF_NAME exists
  stopifnot("Default variable DF_NAME not found in R/constants.R" = exists(deparse(substitute(DF_NAME))))
  
  ## Assumption that there are no missing columns
  missing_all <- FALSE
  
  ## Warning for if all input columns are missing
  if (any(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("(All) expected collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in ", DF_NAME, ". Column(s) ", 
                   paste(out_cols, collapse = ", "),
                   " cannot be added to this dataframe."
                   )
            )
    
    missing_all <- TRUE
            
  } else if (all(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("Expected Collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in ", DF_NAME, ". They will not be used in Calculating collumn(s) ",
                   paste(out_cols, collapse = ", "), "."
                   )
            )
  }
  
  return(missing_all)
}


#calculate the costs of the personal measures bough
retrieve_personalmeasure_calculated_costs <- function(pm_df, sum_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable COST_ABSOLUTE_COL not found in R/constants.R" = exists(deparse(substitute(COST_ABSOLUTE_COL))),
            "Default variable PERCENTAGE_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_INCOME_COL))),
            "Default variable PERCENTAGE_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_HOUSE_COL))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            
  )
  
  ## Calculate costs by summing absolute costs, amount of income and house-related costs 
  pm_df <- pm_df %>%
    mutate(
      !!sum_col :=
        rowSums(
          cbind(
            .data[[COST_ABSOLUTE_COL]],
            (.data[[PERCENTAGE_INCOME_COL]] / PERCENTAGE_FACTOR) * .data[[ROUND_INCOME_COL]],
            (.data[[PERCENTAGE_HOUSE_COL]] / PERCENTAGE_FACTOR) * .data[[LAST_PRICE_COL]]
          ),
          na.rm = TRUE
        )
    )
  
  return(pm_df)
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
  
  missing_all <- report_missing_cols(income_dist_df, CALCULATED_COLS, "calculated_costs_measures_difference")
  missing_all <- missing_all * report_missing_cols(income_dist_df, names(CALCULATED_COLS), "calculated_costs_measures_difference")
  
  if(missing_all == FALSE) {
    
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
  
  missing_all <- report_missing_cols(income_dist_df, TYPE_COST_COLS, "total_damage_costs")
  
  if(missing_all == FALSE) {
    income_dist_df[,"total_damage_costs"] <- rowSums(income_dist_df[names(income_dist_df) %in% TYPE_COST_COLS], na.rm = TRUE)
  }
  
  return(income_dist_df)
}


# Calculate the round costs to check the spendable income
# "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
# If either column has NA, the sum will also be NA unless the sum is done this way

calculate_total_costs <- function(income_dist_df) {
  
  missing_all <- report_missing_cols(income_dist_df, ALL_COST_COLS, "calculated_costs")
  
  if(missing_all == FALSE) {
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

