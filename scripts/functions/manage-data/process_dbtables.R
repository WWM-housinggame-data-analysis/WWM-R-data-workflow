retrieve_personalmeasure_cumulative <- function(personalmeasure) {
  
  # CHANGES vjcortesa-3: Corrected the calculation of the personal measure with the last_sold price instead of the mortgage_payment*10
  #calculate the costs of the personal measures bough
  personalmeasure$calculated_costs <- 
    personalmeasure$cost_absolute + 
    (personalmeasure$cost_percentage_income/100)*personalmeasure$round_income + 
    (personalmeasure$cost_percentage_house/100)*personalmeasure$last_sold_price
  
  
  head(personalmeasure)
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
        ifelse(initialhousemeasure, 0, cost_absolute)
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

# append_playerround_costmeas <- function(playerround, dataset_date) {
#   
#   if (dataset_date == "2409") {
#     playerround$calculated_costs_measures_difference <- playerround$cost_house_measures_bought - 
#       (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)
#   } else {
#     playerround$calculated_costs_measures_difference <- (playerround$cost_house_measures_bought +  playerround$cost_personal_measures_bought) - 
#       (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)
#   }
#   
#   # CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to playerround and df_income_dist
#   if (all(c("cost_fluvial_damage", "cost_pluvial_damage") %in% names(playerround))) {
#     playerround$total_damage_costs <- rowSums(
#       playerround[, c("cost_fluvial_damage", "cost_pluvial_damage")],
#       na.rm = TRUE
#     )
#   } else {
#     warning("cost_fluvial_damage and/or cost_pluvial_damage missing in playerround.")
#   }
#   
#   return(playerround)
# }