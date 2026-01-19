
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