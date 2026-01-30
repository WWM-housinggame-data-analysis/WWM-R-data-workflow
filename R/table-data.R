# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))

group_summary_table <- function(income_dist_reactive, selected_table) {
  
  group_col <- update_group_col(income_dist_reactive, selected_table)
  
  income_dist_reactive <- create_GP1_xlabels(income_dist_reactive, group_col)
  
  income_dist_ave <- retrieve_average_vector(income_dist_reactive,
                                             "xlabels",
                                             c("income_minus_living", "profit_minus_spent_savings_house_moving", "mortgage_payment", "cost_taxes", "paid_debt", "cost_house_measures_bought", "cost_personal_measures_bought", "cost_fluvial_damage", "cost_pluvial_damage", "spendable_income"),
                                             c("ave_income_minus_living", "ave_profit_minus_spent_savings_house_moving", "ave_mortgage", "ave_taxes", "ave_debt", "ave_measures", "ave_satisfaction", "ave_fluvial_damage", "ave_pluvial_damage", "ave_Spendable"))
  
  income_dist_n <- retrieve_n_table(income_dist_reactive, "xlabels")
  
  income_dist_n %>% inner_join(income_dist_ave, by = join_by(xlabels))
}