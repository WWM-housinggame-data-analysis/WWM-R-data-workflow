# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))

group_summary_table <- function(income_dist_reactive, selected_table) {
  
  group_col <- update_group_col(income_dist_reactive, selected_table)
  
  income_dist_reactive <- create_GP1_xlabels(income_dist_reactive, group_col)
  
  income_dist_ave <- retrieve_mean_table(income_dist_reactive, "xlabels",
                                         c("income_minus_living", "profit_minus_spent_savings_house_moving", "mortgage_payment", "cost_taxes", "paid_debt", "cost_house_measures_bought", "cost_personal_measures_bought", "cost_fluvial_damage", "cost_pluvial_damage", "spendable_income"),
                                         c("Average Income - Living Costs", "Average Net Profit House Moving", "Average Morgage Costs", "Average Taxes Costs", "Average Paid Debt", "Average House Measures Bought", "Average Personal Measures Bought", "Average Fluvial Damage Costs", "Average Pluvial Damage Costs", "Average Spendable Income"))
  
  income_dist_ave <- income_dist_ave %>%
    select(-all_of("column_name")) %>%
    pivot_wider(names_from = "mean_label", values_from = "mean_value") %>%
    as.data.frame()
  
  income_dist_n <- retrieve_n_table(income_dist_reactive, "xlabels")
  
  income_dist_n %>% inner_join(income_dist_ave, by = join_by(xlabels))
  
}