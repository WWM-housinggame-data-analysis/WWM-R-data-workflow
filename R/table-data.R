# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))

retrieve_summary_table <- function(df, selected_table) {
  
  group_col <- update_group_col(df, selected_table)
  
  df <- create_GP1_xlabels(df, group_col)
  
  pivoted_mean_df <- retrieve_mean_table(df, "xlabels",
                                            c("income_minus_living", "profit_minus_spent_savings_house_moving", "mortgage_payment", "cost_taxes", "paid_debt", "cost_house_measures_bought", "cost_personal_measures_bought", "cost_fluvial_damage", "cost_pluvial_damage", "spendable_income"),
                                            c("Average Income - Living Costs", "Average Net Profit House Moving", "Average Morgage Costs", "Average Taxes Costs", "Average Paid Debt", "Average House Measures Bought", "Average Personal Measures Bought", "Average Fluvial Damage Costs", "Average Pluvial Damage Costs", "Average Spendable Income"))
  
  summary_df <- pivoted_mean_df %>%
    select(-all_of("column_name")) %>%
    pivot_wider(names_from = "mean_label", values_from = "mean_value") %>%
    as.data.frame()
  
  return(summary_df)
  
}