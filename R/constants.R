

CONFIG <- read_yaml(here("./config.yml"))

SELECTED_GAMESESSION <- CONFIG$defaults$selected_gamesession
SELECTED_USERNAME    <- CONFIG$defaults$selected_username

if (length(grep("fac", SELECTED_USERNAME)) == 1) {
  
  SELECTED_USERTABLE <- paste0("Table", gsub("fac", "", SELECTED_USERNAME))
  
  stopifnot("Provided username cannot have access to all session. Please specify the session in `selected_gamesession`" = identical(SELECTED_GAMESESSION, "All") == FALSE)
    
} else if (length(grep("coord", SELECTED_USERNAME)) == 1) {
  
  SELECTED_USERTABLE <- "All"
  
  stopifnot("Provided username cannot have access to all session. Please specify the session in `selected_gamesession`" = identical(SELECTED_GAMESESSION, "All") == FALSE)
  
} else {
  
  SELECTED_USERTABLE <- "All"
  SELECTED_GAMESESSION <- "All"
  
}

SELECTED_USERNAME <- SELECTED_USERTABLE


# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")
RAWDATA_PATH <- file.path("data", "raw-dbtables")
PREPRDATA_PATH <- file.path("data", "preprocessed-dbtables")

SELECTED_DBTABLES <- c("gamesession", "group", "groupround",
                       "playerround", "player","measuretype",
                       "personalmeasure","housemeasure", "housegroup",
                       "community","house","initialhousemeasure",
                       "question","questionitem","questionscore")

INCOME_DIST_CATEGCOLS <- c("gamesession_name", "group_name", "playerround_id", "player_id", "player_code", "house_code",
                           "groupround_id", "groupround_round_number", "welfaretype_id", "community_name")


## Default variables for handling data import/export

IMPORTED_TABLE_TYPE <- ".csv"

WORKFLOW_STAGES <- c("raw", "preprocessed")
names(WORKFLOW_STAGES) <- c(RAWDATA_PATH, PREPRDATA_PATH)

PREPROCESSED_DBTABLES <- c("income_dist_df")


## Default variables for preprocessing data

TYPE_COST_COLS <- c("cost_fluvial_damage", "cost_pluvial_damage")

CALCULATED_COSTS_COL <- "calculated_costs"

CALCULATED_COSTS_PERSONAL_COL <- "calculated_costs_personal_measures"

CALCULATED_COSTS_HOUSE_COL <- "calculated_costs_house_measures"

CALCULATED_COLS <- c(CALCULATED_COSTS_PERSONAL_COL, CALCULATED_COSTS_HOUSE_COL)

COST_HOUSE_COL <- "cost_house_measures_bought"

names(CALCULATED_COLS) <- c(COST_HOUSE_COL, "cost_personal_measures_bought")


## "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
ALL_COST_COLS <- c("living_costs", "cost_taxes", "spent_savings_for_buying_house",
                   "mortgage_payment", "cost_house_measures_bought", "cost_personal_measures_bought",
                   "cost_fluvial_damage", "cost_pluvial_damage")

COST_ABSOLUTE_COL <- "cost_absolute"

PERCENTAGE_INCOME_COL <- "cost_percentage_income"

ROUND_INCOME_COL <- "round_income"

PERCENTAGE_HOUSE_COL <- "cost_percentage_house"

LAST_PRICE_COL <- "last_sold_price"

PERCENTAGE_FACTOR <- 100

PLAYER_CODE_COL <- "player_code"

ROUND_NUMBER_COL <- "groupround_round_number"

PERSONAL_HOUSE_DIFFCOL <- "difference"

CUMULATIVE_COSTS_PERSONAL_COL <- "cum_costs"

CUMULATIVE_PERSONAL_HOUSE_DIFFCOL <- "cum_difference"

IS_IHM_COL <- "is_initialhousemeasure"

TOTAL_BOUGHT_COL <- "total_bought_measures"

HOUSE_TOTAL_DIFFCOL <- "difference"

CUMULATIVE_COSTS_HOUSE_COL <- "cum_costs"

CUMULATIVE_HOUSE_TOTAL_DIFFCOL <- "cum_differences"

REPORTED_CALCULATED_COSTS_DIFFCOL <- "calculated_costs_measures_difference"

TOTAL_DAMAGE_COL <- "total_damage_costs"

TOTAL_COSTS_COL <- "calculated_costs"

SPENDABLE_INCOME_COL <- "spendable_income"

CALCULATED_SPENDABLE_COL <- "calculated_spendable"

PROFIT_HOUSE_COL <- "profit_sold_house"

SPENT_SAVINGS_COL <- "spent_savings_for_buying_house"

HOUSEMOVING_DIFFCOL <- "profit_minus_spent_savings_house_moving"

SPENDABLE_DIFFCOL <- "calculated_difference_spendable"

WELFARE_LABELS <- c("60k" = "Very Low",
                    "75k" = "Low",
                    "90k" = "Low-average",
                    "110k" = "High-average", 
                    "130k" = "High", 
                    "190k" = "Very High")

WELFARE_ID_COL <- "welfaretype_id"

WELFARE_LABEL_COL <- "welfare_level"

INCOME_GRP_COL <- "income_grp"

LIVING_COSTS_COL <- "living_costs"

INCOME_LIVING_DIFFCOL <- "income_minus_living"


# Select the variables for the income distribution plot
INCOME_DIST_ALLCOLS <- c("gamesession_name", "group_name", "playerround_id", "player_id", "player_code", "house_code",
                         "groupround_id", "groupround_round_number", "round_income", "living_costs", "paid_debt",
                         "profit_sold_house", "spent_savings_for_buying_house", "cost_taxes", "mortgage_payment",
                         "cost_house_measures_bought", "cost_personal_measures_bought", "cost_fluvial_damage", "cost_pluvial_damage",
                         "spendable_income", "calculated_costs_personal_measures", "calculated_costs_house_measures", "calculated_costs_measures_difference",
                         "satisfaction_total", "welfaretype_id", "total_damage_costs",
                         "community_name", "fluvial_house_delta", "pluvial_house_delta")

# Central colour/label dictionaries (names must match cost_type in data)
EXPENSE_BARCOLS <- c("total_damage_costs",
                     "cost_personal_measures_bought",
                     "cost_house_measures_bought",
                     "profit_minus_spent_savings_house_moving",
                     "mortgage_payment",
                     "paid_debt")

names(EXPENSE_BARCOLS) <- c("Damage (river + rain)",
                            "Personal measures",
                            "House measures",
                            "Spent savings (buying house)",
                            "Mortgage payment",
                            "Paid debt")


fill_values_all <- c("#79A2C5", "#dfaba3", "#433E5E", "#a3a3a3", "#cccccc", "black")

names(fill_values_all) <- names(EXPENSE_BARCOLS)

K_FACTOR <- 1000
names(K_FACTOR) <- "k" 

BAR_WIDTH = 0.9
INTERM_ROUNDS <- as.character(1:3)