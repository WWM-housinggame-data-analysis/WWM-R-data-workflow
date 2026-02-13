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

CALCULATED_COLS <- c("calculated_costs_personal_measures", "calculated_costs_house_measures")

names(CALCULATED_COLS) <- c("cost_house_measures_bought", "cost_personal_measures_bought")

ALL_COST_COLS <- c("living_costs", "cost_taxes", "spent_savings_for_buying_house",
                   "mortgage_payment", "cost_house_measures_bought", "cost_personal_measures_bought",
                   "cost_fluvial_damage", "cost_pluvial_damage")

DF_NAME <- "income_dist_df"

COST_ABSOLUTE_COL <- "cost_absolute"

PERCENTAGE_INCOME_COL <- "cost_percentage_income"

ROUND_INCOME_COL <- "round_income"

PERCENTAGE_HOUSE_COL <- "cost_percentage_house"

LAST_PRICE_COL <- "last_sold_price"

PERCENTAGE_FACTOR <- 100

CALCULATED_COSTS_PERSONAL_COL <- "calculated_costs_personal_measures"

COST_HOUSE_COL <- "cost_house_measures_bought"

PLAYER_CODE_COL <- "player_code"

ROUND_NUMBER_COL <- "groupround_round_number"

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


WELFARE_LABELS <- c("60k" = "Very Low",
                    "75k" = "Low",
                    "90k" = "Low-average",
                    "110k" = "High-average", 
                    "130k" = "High", 
                    "190k" = "Very High")

K_FACTOR <- 1000

BAR_WIDTH = 0.9
INTERM_ROUNDS <- as.character(1:3)