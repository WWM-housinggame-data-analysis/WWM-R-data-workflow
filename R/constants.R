# Set defaults ----
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


# Central colour/label dictionaries (names must match cost_type in data)
EXPENSE_BARCOLS <- c("cost_personal_measures_bought", "cost_fluvial_damage",
                     "cost_pluvial_damage", "paid_debt", "cost_taxes",
                     "mortgage_payment", "profit_minus_spent_savings_house_moving")


fill_values_all <- c(
  "paid_debt" = "black", #"ave_income_minus_living" = "#E1BB70", "ave_satisfaction" = "#dfaba3",
  "cost_personal_measures_bought" = "#433E5E",
  "profit_minus_spent_savings_house_moving" =  "#a3a3a3",
  "mortgage_payment" = "#cccccc",
  "cost_taxes" = "#dddddd",
  "cost_fluvial_damage" = "#79A2C5",
  "cost_pluvial_damage" = "#79BCC5")


fill_labels_all <- c(
  "paid_debt" = "Debt", #"ave_satisfaction" = "Satisfaction", "ave_income_minus_living" = "Income - Living costs",
  "cost_personal_measures_bought" = "Measures",
  "profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
  "mortgage_payment" = "Mortgage",
  "cost_taxes" = "Taxes",
  "cost_fluvial_damage" = "River damage",
  "cost_pluvial_damage" = "Rain damage")

WELFARE_LABELS <- c("60k" = "Very Low",
                    "75k" = "Low",
                    "90k" = "Low-average",
                    "110k" = "High-average", 
                    "130k" = "High", 
                    "190k" = "Very High")

K_FACTOR <- 1000

BAR_WIDTH = 0.9
INTERM_ROUNDS <- as.character(1:3)