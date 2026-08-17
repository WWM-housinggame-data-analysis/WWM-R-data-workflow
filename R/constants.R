#R/constants.R

SELECT_ALL <- "All"

# CONFIG <- yaml::read_yaml(here::here("./config.yml"))

# SELECTED_GAMESESSION <- CONFIG$defaults$selected_gamesession
# SELECTED_USERNAME    <- CONFIG$defaults$selected_username
# 
# if (length(grep("fac", SELECTED_USERNAME)) == 1) {
#   
#   SELECTED_USERTABLE <- paste0("Table", gsub("fac", "", SELECTED_USERNAME))
#   
#   stopifnot("Provided username cannot have access to all session. Please specify the session in `selected_gamesession`" = identical(SELECTED_GAMESESSION, "All") == FALSE)
#     
# } else if (length(grep("coord", SELECTED_USERNAME)) == 1) {
#   
#   SELECTED_USERTABLE <- "All"
#   
#   stopifnot("Provided username cannot have access to all session. Please specify the session in `selected_gamesession`" = identical(SELECTED_GAMESESSION, "All") == FALSE)
#   
# } else {
#   
#   SELECTED_USERTABLE <- "All"
#   SELECTED_GAMESESSION <- "All"
#   
# }

GAMESESSION_FLAG <- "housinggame"
AVAILABLE_QUESTIONS <- c("GP2", "GP3")

SELECTED_TABLEGROUP <- SELECT_ALL
SELECTED_GAMESESSION <- SELECT_ALL
SELECTED_QUESTION <- SELECT_ALL




# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")
RAWDATA_PATH <- file.path("data", "raw-dbtables")
PREPRDATA_PATH <- file.path("data", "preprocessed-dbtables")
RESULTS_PATH <- file.path("data", "results")

SELECTED_DBTABLES <- c("gamesession", "group", "groupround",
                       "playerround", "player","measuretype",
                       "personalmeasure","housemeasure", "housegroup",
                       "community","house","initialhousemeasure",
                       "question","questionitem","questionscore")

INCOME_DIST_CATEGCOLS <- c("gamesession_name", "group_name", "playerround_id", "player_id", "player_code", "house_code",
                           "groupround_id", "groupround_round_number", "welfaretype_id", "community_name")


MEASURE_COMBINED_CATEGCOLS <- c("id", "measuretype_id",  "group_name", "player_code", "house_code", "groupround_round_number",
                                "short_alias", "source", "icons_path", "cost_info")

## Default variables for handling data import/export

IMPORTED_TABLE_TYPE <- ".csv"

WORKFLOW_STAGES <- c("raw", "preprocessed")
names(WORKFLOW_STAGES) <- c(RAWDATA_PATH, PREPRDATA_PATH)

PREPROCESSED_DBTABLES <- c("playerround", "measuretype", "personalmeasure",
                           "housemeasure", "questionscore", "questionitem",
                           "initialhousemeasure", "house", "housegroup",
                           "group", "groupround", "player", "gamesession")


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
COST_BAR_SEGMENTS <- c("total_damage_costs",
                     "cost_personal_measures_bought",
                     "cost_house_measures_bought",
                     "profit_minus_spent_savings_house_moving",
                     "mortgage_payment",
                     "paid_debt")

names(COST_BAR_SEGMENTS) <- c("Damage (river + rain)",
                            "Personal measures",
                            "House measures",
                            "Spent savings (buying house)",
                            "Mortgage payment",
                            "Paid debt")

TABLE_GROUPCOL <- "group_name"
GP2_XLABEL_COL <- "xlabels"
GP3_YLABEL_COL <- "ylabels"

GP3_BARGEGLABEL_COL <- "barseglabel"

COST_SCATTER_LINE <- "satisfaction_total"
names(COST_SCATTER_LINE) <-  "Average total satisfaction"

COST_TABLE_ENTRIES <- c("income_minus_living",
                        "profit_minus_spent_savings_house_moving",
                        "mortgage_payment",
                        "cost_taxes",
                        "paid_debt",
                        "cost_house_measures_bought",
                        "cost_personal_measures_bought",
                        "cost_fluvial_damage",
                        "cost_pluvial_damage", 
                        "spendable_income")

names(COST_TABLE_ENTRIES) <- c("Average Income - Living Costs",
                               "Average Net Profit House Moving",
                               "Average Mortgage Costs",
                               "Average Taxes Costs",
                               "Average Paid Debt",
                               "Average House Measures Bought",
                               "Average Personal Measures Bought",
                               "Average Fluvial Damage Costs",
                               "Average Pluvial Damage Costs",
                               "Average Spendable Income")

# COST_BAR_COLORS
fill_values_all <- c("#79A2C5", "#dfaba3", "#433E5E", "#a3a3a3", "#cccccc", "black")
names(fill_values_all) <- names(COST_BAR_SEGMENTS)

WELFARE_BARSEG_COLORPALT <- grDevices::colorRampPalette(c("wheat3",  "wheat", "yellow"))

K_FACTOR <- 1000
names(K_FACTOR) <- "k" 

BAR_WIDTH = 0.9

EXPECTED_ROUNDS <- as.character(0:4)
EXPECTED_INTERM_ROUNDS <- EXPECTED_ROUNDS[2 : (length(EXPECTED_ROUNDS) - 1)]

HEADER_TITLE <- "WhereWeMove Dashboard"
HEADER_BACKCOLOR <- "#2D89C8"
HEADER_THEME <- "dark"

APP_NAVBAR_OPTIONS <- bslib::navbar_options(
  bg = HEADER_BACKCOLOR,
  theme = HEADER_THEME
)

HEADER_TAB1 <- "Game Play"

SIDEBAR1_TITLE <- "Choices and effects"
SIDEBAR1_BACKCOLOR <- "white"

EXPAND_MULTIPLE_ACCORDIONS <- TRUE

SESSION_ACCORDION_TITLE <- "1: Select Game Session"
SESSION_ACCORDION_LABEL <- "Session"
SESSION_ACCORDION_VALUE <- "gamesession"

QUESTION_ACCORDION_TITLE <- "2: Research Question"
QUESTION_ACCORDION_LABEL <- "Question"
QUESTION_ACCORDION_VALUE <- "question"

GROUP_ACCORDION_TITLE <- "2: Select Table"
GROUP_ACCORDION_LABEL <- "Table"
GROUP_ACCORDION_VALUE <- "table"

ADDRESS_ACCORDION_TITLE <- "3: Where players live"

GP2_SEGMENT_ACCORDION_TITLE <- "4: Player spending"
GP2_SEGMENT_ACCORDION_LABEL <- "Cost Types"
GP2_SEGMENT_ACCORDION_VALUE <- "cost_types"

MEASURES_ACCORDION_TITLE <- "5: Selected measures"
FLOOD_ACCORDION_TITLE <- "6: Flood in gameplay"
SATISFACTION_ACCORDION_TITLE <- "7: Damage & satisfaction"

UI_ROUNDS_RENDERING <- "ui_round_rendering"
UI_GP2_SEGMENT_RENDERING <- "UI_GP2_segment_rendering"

# Apply a top margin of 1rem (typically 16px) to the div.
DIV_16PX_MARGIN <- "mt-3"

ACTION_BUTTON_ID <- "reset_all_filters"
ACTION_BUTTON_LABEL <- "Reset all filters"

#Style this button with Bootstrap’s “warning” theme,
ACTION_BUTTON_WARNING <- "btn-warning"

# Optional: a global reset ill button for the whole sidebar
RESET_ALL_BUTTON <- shiny::div(
  class = DIV_16PX_MARGIN,
  shiny::actionButton(ACTION_BUTTON_ID, ACTION_BUTTON_LABEL, class = ACTION_BUTTON_WARNING)
)

HEADER_TAB2 <- "Game Settings"

MAIN_PANEL_WIDTH <- 10

ROUND_ACCORDION_IDPREF <- "r"
names(ROUND_ACCORDION_IDPREF) <- "Round"
ROUND_ACCORDION_LABELALL <- "All Rounds"

DEFAULT_OPEN_ACCORDIONS <- ROUND_ACCORDION_LABELALL

REFS_HEADER_TITLE <- "Links"
REFS_HEADER_ALIGN <- "right"

ABOUT_LINK <- "https://seriousgaming.tudelft.nl/games/"
names(ABOUT_LINK) <- "About WhereWeMove"

INFO_LINK <- "https://pure.tudelft.nl/ws/portalfiles/portal/180909041/WhereWeMove-Brochure_Final.pdf"
names(INFO_LINK) <- "WhereWeMove info"

FACILIT_LINK <- "https://housing-game.tbm.tudelft.nl/housinggame-facilitator/jsp/facilitator/login.jsp"
names(FACILIT_LINK) <- "Facilitator website"

PLAYER_LINK <- "https://housing-game.tbm.tudelft.nl/housinggame-player/jsp/player/login.jsp"
names(PLAYER_LINK) <- "Player website"

REFS_HEADER_TAB <- bslib::nav_menu(
  title = REFS_HEADER_TITLE,
  align = REFS_HEADER_ALIGN,
  bslib::nav_item(shiny::tags$a(names(ABOUT_LINK), href = ABOUT_LINK)),
  bslib::nav_item(shiny::tags$a(names(INFO_LINK), href = INFO_LINK)),
  bslib::nav_item(shiny::tags$a(names(FACILIT_LINK), href = FACILIT_LINK)),
  bslib::nav_item(shiny::tags$a(names(PLAYER_LINK), href = PLAYER_LINK))
)

LINEBREAK <- "<br>"

EXCLUDE_IHM <- FALSE  # TRUE = keep only initialhousemeasure = 0; FALSE = ignore this filter
IHM_CONDITION <- if (EXCLUDE_IHM) paste0(IS_IHM_COL, " = 0 AND ") else NULL
PLAYER_CODE_CONDITION <- paste0(PLAYER_CODE_COL, " IS NOT NULL")

COST_INFO_COL <- "cost_info"

MEASURE_ALIAS_COL <- "short_alias"

MEASURE_ALIASES <- c("Underground rainbarrel", "Waterproof walls, floors", "Green garden",
                   "Self-activating wall", "Water pump installation", "Sandbags",
                   "Modest house renovations", "Structural house changes",
                   "Personal improvements", "Flood insurance")

MEASURE_BAR_GROUPS <- MEASURE_ALIASES
names(MEASURE_BAR_GROUPS) <- MEASURE_ALIASES

ICONS_PATH <- "data/dependencies/icons"

MEASURE_ICONS_COL <- "icons_path"

MEASURE_ICONS_FILENAMES <- c("RainBarrel", "WaterproofingWalls", "GreenGarden",
                             "Self-ActivatingFloodWall", "Waterpump","Sandbags",
                             "ModestHouseRenovations", "StructuralHouseChanges",
                             "PersonalImprovements", "FloodInsurance")

MEASURE_ICONS_FILETYPE <- ".png"

MEASURE_ICONS_FILEPATHS <- file.path(ICONS_PATH,
                                     paste0(MEASURE_ICONS_FILENAMES, MEASURE_ICONS_FILETYPE))

names(MEASURE_ICONS_FILEPATHS) <- MEASURE_ALIASES

if (any(file.exists(MEASURE_ICONS_FILEPATHS) == FALSE)) {
  stop(paste0("The following files do not match existing files within the project directory: \n",
              paste(MEASURE_ICONS_FILEPATHS[file.exists(MEASURE_ICONS_FILEPATHS) == FALSE], collapse = "\n")
              )
       )
}

ACCEPTED_IMAGE_FORMATS <- c(".png", ".jpg", ".jpeg", ".svg")

if (any(grepl(paste0("\\.(", paste(gsub("\\.", "", ACCEPTED_IMAGE_FORMATS), collapse = "|"), ")$"), MEASURE_ICONS_FILEPATHS) == FALSE)) {
  stop(paste0("MEASURE_ICONS_FILEPATHS does contain filepaths whose format do not match the accepted image formats: ", paste(ACCEPTED_IMAGE_FORMATS, collapse = ", ")))
}

stopifnot("MEASURE_ICONS_FILEPATHS does contain filepaths whose format do not match the accepted image formats:" =
            length(grepl(paste0("\\.(", paste(ACCEPTED_IMAGE_FORMATS, collapse = "|"), ")$"), MEASURE_ICONS_FILEPATHS)) == length(MEASURE_ICONS_FILEPATHS))

MEASURE_COSTREF_COL <- "cost_reference"
 
MEASURE_COSTREF_VALUES <- c(0,0,0,0,0,0,
                            "% House cost",
                            "% House cost",
                            "% Round income",
                            "% House cost")

names(MEASURE_COSTREF_VALUES) <- MEASURE_ALIASES

MEASURE_COSTPLOT_COL <- "plot_order"

MEASURE_COSTPLOT_ORDER <- c(0,0,0,0,0,0,2,1,3,4)

names(MEASURE_COSTPLOT_ORDER) <- MEASURE_ALIASES
  
MEASURETEXT_DF_COLS <- c(MEASURE_ALIAS_COL, MEASURE_COSTREF_COL, MEASURE_ICONS_COL, MEASURE_COSTPLOT_COL)

MEASURETEXT_DF <- data.frame(matrix(nrow = length(MEASURE_ALIASES), ncol = length(MEASURETEXT_DF_COLS)), stringsAsFactors = FALSE)

names(MEASURETEXT_DF) <- MEASURETEXT_DF_COLS

MEASURETEXT_DF[,MEASURE_ALIAS_COL] <- MEASURE_ALIASES
MEASURETEXT_DF[,MEASURE_COSTREF_COL] <- MEASURE_COSTREF_VALUES
MEASURETEXT_DF[,MEASURE_ICONS_COL] <- MEASURE_ICONS_FILEPATHS
MEASURETEXT_DF[,MEASURE_COSTPLOT_COL] <- MEASURE_COSTPLOT_ORDER

ROUND_SPLIT_COLORS <- c(
  "#e0e0e0",
  "#b3b3b3",
  "#808080",
  "#4d4d4d",
  "#1a1a1a"
)

names(ROUND_SPLIT_COLORS) <- EXPECTED_ROUNDS
