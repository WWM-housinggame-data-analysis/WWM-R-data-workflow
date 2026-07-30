# R/preprocess-dbtables.R
# ---------------------------------------------------------------
# Load constants and helper components
# ---------------------------------------------------------------

## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "sql-query-dbtables.R")))
source(here::here(file.path(FUNCTION_PATH, "create-dbtables.R")))
source(here::here(file.path(FUNCTION_PATH, "format-add-cols.R")))
source(here::here(file.path(FUNCTION_PATH, "list-upload-export-dbtables.R")))


# Functions ----

# ---------------------------------------------------------------
# Helper: unpack list of data frames into environment
# ---------------------------------------------------------------

unpack_dbtable_list <- function(dblist, suffix = "_df") {
  
  stopifnot("dblist is not list" = is.list(dblist),
            "dblist expected to have length higher than 0" = length(dblist) > 0,
            "dblist expected to have data frames only" = all(vapply(dblist, inherits, logical(1), "data.frame")))
  
  dblist <- stats::setNames(dblist, paste0(names(dblist), suffix))
  list2env(dblist, envir = parent.frame())
}

# ---------------------------------------------------------------
# Main preprocessing function
# ---------------------------------------------------------------

preprocess_selected_dbtables <- function(dbtable_list, session_name, excel = FALSE) {
  
  ## Unpack into global environment
  unpack_dbtable_list(dbtable_list, "_df")
  
  ## Check constants used in preprocessing exist
  stopifnot("Default variable SELECTED_DBTABLES not found in R/constants.R" = exists(deparse(substitute(SELECTED_DBTABLES))),
            "Default variable WELFARE_LABEL_COL not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABEL_COL))),
            "Default variable CALCULATED_COSTS_DIFF not found in R/constants.R" = exists(deparse(substitute(REPORTED_CALCULATED_COSTS_DIFFCOL))),
            "Default variable TOTAL_DAMAGE_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_DAMAGE_COL))),
            "Default variable INCOME_GRP_COL not found in R/constants.R" = exists(deparse(substitute(INCOME_GRP_COL))),
            "Default variable TOTAL_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_COSTS_COL))),
            "Default variable CALCULATED_SPENDABLE_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_SPENDABLE_COL))),
            "Default variable SPENDABLE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(SPENDABLE_DIFFCOL))),
            "Default variable INCOME_LIVING_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(INCOME_LIVING_DIFFCOL))),
            "Default variable HOUSEMOVING_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(HOUSEMOVING_DIFFCOL))))
  
  ## check SELECTED_DBTABLES needed for preprocessing are all found in dbtable_list
  stopifnot("Missing dbtables needed for preprocessing" = all(SELECTED_DBTABLES %in% names(dbtable_list)))
  
  # -----------------------------------------------------------
  # SQL transformations
  # -----------------------------------------------------------
  
  ## Rename the session name variable in the dataframe to avoid name overlap with the group name variable
  ## "SELECT id, name AS gamesession_name, password, location, create_time, date, start_time, end_time, gameversion_id, description FROM gamesession_df" 
  
  gamesession_df <- sqldf::sqldf(rename_cols_sqlquery(gamesession_df, "name", "gamesession_name"))
  
  
  ## Add to the group dataframe the gamesession_name by the group_df id = gamesession_df
  ## Leftjoin Keeps only the rows that have matching values in both data frames
  ##
  ## "SELECT dbtable1.*, dbtable2.gamesession_name
  ##  FROM [group_df] AS dbtable1
  ##  LEFT JOIN [gamesession_df] AS dbtable2
  ##  ON dbtable1.gamesession_id = dbtable2.id"
  
  group_df <- sqldf::sqldf(left_join_sqlquery(group_df, "gamesession_id",
                                       gamesession_df, "id",
                                       kept_dbtable2_cols = "gamesession_name"))

  
  ## Add to groupround_df the group_df variables selection
  ##
  ## "SELECT dbtable1.*, dbtable2.name, dbtable2.gamesession_id, dbtable2.gamesession_name, dbtable2.scenario_id
  ##  FROM [groupround_df] AS dbtable1
  ##  LEFT JOIN [group_df] AS dbtable2
  ##  ON dbtable1.group_id = dbtable2.id"
    
  groupround_df <- sqldf::sqldf(left_join_sqlquery(groupround_df, "group_id",
                                            group_df, "id",
                                            kept_dbtable2_cols = c("name", "gamesession_id",
                                                                   "gamesession_name", "scenario_id")))

    
  ## Rename the added columns in the dataframe to know from which table first come from
  ## "SELECT id, timestamp, pluvial_flood_intensity, fluvial_flood_intensity, group_state, round_number, group_id, name AS group_name, gamesession_id, gamesession_name, scenario_id AS group_scenario_id FROM groupround_df"
  
  groupround_df <- sqldf::sqldf(rename_cols_sqlquery(groupround_df, c("name", "scenario_id"), c("group_name", "group_scenario_id")))
  
  
  ## Added community name to houde_df
  ##
  ## "SELECT dbtable1.*, dbtable2.name
  ## FROM [house_df] AS dbtable1
  ## LEFT JOIN [community_df] AS dbtable2
  ## ON dbtable1.community_id = dbtable2.id"
  
  house_df <- sqldf::sqldf(left_join_sqlquery(house_df, "community_id",
                                       community_df, "id",
                                       kept_dbtable2_cols = "name"))
  
  
  ## Rename the added columns in the dataframe to know from which table first come from
  ## "SELECT id, price, code, available_round, address, rating, initial_pluvial_protection, initial_fluvial_protection, community_id, name AS community_name FROM house_df"
  
  house_df <- sqldf::sqldf(rename_cols_sqlquery(house_df, "name", "community_name"))

  
  ## Rename playerround_df column "id" with the table prefix to avoid id ambiguity , i.e. "playerround_id"
  ##
  ## "SELECT id AS playerround_id, create_time, round_income, living_costs, paid_debt, mortgage_payment, profit_sold_house,
  ##  spent_savings_for_buying_house, cost_taxes, cost_house_measures_bought, cost_personal_measures_bought, cost_fluvial_damage,
  ##  cost_pluvial_damage, spendable_income, satisfaction_move_penalty, satisfaction_house_rating_delta, satisfaction_house_measures,
  ##  satisfaction_personal_measures, satisfaction_fluvial_penalty, satisfaction_pluvial_penalty, satisfaction_debt_penalty, satisfaction_total,
  ##  preferred_house_rating, maximum_mortgage, mortgage_house_start, mortgage_left_start, house_price_sold, house_price_bought, mortgage_house_end,
  ##  mortgage_left_end, movingreason_id, moving_reason_other, pluvial_base_protection, fluvial_base_protection, pluvial_community_delta,
  ##  fluvial_community_delta, pluvial_house_delta, fluvial_house_delta, player_state, player_id, groupround_id, start_housegroup_id,
  ##  final_housegroup_id, active_transaction_id FROM playerround_df"
  
  playerround_df <- sqldf::sqldf(rename_cols_sqlquery(playerround_df, "id", "playerround_id"))
  
  
  ## Add to playerround_df the groupround_df selection to filter per round, group_df and session id and names by playerround_df = groupround_df id, with "playerround_id" as first column
  ##
  ## "SELECT dbtable1.*, dbtable2.round_number, dbtable2.group_id, dbtable2.group_name, dbtable2.gamesession_id, dbtable2.gamesession_name, dbtable2.group_scenario_id 
  ##  FROM [playerround_df] AS dbtable1
  ##  LEFT JOIN [groupround_df] AS dbtable2
  ##  ON dbtable1.groupround_id = dbtable2.id"
  
  playerround_df <- sqldf::sqldf(left_join_sqlquery(playerround_df, "groupround_id",
                                             groupround_df, "id",
                                             kept_dbtable1_cols = unique(c("playerround_id", names(playerround_df))),
                                             kept_dbtable2_cols = c("round_number", "group_id",
                                                                    "group_name", "gamesession_id",
                                                                    "gamesession_name", "group_scenario_id")))
  
  ## Rename the added columns in the dataframe to know from which table first come from
  ##
  ## "SELECT playerround_id, create_time, round_income, living_costs, paid_debt, mortgage_payment, profit_sold_house,
  ##  spent_savings_for_buying_house, cost_taxes, cost_house_measures_bought, cost_personal_measures_bought, cost_fluvial_damage,
  ##  cost_pluvial_damage, spendable_income, satisfaction_move_penalty, satisfaction_house_rating_delta, satisfaction_house_measures,
  ##  satisfaction_personal_measures, satisfaction_fluvial_penalty, satisfaction_pluvial_penalty, satisfaction_debt_penalty,
  ##  satisfaction_total, preferred_house_rating, maximum_mortgage, mortgage_house_start, mortgage_left_start, house_price_sold,
  ##  house_price_bought, mortgage_house_end, mortgage_left_end, movingreason_id, moving_reason_other, pluvial_base_protection,
  ##  fluvial_base_protection, pluvial_community_delta, fluvial_community_delta, pluvial_house_delta, fluvial_house_delta,
  ##  player_state, player_id, groupround_id, start_housegroup_id, final_housegroup_id, active_transaction_id,
  ##  round_number AS groupround_round_number, group_id, group_name, gamesession_id, gamesession_name, group_scenario_id FROM playerround_df"
  
  playerround_df <- sqldf::sqldf(rename_cols_sqlquery(playerround_df, "round_number", "groupround_round_number"))
  
  
  ## Add to the playerround_df the player code and welfaretype_id
  ##
  ## "SELECT dbtable1.*, dbtable2.code, dbtable2.welfaretype_id
  ##  FROM [playerround_df] AS dbtable1
  ##  LEFT JOIN [player_df] AS dbtable2
  ##  ON dbtable1.player_id = dbtable2.id"
  
  playerround_df <- sqldf::sqldf(left_join_sqlquery(playerround_df, "player_id",
                                             player_df, "id",
                                             kept_dbtable2_cols = c("code", "welfaretype_id")))
  
  
  ## Rename the added columns in the dataframe to know from which table first come from
  ##
  ## "SELECT playerround_id, create_time, round_income, living_costs, paid_debt, mortgage_payment, profit_sold_house,
  ##  spent_savings_for_buying_house, cost_taxes, cost_house_measures_bought, cost_personal_measures_bought,
  ##  cost_fluvial_damage, cost_pluvial_damage, spendable_income, satisfaction_move_penalty, satisfaction_house_rating_delta,
  ##  satisfaction_house_measures, satisfaction_personal_measures, satisfaction_fluvial_penalty, satisfaction_pluvial_penalty,
  ##  satisfaction_debt_penalty, satisfaction_total, preferred_house_rating, maximum_mortgage, mortgage_house_start, mortgage_left_start,
  ##  house_price_sold, house_price_bought, mortgage_house_end, mortgage_left_end, movingreason_id, moving_reason_other, pluvial_base_protection,
  ## fluvial_base_protection, pluvial_community_delta, fluvial_community_delta, pluvial_house_delta, fluvial_house_delta, player_state, player_id,
  ##  groupround_id, start_housegroup_id, final_housegroup_id, active_transaction_id, groupround_round_number, group_id, group_name, gamesession_id,
  ##  gamesession_name, group_scenario_id, code AS player_code, welfaretype_id FROM playerround_df"
  
  playerround_df <- sqldf::sqldf(rename_cols_sqlquery(playerround_df, "code", "player_code"))
  
  
  ## Add to the playerround_df the house code
  ##
  ## "SELECT dbtable1.*, dbtable2.code
  ##  FROM [playerround_df] AS dbtable1
  ##  LEFT JOIN [housegroup_df] AS dbtable2
  ##  ON dbtable1.final_housegroup_id = dbtable2.id"
  
  playerround_df <- sqldf::sqldf(left_join_sqlquery(playerround_df, "final_housegroup_id",
                                             housegroup_df, "id",
                                             kept_dbtable2_cols = "code"))
  
  
  ## Rename the added columns in the dataframe to know from which table first come from
  ##
  ## "SELECT playerround_id, create_time, round_income, living_costs, paid_debt, mortgage_payment, profit_sold_house,
  ##  spent_savings_for_buying_house, cost_taxes, cost_house_measures_bought, cost_personal_measures_bought, cost_fluvial_damage,
  ##  cost_pluvial_damage, spendable_income, satisfaction_move_penalty, satisfaction_house_rating_delta, satisfaction_house_measures,
  ##  satisfaction_personal_measures, satisfaction_fluvial_penalty, satisfaction_pluvial_penalty, satisfaction_debt_penalty,
  ##  satisfaction_total, preferred_house_rating, maximum_mortgage, mortgage_house_start, mortgage_left_start, house_price_sold,
  ##  house_price_bought, mortgage_house_end, mortgage_left_end, movingreason_id, moving_reason_other, pluvial_base_protection,
  ##  fluvial_base_protection, pluvial_community_delta, fluvial_community_delta, pluvial_house_delta, fluvial_house_delta,
  ##  player_state, player_id, groupround_id, start_housegroup_id, final_housegroup_id, active_transaction_id, groupround_round_number,
  ##  group_id, group_name, gamesession_id, gamesession_name, group_scenario_id, player_code, welfaretype_id, code AS house_code
  ##  FROM playerround_df"
  
  playerround_df <- sqldf::sqldf(rename_cols_sqlquery(playerround_df, "code", "house_code"))
  
  
  ## Add to the playerround_df the community_name
  ##
  ## "SELECT dbtable1.*, dbtable2.community_name
  ##  FROM [playerround_df] AS dbtable1
  ##  LEFT JOIN [house_df] AS dbtable2
  ##  ON dbtable1.house_code = dbtable2.code"
  
  playerround_df <- sqldf::sqldf(left_join_sqlquery(playerround_df, "house_code",
                                             house_df, "code",
                                             kept_dbtable2_cols = "community_name"))
  
  
  ## Sort playerround_df by player_code ascendingly
  ## "SELECT * FROM playerround_df ORDER BY player_code ASC"
  
  playerround_df <- sqldf::sqldf(sort_dbtable_sqlquery(playerround_df, "player_code"))
  
 
  ##  Add to the personalmeasure the playerround_df selection to filter per player, table, round and cost of measures
  ##
  ## "SELECT dbtable1.*, dbtable2.gamesession_name, dbtable2.group_name, dbtable2.player_id, dbtable2.player_code, dbtable2.groupround_round_number, dbtable2.round_income, dbtable2.cost_house_measures_bought, dbtable2.final_housegroup_id, dbtable2.mortgage_payment
  ##  FROM [personalmeasure_df] AS dbtable1
  ##  LEFT JOIN [playerround_df] AS dbtable2
  ##  ON dbtable1.playerround_id = dbtable2.playerround_id"
  
  personalmeasure_df <- sqldf::sqldf(left_join_sqlquery(personalmeasure_df, "playerround_id",
                                                 playerround_df, "playerround_id",
                                                 kept_dbtable2_cols = c("gamesession_name", "group_name",
                                                                        "player_id", "player_code", "groupround_round_number",
                                                                        "round_income", "cost_house_measures_bought",
                                                                        "final_housegroup_id", "mortgage_payment")))
  
  
  ## Make game_session_name first column in personalmeasure
  ##
  ## "SELECT gamesession_name, id, measuretype_id, playerround_id, group_name, player_id, player_code,
  ##  groupround_round_number, round_income, cost_house_measures_bought, final_housegroup_id, mortgage_payment
  ##  FROM personalmeasure_df"
  
  personalmeasure_df <- sqldf::sqldf(select_sqlquery(personalmeasure_df, unique(c("gamesession_name", names(personalmeasure_df)))))
  
  
  ## Add to the personalmeasure the housegroup selection to calculate the cost of measures
  ##
  ## "SELECT dbtable1.*, dbtable2.code, dbtable2.last_sold_price, dbtable2.owner_id
  ##  FROM [personalmeasure_df] AS dbtable1
  ##  LEFT JOIN [housegroup_df] AS dbtable2
  ##  ON dbtable1.final_housegroup_id = dbtable2.id"
  
  personalmeasure_df <- sqldf::sqldf(left_join_sqlquery(personalmeasure_df, "final_housegroup_id",
                                                 housegroup_df, "id",
                                                 kept_dbtable2_cols = c("code", "last_sold_price", "owner_id")))
  
  
  ## Rename the added columns in the dataframe to know from which table first come from
  ##
  ## "SELECT id, measuretype_id, playerround_id, gamesession_name, group_name, player_id, player_code,
  ##  groupround_round_number, round_income, cost_house_measures_bought, final_housegroup_id, mortgage_payment,
  ##  code AS house_code, last_sold_price, owner_id
  ##  FROM personalmeasure_df"
  
  personalmeasure_df <- sqldf::sqldf(rename_cols_sqlquery(personalmeasure_df, "code", "house_code"))
  
  
  ## Add to personalmeasure the measuretype selection to compare it with the costs of measures per round
  ##
  ## "SELECT dbtable1.*, dbtable2.short_alias, dbtable2.cost_absolute, dbtable2.cost_percentage_income, dbtable2.cost_percentage_house, dbtable2.satisfaction_delta_once, dbtable2.pluvial_protection_delta, dbtable2.fluvial_protection_delta
  ##  FROM [personalmeasure_df] AS dbtable1
  ##  LEFT JOIN [measuretype_df] AS dbtable2
  ##  ON dbtable1.measuretype_id = dbtable2.id"
  
  personalmeasure_df <- sqldf::sqldf(left_join_sqlquery(personalmeasure_df, "measuretype_id",
                                                 measuretype_df, "id",
                                                 kept_dbtable2_cols = c("short_alias", "cost_absolute", "cost_percentage_income",
                                                                        "cost_percentage_house", "satisfaction_delta_once",
                                                                        "pluvial_protection_delta", "fluvial_protection_delta")))
  
  
  ## Sort playerround_df by player_code ascendingly
  ## "SELECT * FROM personalmeasure_df ORDER BY player_code ASC"
  
  personalmeasure_df <- sqldf::sqldf(sort_dbtable_sqlquery(personalmeasure_df, "player_code"))
  
  
  ## calculate costs for personal measures
  personalmeasure_df <- append_personalmeasure_calculated_costs(personalmeasure_df, CALCULATED_COSTS_COL)
  
  
  # Create table with cumulative costs for personal measures
  personalmeasure_cumulative_df <- create_personalmeasure_cumulative_df(personalmeasure_df)
  
  
  ## Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
  ##
  ##  "SELECT dbtable1.*, dbtable2.code, dbtable2.rating, dbtable2.initial_pluvial_protection, dbtable2.initial_fluvial_protection, dbtable2.community_id
  ##  FROM [initialhousemeasure_df] AS dbtable1
  ##  LEFT JOIN [house_df] AS dbtable2
  ##  ON dbtable1.house_id = dbtable2.id"
  
  initialhousemeasure_df <- sqldf::sqldf(left_join_sqlquery(initialhousemeasure_df, "house_id",
                                                     house_df, "id",
                                                     kept_dbtable2_cols = c("code", "rating", "initial_pluvial_protection",
                                                                            "initial_fluvial_protection", "community_id")))
  
  
  ##  Rename the added columns in the dataframe to know from which table first come from
  ## "SELECT id, name, round_number, measuretype_id, house_id, code AS house_code, rating, initial_pluvial_protection,
  ##  initial_fluvial_protection, community_id FROM initialhousemeasure_df"
  
  initialhousemeasure_df <- sqldf::sqldf(rename_cols_sqlquery(initialhousemeasure_df, "code", "house_code"))
  
  
  ## Added to the initialhouse measure the measure type
  ##
  ## "SELECT dbtable1.*, dbtable2.short_alias
  ##  FROM [initialhousemeasure_df] AS dbtable1
  ##  LEFT JOIN [measuretype_df] AS dbtable2
  ##  ON dbtable1.measuretype_id = dbtable2.id"
  
  initialhousemeasure_df <- sqldf::sqldf(left_join_sqlquery(initialhousemeasure_df, "measuretype_id",
                                                     measuretype_df, "id",
                                                     kept_dbtable2_cols = "short_alias"))
  
  
  ## Sort initialhousemeasure_df by house_id ascendingly
  ## "SELECT * FROM initialhousemeasure_df ORDER BY house_id ASC"
  initialhousemeasure_df <- sqldf::sqldf(sort_dbtable_sqlquery(initialhousemeasure_df, "house_id"))
  
  
  ## Add to the housemeasure_df the housegroup selection to calculate the cost of measures
  ##
  ## "SELECT dbtable1.*, dbtable2.code, dbtable2.owner_id
  ##  FROM [housemeasure_df] AS dbtable1
  ##  LEFT JOIN [housegroup_df] AS dbtable2
  ##  ON dbtable1.housegroup_id = dbtable2.id"
  
  housemeasure_df <- sqldf::sqldf(left_join_sqlquery(housemeasure_df, "housegroup_id",
                                              housegroup_df, "id",
                                              kept_dbtable2_cols = c("code", "owner_id")))
  
  
  ##  Rename the added columns in the dataframe to know from which table first come from
  ## "SELECT id, bought_in_round, measuretype_id, housegroup_id, used_in_round, code AS house_code, owner_id FROM housemeasure_df"
  
  housemeasure_df <- sqldf::sqldf(rename_cols_sqlquery(housemeasure_df, "code", "house_code"))
  
  
  ## Add playerround data to the house measures table
  ##
  ## "SELECT dbtable1.*, dbtable2.gamesession_name, dbtable2.group_name, dbtable2.player_id, dbtable2.player_code, dbtable2.groupround_round_number, dbtable2.round_income, dbtable2.cost_house_measures_bought
  ##  FROM [housemeasure_df] AS dbtable1
  ##  LEFT JOIN [playerround_df] AS dbtable2
  ##  ON dbtable1.owner_id = dbtable2.player_id AND dbtable1.bought_in_round = dbtable2.groupround_round_number"
  
  housemeasure_df <- sqldf::sqldf(left_join_sqlquery(housemeasure_df, c("owner_id", "bought_in_round"),
                                              playerround_df, c("player_id", "groupround_round_number"),
                                              kept_dbtable2_cols = c("gamesession_name", "group_name", "player_id",
                                                                     "player_code", "groupround_round_number",
                                                                     "round_income", "cost_house_measures_bought")))
  
  
  ## Make "gamesession_name" first column
  ## "SELECT gamesession_name, id, bought_in_round, measuretype_id, housegroup_id, used_in_round, house_code,
  ##  owner_id, group_name, player_id, player_code, groupround_round_number, round_income, cost_house_measures_bought
  ##  FROM housemeasure_df"
  
  housemeasure_df <- sqldf::sqldf(select_sqlquery(housemeasure_df, unique(c("gamesession_name", names(housemeasure_df)))))
  
  
  ## Add the measuretype variables to calculate the costs of house measures per round 
  ##
  ## "SELECT dbtable1.*, dbtable2.short_alias, dbtable2.cost_absolute, dbtable2.satisfaction_delta_once, dbtable2.pluvial_protection_delta, dbtable2.fluvial_protection_delta
  ##  FROM [housemeasure_df] AS dbtable1
  ##  LEFT JOIN [measuretype_df] AS dbtable2
  ##  ON dbtable1.measuretype_id = dbtable2.id"
  
  housemeasure_df <- sqldf::sqldf(left_join_sqlquery(housemeasure_df, "measuretype_id",
                                              measuretype_df, "id",
                                              kept_dbtable2_cols = c("short_alias", 'cost_absolute', "satisfaction_delta_once",
                                                                     "pluvial_protection_delta", "fluvial_protection_delta")))
  
  
  ## The subquery checks if there is at least one measure from the initialhousemeasure table in the housemeasure table according to the house_code 
  ## Appends collumn compare_col informing whether condition above is TRUE or FALSE
  ##
  ## "SELECT dbtable1.*,
  ##  CASE WHEN EXISTS (SELECT TRUE FROM [initialhousemeasure_df] AS dbtable2
  ##                    WHERE dbtable1.measuretype_id = dbtable2.measuretype_id AND dbtable1.house_code = dbtable2.house_code)
  ##       THEN TRUE ELSE FALSE
  ##    END AS is_initialhousemeasure 
  ##  FROM [housemeasure_df] AS dbtable1"
  
  housemeasure_df <- sqldf::sqldf(compare_dbtables_sqlquery(housemeasure_df, c("measuretype_id", "house_code"),
                                                     initialhousemeasure_df, c("measuretype_id", "house_code"),
                                                     compare_col = "is_initialhousemeasure"))
  
  
  ## Sort housemeasure_df by player_code ascendingly
  ## "SELECT * FROM housemeasure_df ORDER BY player_code ASC"
  
  housemeasure_df <- sqldf::sqldf(sort_dbtable_sqlquery(housemeasure_df, "player_code"))
  
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
  
  housemeasure_cumulative_df <- create_housemeasure_cumulative_df(housemeasure_df)
    
  
  ## append human‑readable ordered categories matching numeric welfare IDs  
  playerround_df <- append_welfare_labels(playerround_df, WELFARE_LABEL_COL)
  
  
  ## Add to playerround_df the calculated costs of measures
  ##
  ## "SELECT dbtable1.*, dbtable2.calculated_costs_house_measures
  ##  FROM [playerround_df] AS dbtable1
  ##  LEFT JOIN [housemeasure_cumulative_df] AS dbtable2
  ##  ON dbtable1.player_code = dbtable2.player_code AND dbtable1.groupround_round_number = dbtable2.groupround_round_number"
  
  playerround_df <- sqldf::sqldf(left_join_sqlquery(playerround_df, c("player_code", "groupround_round_number"),
                                             housemeasure_cumulative_df, c("player_code", "groupround_round_number"),
                                             kept_dbtable2_cols = "calculated_costs_house_measures"))
  
  
  ## "SELECT dbtable1.*, dbtable2.calculated_costs_personal_measures
  ## FROM [playerround_df] AS dbtable1
  ## LEFT JOIN [personalmeasure_cumulative_df] AS dbtable2
  ## ON dbtable1.player_code = dbtable2.player_code AND dbtable1.groupround_round_number = dbtable2.groupround_round_number"
  
  playerround_df <- sqldf::sqldf(left_join_sqlquery(playerround_df, c("player_code", "groupround_round_number"),
                                             personalmeasure_cumulative_df, c("player_code", "groupround_round_number"),
                                             kept_dbtable2_cols = "calculated_costs_personal_measures"))
  
  
  ##  Sort playerround_df by player_code ascendingly
  ## "SELECT * FROM playerround_df ORDER BY player_code ASC"
  
  playerround_df <- sqldf::sqldf(sort_dbtable_sqlquery(playerround_df, "player_code"))
  
  
  ## Append difference between reported and calculated measures
  playerround_df <- append_reported_calculated_difference(playerround_df, REPORTED_CALCULATED_COSTS_DIFFCOL)
  
  
  ## Append pluvial + fluvial costs as total_damage
  playerround_df <- append_total_damage_costs(playerround_df, TOTAL_DAMAGE_COL)
  
  
  ## Rename columns in the dataframe
  ## "SELECT id AS questionitem_id, code AS answer_code, name AS answer_name, question_id FROM questionitem_df"
  
  questionitem_df <- sqldf::sqldf(rename_cols_sqlquery(questionitem_df, c("id", "code", "name"), c("questionitem_id", "answer_code", "answer_name")))
  
  
  ## Append column "answercode_plus_name" that combines integer column "answer_code" and character column "answer_name"
  ## "SELECT *, CAST(answer_code AS INTEGER) || ' - ' || answer_name AS answercode_plus_name FROM questionitem_df"
  
  questionitem_df <- sqldf::sqldf(combine_cols_sqlquery(questionitem_df, "answer_code", "integer", "answer_name", "string", "answercode_plus_name"))
  
  
  ## Add to question item the question name and description
  ## "SELECT dbtable1.questionitem_id, dbtable1.answer_code, dbtable1.answer_name, dbtable1.answercode_plus_name, dbtable1.question_id, dbtable2.name, dbtable2.description
  ##  FROM [questionitem_df] AS dbtable1
  ##  LEFT JOIN [question_df] AS dbtable2
  ##  ON dbtable1.question_id = dbtable2.id"
  
  questionitem_df <- sqldf::sqldf(left_join_sqlquery(questionitem_df, "question_id",
                                              question_df, "id",
                                              kept_dbtable1_cols = c("questionitem_id", "answer_code", "answer_name", "answercode_plus_name", "question_id"),
                                              kept_dbtable2_cols = c("name", "description")))
  
  
  ##  Rename the added columns in the dataframe to know from which table first come from
  ## "SELECT questionitem_id, answer_code, answer_name, answercode_plus_name, question_id, name AS question_name, description AS question_description FROM questionitem_df"
  
  questionitem_df <- sqldf::sqldf(rename_cols_sqlquery(questionitem_df, c("name", "description"), c("question_name", "question_description")))
  
  
  ## Rename columns in the dataframe
  ## "SELECT id AS answer_id, answer, late_answer, playerround_id, question_id FROM questionscore_df"  
  
  questionscore_df <- sqldf::sqldf(rename_cols_sqlquery(questionscore_df, "id", "answer_id"))

  
  ## Add to question score the relevant columns from question and question item tables
  ## "SELECT dbtable1.answer_id, dbtable1.answer, dbtable1.late_answer, dbtable1.question_id, dbtable1.playerround_id, dbtable2.answer_name, dbtable2.answercode_plus_name, dbtable2.question_name, dbtable2.question_description
  ##  FROM [questionscore_df] AS dbtable1
  ##  LEFT JOIN [questionitem_df] AS dbtable2
  ##  ON dbtable1.answer = dbtable2.answer_code AND dbtable1.question_id = dbtable2.question_id"
  
  questionscore_df <- sqldf::sqldf(left_join_sqlquery(questionscore_df, c("answer", "question_id"),
                                               questionitem_df, c("answer_code", "question_id"),
                                               kept_dbtable1_cols = c("answer_id", "answer", "late_answer", "question_id", "playerround_id"),
                                               kept_dbtable2_cols = c("answer_name", "answercode_plus_name", "question_name", "question_description")))
  
  
  ## Rename the added columns in the dataframe to know from which table first come from
  ## "SELECT answer_id, answer, late_answer, question_id, playerround_id, answer_name AS answer_option,
  ##  answercode_plus_name AS answer_plus_option, question_name, question_description
  ##  FROM questionscore_df"
  
  questionscore_df <- sqldf::sqldf(rename_cols_sqlquery(questionscore_df, c("answer_name", "answercode_plus_name"), c("answer_option", "answer_plus_option")))
  
  
  ## Rename columns in the dataframe
  ## "SELECT answer_id, answer, late_answer, answer_option, answer_plus_option, question_id, question_name, question_description, playerround_id
  ##  FROM questionscore_df"
  
  questionscore_df <- sqldf::sqldf(select_sqlquery(questionscore_df, c("answer_id", "answer", "late_answer", "answer_option", "answer_plus_option",
                                                                "question_id", "question_name", "question_description", "playerround_id")))
    
  
  ## Add to question score the relevant columns from player_round tables
  ##"SELECT dbtable1.*, dbtable2.groupround_round_number, dbtable2.player_code, dbtable2.group_name, dbtable2.gamesession_name
  ##  FROM [questionscore_df] AS dbtable1
  ##  LEFT JOIN [playerround_df] AS dbtable2
  ##  ON dbtable1.playerround_id = dbtable2.playerround_id"
  
  questionscore_df <- sqldf::sqldf(left_join_sqlquery(questionscore_df, "playerround_id",
                                               playerround_df, "playerround_id",
                                               kept_dbtable2_cols = c("groupround_round_number", "player_code", "group_name", "gamesession_name")))
    
  
  ## Remove "question_id" from questionitem_df
  ## "SELECT questionitem_id, answer_code, answer_name, answercode_plus_name, question_name, question_description FROM questionitem_df"
  
  questionitem_df <- sqldf::sqldf(select_sqlquery(questionitem_df, names(questionitem_df)[names(questionitem_df) %in% "question_id" == F]))
  
  
  ## Run the query to filter the playerround_df dataframe with the var_income_dist
  ## "SELECT gamesession_name, group_name, playerround_id, player_id, player_code, house_code, groupround_id, groupround_round_number,
  ##  round_income, living_costs, paid_debt, profit_sold_house, spent_savings_for_buying_house, cost_taxes, mortgage_payment,
  ##  cost_house_measures_bought, cost_personal_measures_bought, cost_fluvial_damage, cost_pluvial_damage, spendable_income,
  ##  calculated_costs_personal_measures, calculated_costs_house_measures, calculated_costs_measures_difference,
  ##  satisfaction_total, welfaretype_id, total_damage_costs, community_name, fluvial_house_delta, pluvial_house_delta
  ##  FROM playerround_df"
  
  # income_dist_df <- sqldf::sqldf(select_sqlquery(playerround_df, INCOME_DIST_ALLCOLS))
  # 
  # # -----------------------------------------------------------
  # # tidyverse operations
  # # -----------------------------------------------------------
  # 
  # ## Convert INCOME_DIST_CATEGCOLS to factor
  # income_dist_df <- income_dist_df |>
  #   dplyr::mutate_at(INCOME_DIST_CATEGCOLS, as.factor)
  # 
  # 
  # ## Append income_grp labels based on round_income to dataframe
  # income_dist_df <- append_income_grp(income_dist_df, INCOME_GRP_COL)
  # 
  # 
  # ## Convert columns not in INCOME_DIST_CATEGCOLS nor INCOME_GRP_COL to numeric
  # income_dist_df <- income_dist_df |>
  #   dplyr::mutate_at(
  #     names(income_dist_df)[!(names(income_dist_df) %in% c(INCOME_DIST_CATEGCOLS, INCOME_GRP_COL))],
  #     as.numeric
  #   )
  # 
  # 
  # ## Calculate the round costs to check the spendable income
  # income_dist_df <- append_total_costs(income_dist_df, TOTAL_COSTS_COL)
  # 
  # 
  # ## Calculate the spendable income
  # income_dist_df <- append_spendable_income_cols(income_dist_df, CALCULATED_SPENDABLE_COL, SPENDABLE_DIFFCOL)
  # 
  # 
  # ## Calculate income - living costs
  # income_dist_df <- append_income_living_diff(income_dist_df, INCOME_LIVING_DIFFCOL)
  # 
  # 
  # ## Calculate  "profit - spent savings house moving"
  # income_dist_df <- append_housemoving_diff(income_dist_df, HOUSEMOVING_DIFFCOL)
  
  # -----------------------------------------------------------
  # Collect results
  # -----------------------------------------------------------

  ## Update list to be returned with the tables used in the calculation 
  dbtable_list <- list(
    playerround = playerround_df,
    measuretype = measuretype_df,
    personalmeasure = personalmeasure_df,
    housemeasure = housemeasure_df,
    questionscore = questionscore_df,
    questionitem = questionitem_df,
    initialhousemeasure = initialhousemeasure_df,
    house = house_df,
    housegroup = housegroup_df,
    group = group_df,
    groupround = groupround_df,
    player = player_df,
    gamesession = gamesession_df
  )
  
  if (excel) {
    export_excel(dbtable_list, session_name, preprocessed = TRUE)
  }
  
  return(dbtable_list)
}

#Add if the player implemented house or personal measures after flood experience (either river or rain damage)in the previous round
#Control if exclude or not pre-existing house measures or initial house measures already implemented when the player buys and moves into a house
extra_preprocess_dbtables4GP3 <- function(dbtable_list, session_name, excel = FALSE) {
    
  ## Unpack into global environment
  unpack_dbtable_list(dbtable_list, "_df")
  
  housemeasure_filtered_df <- sqldf::sqldf(select_sqlquery(housemeasure_df, c("id", "measuretype_id", "group_name",
                                                                              "player_code", "house_code",
                                                                              "groupround_round_number", "round_income",
                                                                              "short_alias", "cost_absolute",
                                                                              "satisfaction_delta_once",
                                                                              "pluvial_protection_delta",
                                                                              "fluvial_protection_delta"),
                                                           is_where = TRUE,
                                                           where_cond = paste(c(IHM_CONDITION,
                                                                                PLAYER_CODE_CONDITION),
                                                                              collapse = " AND ")
  )
  )
  
  housemeasure_filtered_df <- sqldf::sqldf(rename_cols_sqlquery(housemeasure_filtered_df, "cost_absolute", "measure_cost"))
  
  
  personalmeasure_filtered_df <- sqldf::sqldf(select_sqlquery(personalmeasure_df, c("id", "measuretype_id", "group_name",
                                                                                    "player_code", "house_code",
                                                                                    "groupround_round_number", "round_income",
                                                                                    "short_alias", "calculated_costs",
                                                                                    "satisfaction_delta_once",
                                                                                    "pluvial_protection_delta",
                                                                                    "fluvial_protection_delta")))
  
  personalmeasure_filtered_df <- sqldf::sqldf(rename_cols_sqlquery(personalmeasure_filtered_df, "calculated_costs", "measure_cost"))
  
  
  # Add a source column to each measures table and combine them
  measures_combined_df <- sqldf::sqldf(union_all_sqlquery(personalmeasure_filtered_df, housemeasure_filtered_df,
                                                          source_col = "source", source_label_dbtable1 ="personalmeasure_filtered", source_label_dbtable2 = "housemeasure_filtered")
  )
  
  
  measuretype_df <- sqldf::sqldf(left_join_sqlquery(measuretype_df, match_dbtable1_cols = "short_alias",
                                                    MEASURETEXT_DF, match_dbtable2_cols = "short_alias",
                                                    kept_dbtable1_cols = c("short_alias", "cost_absolute", "cost_percentage_income", "cost_percentage_house"),
                                                    kept_dbtable2_cols = c(MEASURE_COSTREF_COL, MEASURE_COSTPLOT_COL, MEASURE_ICONS_COL))
  )
  
  measuretype_df <- sqldf::sqldf(sort_dbtable_sqlquery(measuretype_df, MEASURE_COSTPLOT_COL))
  measuretype_df <- sqldf::sqldf(sort_dbtable_sqlquery(measuretype_df, "cost_absolute", asc = FALSE))                                           
}
