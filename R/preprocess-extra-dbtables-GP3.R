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

dbtable_list <- preprocess_data_list[[3]]
session_name <- names(preprocess_data_list)[3]
  
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
stopifnot("Missing dbtables needed for preprocessing" = all(c("housemeasure", "playerround", "personalmeasure", "measuretype") %in% names(dbtable_list)))


# Step 2: Data Preparations ---------------------------------------------------
#Add if the player implemented house or personal measures after flood experience (either river or rain damage)in the previous round
#Control if exclude or not pre-existing house measures or initial house measures already implemented when the player buys and moves into a house
exclude_initialhousemeasure <- FALSE  # TRUE = keep only initialhousemeasure = 0; FALSE = ignore this filter
initialhousemeasure_cond <- if (exclude_initialhousemeasure) paste0("is_initialhousemeasure", " = 0 AND ") else NULL
player_code_cond <- paste0("player_code", " IS NOT NULL")
round_number_cond <- paste0("groupround_round_number", " > 0")

playerround_temp_df <- playerround_df
playerround_temp_df[,"groupround_round_number"] <- playerround_temp_df[,"groupround_round_number"] + 1

housemeasure_filtered_df <- sqldf::sqldf(left_join_sqlquery(housemeasure_df, c("player_code", "groupround_round_number"),
                                                            playerround_temp_df, c("player_code", "groupround_round_number"),
                                                            kept_dbtable1_cols = c("gamesession_name", "id", "measuretype_id",
                                                                                   "group_name", "player_code", "house_code",
                                                                                   "groupround_round_number", "round_income",
                                                                                   "short_alias", "cost_absolute",
                                                                                   "satisfaction_delta_once",
                                                                                   "pluvial_protection_delta",
                                                                                   "fluvial_protection_delta",
                                                                                   "is_initialhousemeasure"),
                                                            kept_dbtable2_cols = c("cost_fluvial_damage",
                                                                                   "cost_pluvial_damage",
                                                                                   "total_damage_costs"),
                                                            is_where = TRUE,
                                                            where_cond = paste(
                                                              paste0("dbtable1.", c(initialhousemeasure_cond,
                                                                                    player_code_cond,
                                                                                    round_number_cond)
                                                              ),
                                                              collapse = " AND ")
)
)


housemeasure_filtered_df <- sqldf::sqldf(select_sqlquery(housemeasure_filtered_df,
                                                         names(housemeasure_filtered_df)[names(housemeasure_filtered_df) %in% "is_initialhousemeasure" == F])
                                         )

housemeasure_filtered_df <- sqldf::sqldf(rename_cols_sqlquery(housemeasure_filtered_df, "cost_absolute", "measure_cost"))

housemeasure_filtered_df <- sqldf::sqldf(unnull_dbcol_sqlquery(housemeasure_filtered_df, "cost_fluvial_damage", coal_val = 0))
housemeasure_filtered_df <- sqldf::sqldf(unnull_dbcol_sqlquery(housemeasure_filtered_df, "cost_pluvial_damage", coal_val = 0))
housemeasure_filtered_df <- sqldf::sqldf(unnull_dbcol_sqlquery(housemeasure_filtered_df, "total_damage_costs", coal_val = 0))


housemeasure_filtered <- sqldf(sprintf("
  SELECT
    hm.gamesession_name,
    hm.id,
    hm.measuretype_id,
    hm.group_name,
    hm.player_code,
    hm.house_code,
    hm.groupround_round_number,
    hm.round_income,
    hm.short_alias,
    hm.cost_absolute AS measure_cost,
    hm.satisfaction_delta_once,
    hm.pluvial_protection_delta,
    hm.fluvial_protection_delta,
    -- previous round damage values, COALESCE avoids NA/NULL values if there is no match 
    COALESCE(pr_prev.cost_fluvial_damage, 0) AS prev_cost_fluvial_damage,
    COALESCE(pr_prev.cost_pluvial_damage, 0) AS prev_cost_pluvial_damage,
    -- convenient total of previous damage
    COALESCE(pr_prev.total_damage_costs, 0) AS prev_total_damage 
  FROM housemeasure hm
  LEFT JOIN playerround pr_prev
         ON pr_prev.player_code = hm.player_code
        AND pr_prev.groupround_round_number = hm.groupround_round_number - 1
  WHERE %s
        hm.player_code IS NOT NULL
  AND hm.groupround_round_number > 0", initial_clause))

"bought_in_round"           
"housegroup_id"
"used_in_round"             
"owner_id"
"group_name"
"player_id"   
[13] "round_income"               "cost_house_measures_bought" "short_alias"               
[16] "cost_absolute"              "satisfaction_delta_once"    "pluvial_protection_delta"  
[19] "fluvial_protection_delta"   "is_initialhousemeasure"

personalmeasure_filtered <- sqldf("
  SELECT
    pm.gamesession_name,
    pm.id,
    pm.measuretype_id,
    pm.group_name,
    pm.player_code,
    pm.house_code,
    pm.groupround_round_number,
    pm.round_income,
    pm.short_alias,
    pm.calculated_costs AS measure_cost,
    pm.satisfaction_delta_once,
    pm.pluvial_protection_delta,
    pm.fluvial_protection_delta,
    -- previous round damage values, COALESCE avoids NA/NULL values if there is no match 
    COALESCE(pr_prev.cost_fluvial_damage, 0) AS prev_cost_fluvial_damage,
    COALESCE(pr_prev.cost_pluvial_damage, 0) AS prev_cost_pluvial_damage,
    -- convenient total of previous damage
    COALESCE(pr_prev.total_damage_costs, 0) AS prev_total_damage 
  FROM personalmeasure AS pm
  LEFT JOIN playerround pr_prev
         ON pr_prev.player_code = pm.player_code
        AND pr_prev.groupround_round_number = pm.groupround_round_number - 1
")

# Add a source column to each measures table and combine them
measures_combined <- sqldf("
  SELECT *, 'personalmeasure_filtered' AS source FROM personalmeasure_filtered
  UNION ALL
  SELECT *, 'housemeasure_filtered' AS source FROM housemeasure_filtered
")
# Step 3: Variables to plot calculation ---------------------------------------------------

#  Define the order to plot the measures
measures_text <- data.frame(
  short_alias = c("Rainbarrel for recycling",
                  "Waterproof walls, floors",
                  "Green garden",
                  "Self-activating wall",
                  "Water pump installation",
                  "Sandbags",
                  "Modest house renovations",
                  "Structural house changes",
                  "Personal improvements",
                  "Flood insurance"),
  cost_reference = c(0,0,0,0,0,0,
                     "% House cost",
                     "% House cost",
                     "% Round income",
                     "% House cost"),
  icons_path = c(file.path("icons","RainBarrel.png"),
                 file.path("icons","WaterproofingWalls.png"),          
                 file.path("icons","GreenGarden.png"),
                 file.path("icons","Self-ActivatingFloodWall.png"),
                 file.path("icons","Waterpump.png"),
                 file.path("icons","Sandbags.png"),
                 file.path("icons","ModestHouseRenovations.png"),
                 file.path("icons","StructuralHouseChanges.png"),
                 file.path("icons","PersonalImprovements.png"),
                 file.path("icons","FloodInsurance.png")),
  plot_order = c(0,0,0,0,0,0,2,1,3,4),
  stringsAsFactors = FALSE
)

measuretype <- sqldf("
  SELECT 
    m.short_alias,
    m.cost_absolute,
    m.cost_percentage_income,
    m.cost_percentage_house,
    mt.cost_reference,
    mt.plot_order,
    mt.icons_path
  FROM measuretype AS m
  LEFT JOIN measures_text AS mt
    ON m.short_alias = mt.short_alias
  ORDER BY 
    CASE
    WHEN m.cost_absolute <> 0 THEN 1 ELSE 2 
    END,
    m.cost_absolute DESC,
    mt.plot_order
")

#create a new column in R that concatenates the absolute cost (if non‑zero) and the percentage cost (if non‑zero) together with the cost reference. 
measuretype <- measuretype %>%
  mutate(
    cost_info = case_when(
      cost_absolute != 0 ~ paste0(cost_absolute/1000, "k"),
      cost_percentage_income != 0 ~ paste0(cost_percentage_income, "% income"),
      cost_percentage_house != 0 ~ paste0(cost_percentage_house, "% house cost"),
      TRUE ~ "No cost"
    )
  )

# Assuming measures_combined and measuretype data frames are in your R session
# Start from your measures_combined data frame
measures_combined_counts <- measures_combined %>%
  mutate(
    # Your three requested cases:
    case_both_prot_prev_total = (pluvial_protection_delta > 0 &
                                   fluvial_protection_delta > 0 &
                                   prev_total_damage > 0),
    
    case_pluvial_prev_pluvial = (pluvial_protection_delta > 0 &
                                   prev_cost_pluvial_damage > 0),
    
    case_fluvial_prev_fluvial = (fluvial_protection_delta > 0 &
                                   prev_cost_fluvial_damage > 0),
    
    # Remaining = NOT in any of the three conditions
    case_remaining = !(
      case_both_prot_prev_total |
        case_pluvial_prev_pluvial |
        case_fluvial_prev_fluvial
    )
  ) %>%
  # Group per round and measure type
  group_by(groupround_round_number, short_alias) %>%
  summarise(
    # Total implementations per round × measure type
    count_total_implementations             = n(),
    
    # Three bucket counts per round × measure type
    count_both_protection_prev_total_damage = sum(case_both_prot_prev_total,   na.rm = TRUE),
    count_pluvial_prev_pluvial_damage       = sum(case_pluvial_prev_pluvial,   na.rm = TRUE),
    count_fluvial_prev_fluvial_damage       = sum(case_fluvial_prev_fluvial,   na.rm = TRUE),
    
    # Remaining bucket
    count_remaining                         = sum(case_remaining,              na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # ---- Optional sanity check columns (per round × measure type) ----
mutate(
  check_sum = count_both_protection_prev_total_damage +
    count_pluvial_prev_pluvial_damage +
    count_fluvial_prev_fluvial_damage +
    count_remaining,
  all_good  = (check_sum == count_total_implementations))

# Keep your preferred plotting order (reverse of measuretype$short# Keep your preferred plotting order (reverse of measuretype$short_alias)
measures_combined_counts$short_alias <- factor(
  measures_combined_counts$short_alias,
  levels = rev(measuretype$short_alias)
)
# Ensure every row in measures_combined_counts has the correct icon according to its short_alias
measures_combined_counts <- measures_combined_counts %>%
  left_join(measuretype %>% select(short_alias, icons_path,cost_info), by = "short_alias")

# On Windows, this should open the image in your default viewer
shell.exec(normalizePath(measures_combined_counts$icons_path[1], winslash = "/", mustWork = TRUE))

# Set the factor level order based on groupround_round_number
measures_combined_counts$groupround_round_number <- factor(
  measures_combined_counts$groupround_round_number,
  levels = rev(sort(unique(measures_combined_counts$groupround_round_number)))# 1 → 2 → 3 …
)

# Improvements distribution specification ---------------------------------------------------
# Create a list with the tables used in the calculation
list_improv_dist <- list(
  measures_combined = measures_combined,
  measures_combined_counts = measures_combined_counts,
  measuretype = measuretype,
  personalmeasure = personalmeasure,
  housemeasure = housemeasure,
  questionscore = questionscore,
  questionitem = questionitem,
  initialhousemeasure = initialhousemeasure,
  house = house,
  housegroup = housegroup,
  group = group,
  groupround = groupround,
  player = player
)