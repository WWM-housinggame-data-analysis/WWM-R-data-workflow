
select_sqlquery <- function(dbtable, selected_cols) {
  
  if (missing(selected_cols) == FALSE && identical(selected_cols, names(dbtable)) == FALSE) {
    sqlquery <- paste0("SELECT ", paste(selected_cols, collapse = ", "), " FROM ", deparse(substitute(dbtable)))
  } else {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)))
  }
  
  return(sqlquery)
}

left_join_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, kept_dbtable1_vars, kept_dbtable2_vars) {
  
  if (missing(kept_dbtable1_vars) == TRUE && missing(kept_dbtable2_vars) == TRUE) {
    
    select_statement <- "SELECT dbtable1.*, dbtable2.*"
    
  } else if (missing(kept_dbtable1_vars) == TRUE && missing(kept_dbtable2_vars) == FALSE) {
    
    select_statement <- paste0("SELECT dbtable1.*, ", paste(paste0("dbtable2.", kept_dbtable2_vars), collapse = ", "))
    
  } else if (missing(kept_dbtable1_vars) == FALSE && missing(kept_dbtable2_vars) == TRUE) {
    
    select_statement <- paste0("SELECT ", paste(paste0("dbtable1.", kept_dbtable1_vars), collapse = ", "), ", dbtable2.*")
  } else {
    select_statement <- paste0("SELECT ", paste(paste0("dbtable1.", kept_dbtable1_vars), collapse = ", "), ", ",
                               paste(paste0("dbtable2.", kept_dbtable2_vars), collapse = ", "))
  }
  
  
  for_statement <- paste0("FROM [", deparse(substitute(dbtable1)), "] AS dbtable1")
  
  left_join_statement <- paste0("LEFT JOIN [", deparse(substitute(dbtable2)), "] AS dbtable2")
  
  on_statement <- paste0("ON ", paste(paste(paste0("dbtable1.", match_dbtable1_cols), paste0("dbtable2.",  match_dbtable2_cols), sep = " = "), collapse = " AND "))
  
  sqlquery <- paste(select_statement,
                     for_statement,
                     left_join_statement,
                     on_statement
  )
  
  return(sqlquery)
}

rename_cols_sqlquery <- function(dbtable, current_colnames, new_colnames, renamed_cols_first = FALSE) {
  
  stopifnot(is.character(current_colnames))
  
  stopifnot(is.character(new_colnames))
  
  if (length(current_colnames) != length(new_colnames)) {
    stop("current_colnames and new_colnames need to have the same length")
  }
  
  colnames <- names(dbtable)
  
  colnames <- colnames[colnames %in% current_colnames == FALSE]  # exclude the column you want to rename
  
  if (renamed_cols_first) {
    sqlquery <- paste0("SELECT ", paste(paste(current_colnames, new_colnames, sep = " AS "), collapse = ", "), ", ", paste(colnames, collapse = ", "),
                       " FROM " , deparse(substitute(dbtable)))
  } else {
    sqlquery <- paste0("SELECT ", paste(colnames, collapse = ", "), ", ", paste(paste(current_colnames, new_colnames, sep = " AS "), collapse = ", "),
                       " FROM " , deparse(substitute(dbtable)))
  }
  
  
  
  return(sqlquery)
}

sort_dbtable_sqlquery <- function(dbtable, sorting_col, asc = TRUE) {
  if (asc) {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)), " ORDER BY ", sorting_col, " ASC")
    
  } else {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)), " ORDER BY ", sorting_col, " DESC")
  }
  return(sqlquery)
}

compare_dbtables_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, compare_col) {
  
  case_statement <- paste0("CASE WHEN EXISTS (SELECT TRUE FROM [", deparse(substitute(dbtable2)), "] AS dbtable2 ",
                           "WHERE ", paste(paste(paste0("dbtable1.", match_dbtable1_cols), paste0("dbtable2.",  match_dbtable2_cols), sep = " = "), collapse = " AND "), ") ",
                           "THEN TRUE ELSE FALSE END AS ", compare_col)
  
  sqlquery <- paste0("SELECT dbtable1.*, ", case_statement, " FROM [", deparse(substitute(dbtable1)), "] AS dbtable1")
  
  return(sqlquery)
}

make_cast_statement <- function(colname, coltype) {
  if (coltype == "integer") {
    cast_statement <- paste0("CAST(", colname, " AS INTEGER)")
  } else {
    cast_statement <- colname
  }
  
  return(cast_statement)
}

combine_cols_sqlquery <- function(dbtable, colname1, coltype1, colname2, coltype2, comb_colname) {
  
  colname1_statement <- make_cast_statement(colname1, coltype1)
  
  colname2_statement <- make_cast_statement(colname2, coltype2)
  
  sqlquery <- paste0("SELECT *, ", colname1_statement, " || ' - ' || ", colname2_statement, " AS ", comb_colname, " FROM ", deparse(substitute(dbtable)))
  
  return(sqlquery)
}
  


retrieve_dbtables <- function(folder_path = "local path", folder_pattern = "csv_folder") {
  
  source(file.path(function_path, "process_dbtables.R"))
  
  # Read all tables in the folder with the custom function
  csv_data_list <- read_all_csvs(folder_path, folder_pattern)
  
  
  # Build a new list with only the elements you want
  GP2_data <- csv_data_list[GP2_tables]
  names(GP2_data)
  
  # Assign a table to a variable in the global environment
  gamesession_df <- GP2_data[["gamesession"]]
  group_df <- GP2_data[["group"]]
  groupround_df <- GP2_data[["groupround"]]
  playerround_df <- GP2_data[["playerround"]]
  player_df <- GP2_data[["player"]]
  measuretype_df <- GP2_data[["measuretype"]]
  personalmeasure_df <- GP2_data[["personalmeasure"]]
  housemeasure_df <- GP2_data[["housemeasure"]]
  housegroup_df <- GP2_data[["housegroup"]]
  community_df <- GP2_data[["community"]]
  house_df <- GP2_data[["house"]]
  initialhousemeasure_df <- GP2_data[["initialhousemeasure"]]
  question_df <- GP2_data[["question"]]
  questionitem_df <- GP2_data[["questionitem"]]
  questionscore_df <- GP2_data[["questionscore"]]
  
  # Rename the session name variable in the dataframe to avoid name overlap with the group name variable
  #gamesession_df <- sqldf("SELECT * FROM gamesession_df")
  #names(gamesession_df)[names(gamesession_df) == "name"] <- "gamesession_name"
  gamesession_df <- sqldf(rename_cols_sqlquery(gamesession_df, "name", "gamesession_name", renamed_cols_first = T))
  
  # Extract the dataset date to name the data and figure outputs accordingly 
  dataset_date <- str_extract(gamesession_df$gamesession_name, "\\d+")
  dataset_output<- file.path(data_output_path,paste0("GP2_",dataset_date))
  
  # Add to the group dataframe the gamesession_name by the group_df = gamesession_df id
  # Leftjoin Keeps only the rows that have matching values in both data frames
  # group_df <- sqldf("
  # SELECT g.*, gs.gamesession_name
  # FROM [group_df] AS g
  # LEFT JOIN [gamesession_df] AS gs
  # ON g.gamesession_id = gs.id
  # ")
  group_df <- sqldf(left_join_sqlquery(group_df, "gamesession_id", gamesession_df, "id", kept_dbtable2_vars = "gamesession_name"))
  
  # Add to groupround_df the group_df variables selection
  # groupround_df <- sqldf("
  # SELECT gr.*, g.name, g.gamesession_id, g.gamesession_name, g.scenario_id
  # FROM [groupround_df] AS gr
  # LEFT JOIN [group_df] AS g
  # ON gr.group_id = g.id
  # ")
  
  group_df <- sqldf(rename_cols_sqlquery(group_df, c("name", "scenario_id"), c("group_name", "group_scenario_id")))
  
  groupround_df <- sqldf(left_join_sqlquery(groupround_df, "group_id", group_df, "id",
                                         kept_dbtable2_vars = c("group_name", "gamesession_id", "gamesession_name", "group_scenario_id")))
  
  # Rename the added columns in the dataframe to know from which table first come from
  # names(groupround_df)[names(groupround_df) == "scenario_id"] <- "group_scenario_id"
  
  # # Rename name variable in the groupround dataframe for variable naming consistency
  # groupround_df <- sqldf("SELECT * FROM groupround_df")
  # 
  # # Rename the added columns in the dataframe to know from which table first come from
  # names(groupround_df)[names(groupround_df) == "name"] <- "group_name"
  
 
  
  # Add to playerround_df the groupround_df selection to filter per round, group_df and session id and names by playerround_df = groupround_df id
  # playerround_df <- sqldf("
  # SELECT pr.*, gr.round_number, gr.group_id, gr.group_name, gr.gamesession_id, gr.gamesession_name, gr.group_scenario_id
  # FROM [playerround_df] AS pr
  # LEFT JOIN [groupround_df] AS gr
  # ON pr.groupround_id = gr.id
  # ")
  
  groupround_df <- sqldf(rename_cols_sqlquery(groupround_df, "round_number", "groupround_round_number"))
  
  playerround_df <- sqldf(rename_cols_sqlquery(playerround_df, "id", "playerround_id", renamed_cols_first = TRUE))
  
  playerround_df <- sqldf(left_join_sqlquery(playerround_df, "groupround_id", groupround_df, "id",
                                             kept_dbtable2_vars = c("groupround_round_number", "group_id",
                                                                    "group_name", "gamesession_id",
                                                                    "gamesession_name", "group_scenario_id")))
  
  # # Rename the added columns in the dataframe to know from which table first come from
  # names(playerround_df)[names(playerround_df) == "round_number"] <- "groupround_round_number"
  # names(playerround_df)[names(playerround_df) == "scenario_id"] <- "group_scenario_id"
  
  # Rename id with the table prefix to avoid id ambiguity
  # names(playerround_df)[names(playerround_df) == "id"] <- "playerround_id"
  
  # Add to the playerround_df the p.code and welfaretype_id
  # playerround_df <- sqldf("
  # SELECT pr.*, p.code AS player_code, p.welfaretype_id AS welfaretype_id
  # FROM [playerround_df] AS pr
  # LEFT JOIN [player_df] AS p
  # ON pr.player_id = p.id
  # ORDER BY player_code ASC
  # ")
  
  player_df <- sqldf(rename_cols_sqlquery(player_df, "code", "player_code"))
  
  playerround_df <- sqldf(left_join_sqlquery(playerround_df, "player_id", player_df, "id",
                                             kept_dbtable2_vars = c("player_code", "welfaretype_id")))
  
  playerround_df <- sqldf(sort_dbtable_sqlquery(playerround_df, "player_code"))
  
  # Added house query to get community area into the playerround_df table
  # house_df <- sqldf("
  # SELECT h.*, c.name AS community_name
  # FROM [house_df] AS h
  # LEFT JOIN [community_df] as c
  # ON c.id = h.community_id
  # ")
  
  community_df <- sqldf(rename_cols_sqlquery(community_df, "name", "community_name"))
  
  house_df <- sqldf(left_join_sqlquery(house_df, "community_id", community_df, "id", kept_dbtable2_vars = "community_name"))
  
  # playerround_df <- sqldf("
  # SELECT pr.*, hg.code AS house_code, h.community_name
  # FROM [playerround_df] AS pr
  # LEFT JOIN [housegroup_df] AS hg
  # ON pr.final_housegroup_id = hg.id
  # LEFT JOIN [house_df] AS h
  # ON hg.code = h.code
  # ORDER BY pr.player_code ASC
  # ")
  
  housegroup_df <- sqldf(left_join_sqlquery(housegroup_df, "code", house_df, "code", kept_dbtable2_vars = "community_name"))
  
  housegroup_df <- sqldf(rename_cols_sqlquery(housegroup_df, "code", "house_code"))
  
  playerround_df <- sqldf(left_join_sqlquery(playerround_df, "final_housegroup_id", housegroup_df, "id", kept_dbtable2_vars = c("house_code", "community_name")))
  
  playerround_df <- sqldf(sort_dbtable_sqlquery(playerround_df, "player_code"))
  
  #END CHANGES
  
  # Move to tests/test_welfare levels ----
  
  # CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to playerround_df and df_income_dist
  # Map numeric welfaretype_id to welfare text levels
  #converts numeric welfare IDs into human‑readable ordered categories
  # Only if there are exactly six distinct IDs. Otherwise, it warns you that the mapping isn’t valid.
  welfare_labels <- c("Very Low",
                      "Low",
                      "Low-average",
                      "High-average",
                      "High",
                      "Very High")
  
  wt_codes <- sort(unique(playerround_df$welfaretype_id))
  
  if (length(wt_codes) == 6) {
    playerround_df$welfare_level <- factor(
      welfare_labels[match(playerround_df$welfaretype_id, wt_codes)],
      levels = welfare_labels,
      ordered = TRUE
    )
  } else {
    warning("Expected 6 distinct welfaretype_id values, but found ",   #make sure that it returns warning if not applicable
            length(wt_codes),
            ". welfare_level not created.")
  }
  
  # Stop test ----
  
  # Added the game_session_name to the measures tables
  # Add to the personalmeasure the playerround_df selection to filter per player, table, round and cost of measures
  # personalmeasure_df <- sqldf("
  # SELECT pr.gamesession_name, pm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought, pr.final_housegroup_id, pr.mortgage_payment
  # FROM [personalmeasure_df] AS pm
  # LEFT JOIN [playerround_df] AS pr
  # ON pm.playerround_id = pr.playerround_id
  # ORDER BY pr.player_code ASC
  # ")
  # 
  personalmeasure_df <- sqldf(left_join_sqlquery(personalmeasure_df, "playerround_id", playerround_df, "playerround_id",
                                           kept_dbtable2_vars = c("gamesession_name", "group_name", "player_id", "player_code", "groupround_round_number", "round_income",
                                                                  "cost_house_measures_bought", "final_housegroup_id", "mortgage_payment")))
  
  personalmeasure_df <- sqldf(select_sqlquery(personalmeasure_df, c("gamesession_name", names(personalmeasure_df)[names(personalmeasure_df) %in% "gamesession_name" == F])))


  # Add to the personalmeasure the housegroup selection to calculate the cost of measures
  # personalmeasure_df <- sqldf("
  # SELECT pm.*, hg.code AS house_code, hg.last_sold_price, hg.owner_id
  # FROM [personalmeasure_df] AS pm
  # LEFT JOIN [housegroup_df] AS hg
  # ON pm.final_housegroup_id = hg.id
  # ORDER BY pm.player_code ASC
  # ")
  
  personalmeasure_df <- sqldf(left_join_sqlquery(personalmeasure_df, "final_housegroup_id", housegroup_df, "id", kept_dbtable2_vars = c("house_code", "last_sold_price", "owner_id")))
  
  # Add to the measuretype selection to compare it with the costs of measures per round
  # personalmeasure_df <- sqldf("
  # SELECT pm.*, m.short_alias, m.cost_absolute, m.cost_percentage_income, m.cost_percentage_house, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  # FROM [personalmeasure_df] AS pm
  # LEFT JOIN [measuretype_df] AS m
  # ON pm.measuretype_id = m.id
  # ORDER BY pm.player_code ASC
  # ")
  # str(personalmeasure_df)
  
  personalmeasure_df <- sqldf(left_join_sqlquery(personalmeasure_df, "measuretype_id", measuretype_df, "id",
                                                 kept_dbtable2_vars = c("short_alias", "cost_absolute", "cost_percentage_income", "cost_percentage_house",
                                                                        "satisfaction_delta_once", "pluvial_protection_delta", "fluvial_protection_delta")))
  personalmeasure_df <- sqldf(sort_dbtable_sqlquery(personalmeasure_df, "player_code"))
  
  
  # CHANGES vjcortesa-3: Corrected the calculation of the personal measure with the last_sold price instead of the mortgage_payment*10
  personalmeasure_cumulative <- retrieve_personalmeasure_cumulative(personalmeasure_df)
  
  # Add to the housemeasure_df the housegroup selection to calculate the cost of measures
  # housemeasure_df <- sqldf("
  # SELECT hm.*, hg.code AS house_code, hg.owner_id
  # FROM [housemeasure_df] AS hm
  # LEFT JOIN [housegroup_df] AS hg
  # ON hm.housegroup_id = hg.id
  # ORDER BY hg.owner_id
  # ")
  
  housemeasure_df <- sqldf(left_join_sqlquery(housemeasure_df, "housegroup_id", housegroup_df, "id", kept_dbtable2_vars = c("house_code", "owner_id")))
  
  # Added the game_session_name to the measures tables
  # housemeasure_df <- sqldf("
  # SELECT pr.gamesession_name, hm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought
  # FROM [housemeasure_df] AS hm
  # LEFT JOIN [playerround_df] AS pr
  # ON hm.owner_id = pr.player_id AND hm.bought_in_round = pr.groupround_round_number
  # ORDER BY pr.player_code ASC
  # ")
  
  housemeasure_df <- sqldf(left_join_sqlquery(housemeasure_df, c("owner_id", "bought_in_round"), playerround_df, c("player_id", "groupround_round_number"),
                                              kept_dbtable2_vars = c("gamesession_name", "group_name", "player_id", "player_code", "groupround_round_number",
                                                                     "round_income", "cost_house_measures_bought")))
  
  housemeasure_df <- sqldf(select_sqlquery(housemeasure_df, c("gamesession_name", names(housemeasure_df)[names(housemeasure_df) %in% "gamesession_name" == F])))
  
  # Add the measuretype variables to calculate the costs of house measures per round 
  # housemeasure_df <- sqldf("
  # SELECT hm.*, m.short_alias, m.cost_absolute, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  # FROM [housemeasure_df] AS hm
  # LEFT JOIN [measuretype_df] AS m
  # ON hm.measuretype_id = m.id
  # ORDER BY hm.player_code ASC
  # ")
  # 
  housemeasure_df <- sqldf(left_join_sqlquery(housemeasure_df, "measuretype_id", measuretype_df, "id",
                                              kept_dbtable2_vars = c("short_alias", 'cost_absolute', "satisfaction_delta_once", "pluvial_protection_delta", "fluvial_protection_delta")))

  housemeasure_df <- sqldf(sort_dbtable_sqlquery(housemeasure_df, "player_code"))  
    
  # Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
  # initialhousemeasure_df <- sqldf("
  # SELECT ihm.*, h.code AS house_code, h.rating, h.initial_pluvial_protection, h.initial_fluvial_protection, h.community_id
  # FROM [initialhousemeasure_df] AS ihm
  # LEFT JOIN [house_df] AS h
  # ON ihm.house_id = h.id
  # ORDER BY ihm.house_id ASC
  # ")
  
  house_df <- sqldf(rename_cols_sqlquery(house_df, "code", "house_code"))
  
  initialhousemeasure_df <- sqldf(left_join_sqlquery(initialhousemeasure_df, "house_id", house_df, "id",
                                              kept_dbtable2_vars = c("house_code", "rating", "initial_pluvial_protection", "initial_fluvial_protection", "community_id")))
  
  
  # Added to the initialhouse measure the house_code to identify in the housemeasure calculation which measures came already implemented when player_df bought the house
  # initialhousemeasure_df <- sqldf("
  # SELECT ihm.*, m.short_alias 
  # FROM [initialhousemeasure_df] AS ihm
  # LEFT JOIN [measuretype_df] AS m
  # ON ihm.measuretype_id = m.id
  # ORDER BY ihm.house_id ASC
  # ")
  
  initialhousemeasure_df <- sqldf(left_join_sqlquery(initialhousemeasure_df, "measuretype_id", measuretype_df, "id", kept_dbtable2_vars = "short_alias"))
  
  initialhousemeasure_df <- sqldf(sort_dbtable_sqlquery(initialhousemeasure_df, "house_id")) 
  
  #The subquery checks if there is at least one measure from the initialhousemeasure table is in the housemeasure table according to the house_code 
  # housemeasure_df <- sqldf("
  # SELECT 
  #   hm.*,
  #   CASE 
  #     WHEN EXISTS (
  #       SELECT TRUE FROM [initialhousemeasure_df] AS ihm
  #       WHERE ihm.measuretype_id = hm.measuretype_id
  #         AND ihm.house_code = hm.house_code
  #     )
  #     THEN TRUE ELSE FALSE
  #   END AS initialhousemeasure
  # FROM [housemeasure_df] AS hm
  # ")
  
  housemeasure_df <- sqldf(compare_dbtables_sqlquery(housemeasure_df, c("measuretype_id", "house_code"), initialhousemeasure_df, c("measuretype_id", "house_code"), "is_initialhousemeasure"))
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
  housemeasure_cumulative <- retrieve_housemeasure_cumulative(housemeasure_df)
   
  
  #Add to playerround_df the calculated costs of measures
  # playerround_df <- sqldf("
  # SELECT pr.*, calculated_costs_house_measures
  # FROM [playerround_df] AS pr
  # LEFT JOIN [housemeasure_cumulative] AS hmc
  # ON pr.player_code = hmc.player_code AND pr.groupround_round_number = hmc.groupround_round_number
  # ORDER BY pr.player_code ASC
  # ")
  
  
  playerround_df <- sqldf(left_join_sqlquery(playerround_df, c("player_code", "groupround_round_number"),
                                             housemeasure_cumulative, c("player_code", "groupround_round_number"),
                                             kept_dbtable2_vars = "calculated_costs_house_measures"))
  
  # playerround_df <- sqldf("
  # SELECT pr.*, calculated_costs_personal_measures
  # FROM [playerround_df] AS pr
  # LEFT JOIN [personalmeasure_cumulative] AS pmc
  # ON pr.player_code = pmc.player_code AND pr.groupround_round_number = pmc.groupround_round_number
  # ORDER BY pr.player_code ASC
  # ")
  
  playerround_df <- sqldf(left_join_sqlquery(playerround_df, c("player_code", "groupround_round_number"),
                                             personalmeasure_cumulative, c("player_code", "groupround_round_number"),
                                             kept_dbtable2_vars = "calculated_costs_personal_measures"))
  
  # playerround_df <- append_playerround_costmeas(playerround_df, dataset_date)
  
  playerround_df <- sqldf(sort_dbtable_sqlquery(playerround_df, "player_code"))
  
  # Add to question score the question, question item and player_round tables relevant variables
  # questionscore_df <- sqldf("
  # SELECT 
  #   qs.id AS answer_id, qs.answer, qs.late_answer,qi.name AS answer_option, CAST(qs.answer AS INTEGER) || ' - ' || qi.name AS answer_plus_option, 
  #   qs.question_id, q.name AS question_name, q.description AS question_description,
  #   qs.playerround_id, pr.groupround_round_number, pr.player_code, pr.group_name, pr.gamesession_name
  # FROM questionscore_df AS qs
  # LEFT JOIN question_df AS q
  #   ON qs.question_id = q.id
  # LEFT JOIN questionitem_df AS qi
  #   ON qs.answer = qi.code
  #  AND qs.question_id = qi.question_id
  # LEFT JOIN  playerround_df AS pr
  #  ON qs.playerround_id = pr.playerround_id
  # ")
  
  question_df <- sqldf(rename_cols_sqlquery(question_df, c("name", "description"), c("question_name", "question_description")))
  
  questionitem_df <- sqldf(rename_cols_sqlquery(questionitem_df, c("id", "code", "name"), c("questionitem_id", "answer_code", "answer_name")))
  
  questionitem_df <- sqldf(combine_cols_sqlquery(questionitem_df, "answer_code", "integer", "answer_name", "string", "answercode_plus_name"))
  
  questionitem_df <- sqldf(left_join_sqlquery(questionitem_df, "question_id", question_df, "id",
                                              kept_dbtable1_vars = c("questionitem_id", "answer_code", "answer_name", "answercode_plus_name", "question_id"),
                                              kept_dbtable2_vars = c("question_name", "question_description")))
  
  questionscore_df <- sqldf(rename_cols_sqlquery(questionscore_df, "id", "answer_id"))
  
  questionscore_df <- sqldf(left_join_sqlquery(questionscore_df, c("answer", "question_id"),
                                               questionitem_df, c("answer_code", "question_id"),
                                               kept_dbtable1_vars = c("answer_id", "answer", "late_answer", "question_id", "playerround_id"),
                                               kept_dbtable2_vars = c("answer_name", "answercode_plus_name", "question_name", "question_description")))
  
  questionscore_df <- sqldf(rename_cols_sqlquery(questionscore_df, c("answer_name", "answercode_plus_name"), c("answer_option", "answer_plus_option")))
  
  
  questionscore_df <- sqldf(left_join_sqlquery(questionscore_df, "playerround_id", playerround_df, "playerround_id",
                                               kept_dbtable2_vars = c("groupround_round_number", "player_code", "group_name", "gamesession_name")))
  
  # questionitem_df <- sqldf("
  # SELECT 
  #   qi.id AS questionitem_id, qi.code AS answer_code, qi.name AS answer_name, 
  #   CAST(qi.code AS INTEGER) || ' - ' || qi.name AS answercode_plus_name,
  #   q.name AS question_name, q.description AS question_description
  # FROM questionitem_df AS qi
  # LEFT JOIN question_df AS q
  #   ON qi.question_id = q.id
  # ")
  
  group_df <- sqldf(rename_cols_sqlquery(group_df, c("group_name", "group_scenario_id"),  c("name", "scenario_id")))
  groupround_df <- sqldf(rename_cols_sqlquery(groupround_df, "groupround_round_number", "round_number"))
  housegroup_df <- sqldf(rename_cols_sqlquery(housegroup_df, "house_code", "code"))
  community_df <- sqldf(rename_cols_sqlquery(community_df, "community_name", "name"))
  player_df <- sqldf(rename_cols_sqlquery(player_df, "player_code", "code"))
  house_df <- sqldf(rename_cols_sqlquery(house_df, "house_code", "code"))
  question_df <- sqldf(rename_cols_sqlquery(question_df, c("question_name", "question_description"), c("name", "description")))
  questionitem_df <- sqldf(select_sqlquery(questionitem_df, names(questionitem_df)[names(questionitem_df) %in% "question_id" == F]))
  
  # Filter the playerround_df dataset for the income distribution
  
  # Select the variables for the income distribution plot
  var_income_dist <- c(
    "gamesession_name", "group_name",
    "playerround_id", "player_id", "player_code", "house_code", "groupround_id", "groupround_round_number",
    "round_income", "living_costs", "paid_debt",
    "profit_sold_house", "spent_savings_for_buying_house",
    "cost_taxes", "mortgage_payment",
    "cost_house_measures_bought", "cost_personal_measures_bought",
    "cost_fluvial_damage", "cost_pluvial_damage",
    "spendable_income"
  )
  
  # CHANGES vjcortesa-5: # Updated the var_income_dist list with the variables added by vcortesa and annehuitema2003, except for the welfare level to be added in the plot function
  ## Add the new calculated columns for the measures costs
  new_vars <- c("calculated_costs_personal_measures", 
                "calculated_costs_house_measures",
                #"calculated_costs_measures_difference",
                "satisfaction_total",
                "welfaretype_id",
                # "total_damage_costs",
                "community_name", #instead of housing_area to keep variable naming consistent
                "fluvial_house_delta",
                "pluvial_house_delta"
  )
  var_income_dist <- c(var_income_dist, new_vars)
  
  # Collapse the column vector into a comma-separated string
  col_income_dist <- paste(var_income_dist, collapse = ", ")
  
  # Run the query to filter the playerround_df dataframe with the var_income_dist 
  df_income_dist <- sqldf(paste0("
  SELECT ", col_income_dist, "
  FROM playerround_df
  "))
  
  # Step 3: Income distribution specification ---------------------------------------------------
  # CHANGES vjcortesa-7: Added to the list_income_dist file the tables added in the code
  # Create a list with the tables used in the calculation
  list_income_dist <- list(
    df_income_dist = df_income_dist,
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
  
  # Write to Excel with sheet names matching table names
  
  github <- "joaoxg"
  tryCatch({
    write_xlsx(list_income_dist, file.path(data_output_path, paste0(github, "_G2_Income_dist_", dataset_date, ".xlsx")))
    message("File written successfully.")
  }, error = function(e) {
    message("Error: ", e$message)
  })
  
  return(list_income_dist)
  
}
