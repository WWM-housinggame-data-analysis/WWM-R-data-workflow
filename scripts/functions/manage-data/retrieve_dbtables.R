retrieve_dbtables <- function(folder_path = "local path", folder_pattern = "csv_folder") {
  
  source(file.path(function_path, "process_dbtables.R"))
  
  # Read all tables in the folder with the custom function
  csv_data_list <- read_all_csvs(folder_path, folder_pattern)$datalist
  
  GP2_tables <- c("gamesession", "group", "groupround",
                  "playerround", "player","measuretype",
                  "personalmeasure","housemeasure", "housegroup",
                  "community","house","initialhousemeasure",
                  "question","questionitem","questionscore")
  
  data_output_path <- file.path("data", "combined-dbtables")
  
  # Build a new list with only the elements you want
  GP2_data <- csv_data_list[GP2_tables]
  names(GP2_data)
  
  # Assign a table to a variable in the global environment
  gamesession <- GP2_data[["gamesession"]]
  group <- GP2_data[["group"]]
  groupround <- GP2_data[["groupround"]]
  playerround <- GP2_data[["playerround"]]
  player <- GP2_data[["player"]]
  measuretype <- GP2_data[["measuretype"]]
  personalmeasure <- GP2_data[["personalmeasure"]]
  housemeasure <- GP2_data[["housemeasure"]]
  housegroup <- GP2_data[["housegroup"]]
  community <- GP2_data[["community"]]
  house <- GP2_data[["house"]]
  initialhousemeasure <- GP2_data[["initialhousemeasure"]]
  question <- GP2_data[["question"]]
  questionitem <- GP2_data[["questionitem"]]
  questionscore <- GP2_data[["questionscore"]]
  
  # Rename the session name variable in the dataframe to avoid name overlap with the group name variable
  gamesession <- sqldf("SELECT * FROM gamesession")
  names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"
  
  # Extract the dataset date to name the data and figure outputs accordingly 
  dataset_date <- str_extract(gamesession$gamesession_name, "\\d+")
  dataset_output<- file.path(data_output_path,paste0("GP2_",dataset_date))
  
  # Add to the group dataframe the gamesession_name by the group = gamesession id
  # Leftjoin Keeps only the rows that have matching values in both data frames
  group <- sqldf("
  SELECT g.*, gs.gamesession_name
  FROM [group] AS g
  LEFT JOIN [gamesession] AS gs
  ON g.gamesession_id = gs.id
  ")
  
  # Add to groupround the group variables selection
  groupround <- sqldf("
  SELECT gr.*, g.name, g.gamesession_id, g.gamesession_name, g.scenario_id
  FROM [groupround] AS gr
  LEFT JOIN [group] AS g
  ON gr.group_id = g.id
  ")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(groupround)[names(groupround) == "scenario_id"] <- "group_scenario_id"
  
  # Rename name variable in the groupround dataframe for variable naming consistency
  groupround <- sqldf("SELECT * FROM groupround")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(groupround)[names(groupround) == "name"] <- "group_name"
  
  # Add to playerround the groupround selection to filter per round, group and session id and names by playerround = groupround id
  playerround <- sqldf("
  SELECT pr.*, gr.round_number, gr.group_id, gr.group_name, gr.gamesession_id, gr.gamesession_name, gr.group_scenario_id
  FROM [playerround] AS pr
  LEFT JOIN [groupround] AS gr
  ON pr.groupround_id = gr.id
  ")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(playerround)[names(playerround) == "round_number"] <- "groupround_round_number"
  names(playerround)[names(playerround) == "scenario_id"] <- "group_scenario_id"
  
  # Rename id with the table prefix to avoid id ambiguity
  names(playerround)[names(playerround) == "id"] <- "playerround_id"
  
  # Add to the playerround the p.code and welfaretype_id
  playerround <- sqldf("
  SELECT pr.*, p.code AS player_code, p.welfaretype_id AS welfaretype_id
  FROM [playerround] AS pr
  LEFT JOIN [player] AS p
  ON pr.player_id = p.id
  ORDER BY player_code ASC
  ")
  
  # Added house query to get community area into the playerround table
  house <- sqldf("
  SELECT h.*, c.name AS community_name
  FROM [house] AS h
  LEFT JOIN [community] as c
  ON c.id = h.community_id
  ")
  
  playerround <- sqldf("
  SELECT pr.*, hg.code AS house_code, h.community_name
  FROM [playerround] AS pr
  LEFT JOIN [housegroup] AS hg
  ON pr.final_housegroup_id = hg.id
  LEFT JOIN [house] AS h
  ON hg.code = h.code
  ORDER BY pr.player_code ASC
  ")
  #END CHANGES
  
  # Move to tests/test_welfare levels ----
  
  # CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to playerround and df_income_dist
  # Map numeric welfaretype_id to welfare text levels
  #converts numeric welfare IDs into human‑readable ordered categories
  # Only if there are exactly six distinct IDs. Otherwise, it warns you that the mapping isn’t valid.
  welfare_labels <- c("Very Low",
                      "Low",
                      "Low-average",
                      "High-average",
                      "High",
                      "Very High")
  
  wt_codes <- sort(unique(playerround$welfaretype_id))
  
  if (length(wt_codes) == 6) {
    playerround$welfare_level <- factor(
      welfare_labels[match(playerround$welfaretype_id, wt_codes)],
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
  # Add to the personalmeasure the playerround selection to filter per player, table, round and cost of measures
  personalmeasure <- sqldf("
  SELECT pr.gamesession_name, pm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought, pr.final_housegroup_id, pr.mortgage_payment
  FROM [personalmeasure] AS pm
  LEFT JOIN [playerround] AS pr
  ON pm.playerround_id = pr.playerround_id
  ORDER BY pr.player_code ASC
  ")

  # Add to the personalmeasure the housegroup selection to calculate the cost of measures
  personalmeasure <- sqldf("
  SELECT pm.*, hg.code AS house_code, hg.last_sold_price, hg.owner_id
  FROM [personalmeasure] AS pm
  LEFT JOIN [housegroup] AS hg
  ON pm.final_housegroup_id = hg.id
  ORDER BY pm.player_code ASC
  ")
  
  # Add to the measuretype selection to compare it with the costs of measures per round
  personalmeasure <- sqldf("
  SELECT pm.*, m.short_alias, m.cost_absolute, m.cost_percentage_income, m.cost_percentage_house, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [personalmeasure] AS pm
  LEFT JOIN [measuretype] AS m
  ON pm.measuretype_id = m.id
  ORDER BY pm.player_code ASC
  ")
  str(personalmeasure)
  
  # CHANGES vjcortesa-3: Corrected the calculation of the personal measure with the last_sold price instead of the mortgage_payment*10
  personalmeasure_cumulative <- retrieve_personalmeasure_cumulative(personalmeasure)
  
  # Add to the housemeasure the housegroup selection to calculate the cost of measures
  housemeasure <- sqldf("
  SELECT hm.*, hg.code AS house_code, hg.owner_id
  FROM [housemeasure] AS hm
  LEFT JOIN [housegroup] AS hg
  ON hm.housegroup_id = hg.id
  ORDER BY hg.owner_id
  ")
  
  # Added the game_session_name to the measures tables
  housemeasure <- sqldf("
  SELECT pr.gamesession_name, hm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought
  FROM [housemeasure] AS hm
  LEFT JOIN [playerround] AS pr
  ON hm.owner_id = pr.player_id AND hm.bought_in_round = pr.groupround_round_number
  ORDER BY pr.player_code ASC
  ")
  
  
  # Add the measuretype variables to calculate the costs of house measures per round 
  housemeasure <- sqldf("
  SELECT hm.*, m.short_alias, m.cost_absolute, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [housemeasure] AS hm
  LEFT JOIN [measuretype] AS m
  ON hm.measuretype_id = m.id
  ORDER BY hm.player_code ASC
  ")
  
  # Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
  initialhousemeasure <- sqldf("
  SELECT ihm.*, h.code AS house_code, h.rating, h.initial_pluvial_protection, h.initial_fluvial_protection, h.community_id
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [house] AS h
  ON ihm.house_id = h.id
  ORDER BY ihm.house_id ASC
  ")
  
  # Added to the initialhouse measure the house_code to identify in the housemeasure calculation which measures came already implemented when player bought the house
  initialhousemeasure <- sqldf("
  SELECT ihm.*, m.short_alias 
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [measuretype] AS m
  ON ihm.measuretype_id = m.id
  ORDER BY ihm.house_id ASC
  ")
  
  #The subquery checks if there is at least one measure from the initialhousemeasure table is in the housemeasure table according to the house_code 
  housemeasure <- sqldf("
  SELECT 
    hm.*,
    CASE 
      WHEN EXISTS (
        SELECT TRUE FROM [initialhousemeasure] AS ihm
        WHERE ihm.measuretype_id = hm.measuretype_id
          AND ihm.house_code = hm.house_code
      )
      THEN TRUE ELSE FALSE
    END AS initialhousemeasure
  FROM [housemeasure] AS hm
  ")
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
  housemeasure_cumulative <- retrieve_housemeasure_cumulative(housemeasure)
   
  
  #Add to playerround the calculated costs of measures
  playerround <- sqldf("
  SELECT pr.*, calculated_costs_house_measures
  FROM [playerround] AS pr
  LEFT JOIN [housemeasure_cumulative] AS hmc
  ON pr.player_code = hmc.player_code AND pr.groupround_round_number = hmc.groupround_round_number
  ORDER BY pr.player_code ASC
  ")
  
  playerround <- sqldf("
  SELECT pr.*, calculated_costs_personal_measures
  FROM [playerround] AS pr
  LEFT JOIN [personalmeasure_cumulative] AS pmc
  ON pr.player_code = pmc.player_code AND pr.groupround_round_number = pmc.groupround_round_number
  ORDER BY pr.player_code ASC
  ")
  
  # playerround <- append_playerround_costmeas(playerround, dataset_date)
  
  # Add to question score the question, question item and player_round tables relevant variables
  questionscore <- sqldf("
  SELECT 
    qs.id AS answer_id, qs.answer, qs.late_answer,qi.name AS answer_option, CAST(qs.answer AS INTEGER) || ' - ' || qi.name AS answer_plus_option, 
    qs.question_id, q.name AS question_name, q.description AS question_description,
    qs.playerround_id, pr.groupround_round_number, pr.player_code, pr.group_name, pr.gamesession_name
  FROM questionscore AS qs
  LEFT JOIN question AS q
    ON qs.question_id = q.id
  LEFT JOIN questionitem AS qi
    ON qs.answer = qi.code
   AND qs.question_id = qi.question_id
  LEFT JOIN  playerround AS pr
   ON qs.playerround_id = pr.playerround_id
  ")
  
  questionitem <- sqldf("
  SELECT 
    qi.id AS questionitem_id, qi.code AS answer_code, qi.name AS answer_name, 
    CAST(qi.code AS INTEGER) || ' - ' || qi.name AS answercode_plus_name,
    q.name AS question_name, q.description AS question_description
  FROM questionitem AS qi
  LEFT JOIN question AS q
    ON qi.question_id = q.id
  ")
  
  
  # Filter the playerround dataset for the income distribution
  
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
  
  # Run the query to filter the playerround dataframe with the var_income_dist 
  df_income_dist <- sqldf(paste0("
  SELECT ", col_income_dist, "
  FROM playerround
  "))
  
  # Step 3: Income distribution specification ---------------------------------------------------
  # CHANGES vjcortesa-7: Added to the list_income_dist file the tables added in the code
  # Create a list with the tables used in the calculation
  list_income_dist <- list(
    df_income_dist = df_income_dist,
    playerround = playerround,
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
    player = player,
    gamesession = gamesession
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