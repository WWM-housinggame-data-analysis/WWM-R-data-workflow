retrieve_dbtables <- function(folder_path = "local path", folder_pattern = "csv_folder") {
  
  # Read all tables in the folder with the custom function
  csv_data_list <- read_all_csvs(folder_path, folder_name)$datalist
  
  # Assign a table to a variable in the global environment
  gamesession <- csv_data_list[["gamesession"]]
  group <- csv_data_list[["group"]]
  groupround <- csv_data_list[["groupround"]]
  playerround <- csv_data_list[["playerround"]]
  player <- csv_data_list[["player"]]
  
  # Rename the session name variable in the dataframe to avoid name overlap with the group name variable
  gamesession <- sqldf("SELECT * FROM gamesession")
  names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"
  
  # Add to the group dataframe the gamesession_name by the group = gamesession id
  # Leftjoin Keeps only the rows that have matching values in both data frames
  group <- sqldf(" SELECT g.*, gs.gamesession_name
                   FROM [group] AS g
                   LEFT JOIN [gamesession] AS gs
                   ON g.gamesession_id = gs.id
                 ")
  
  # Add to groupround the group variables selection
  groupround <- sqldf(" SELECT gr.*, g.name, g.gamesession_id, g.gamesession_name, g.scenario_id
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
  playerround <- sqldf(" SELECT pr.*, gr.round_number, gr.group_id, gr.group_name, gr.gamesession_id, gr.gamesession_name, gr.group_scenario_id
                         FROM [playerround] AS pr
                         LEFT JOIN [groupround] AS gr
                         ON pr.groupround_id = gr.id
                       ")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(playerround)[names(playerround) == "round_number"] <- "groupround_round_number"
  names(playerround)[names(playerround) == "scenario_id"] <- "group_scenario_id"
  
  # Rename id with the table prefix to avoid id ambiguity
  #names(player)[names(player) == "id"] <- "player_id"
  names(playerround)[names(playerround) == "id"] <- "playerround_id"
  
  # Filter the playerround dataset for the income distribution
  
  # Select the variables for the income distribution plot
  var_income_dist <- c(
    "playerround_id", "player_id", "groupround_id", "groupround_round_number",
    "round_income", "living_costs", "paid_debt",
    "profit_sold_house", "spent_savings_for_buying_house",
    "cost_taxes", "mortgage_payment",
    "cost_house_measures_bought", "cost_personal_measures_bought",
    "cost_fluvial_damage", "cost_pluvial_damage",
    "spendable_income"
  )
  
  # Collapse the column vector into a comma-separated string
  col_income_dist <- paste(var_income_dist, collapse = ", ")
  
  # Run the query to filter the playerround dataframe and add the player code
  df_income_dist <- sqldf(paste0("SELECT ", col_income_dist, ", p.code
                                  FROM playerround
                                  LEFT JOIN player AS p
                                  ON player_id = p.id
                                 "))
  
  # Rename columns added with the table prefix
  names(df_income_dist)[names(df_income_dist) == "code"] <- "p_code"
  
  # Run the query to filter the playerround dataframe and add the player code
  df_income_dist <- sqldf(paste0("SELECT * FROM df_income_dist
                                  ORDER BY p_code, groupround_round_number ASC;
                                 "))
  
  # Create a list with the tables used in the calculation
  list_income_dist <- list(df_income_dist = df_income_dist,
                           gamesession = gamesession,
                           group = group,
                           groupround = groupround,
                           player = player,
                           playerround = playerround
                           )
  
  return(list_income_dist)
  
}