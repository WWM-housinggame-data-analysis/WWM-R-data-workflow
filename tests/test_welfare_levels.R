# Read all tables in the folder with the custom function
csv_data_list <- read_all_csvs(folder_path, folder_name)$datalist

GP2_tables <- c("gamesession", "group", "groupround",
                "playerround", "player","measuretype",
                "personalmeasure","housemeasure", "housegroup",
                "community","house","initialhousemeasure",
                "question","questionitem","questionscore")

# Build a new list with only the elements you want
GP2_data <- csv_list_2510[GP2_tables]
names(GP2_data)

player <- GP2_data[["player"]]

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

wt_codes <- sort(unique(player$welfaretype_id))

if (length(wt_codes) != 6) {
  warning("Expected 6 distinct welfaretype_id values, but found ",   #make sure that it returns warning if not applicable
          length(wt_codes),
          ". welfare_level not created.")
}