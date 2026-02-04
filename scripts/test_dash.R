income_dist_reactive <- income_dist_list[[gamesession_paths[names(gamesession_paths) %in% "housinggame_session_20_251007_VerzekeraarsMasterClass"]]][["income_dist_df"]]

required_tables <- as.character(unique(income_dist_reactive$group_name))

selected_table <- filter_selected_categs("All", required_tables)

selected_bar_segments <- filter_selected_categs("All", names(EXPENSE_BARCOLS))

selected_columns <- EXPENSE_BARCOLS[names(EXPENSE_BARCOLS) %in% selected_bar_segments]

grouped_data <-group_summary_table(income_dist_reactive, selected_table)

gg_plot <- prepare_visualize_GP1(income_dist_reactive, selected_columns, selected_table, game_round = "All", fill_values_all)


df <- income_dist_reactive

stacked_vec <- selected_costtypes

game_round <- "All"
