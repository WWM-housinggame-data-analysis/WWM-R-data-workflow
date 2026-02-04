income_dist_df <- income_dist_list[[gamesession_paths[names(gamesession_paths) %in% "housinggame_session_20_251007_VerzekeraarsMasterClass"]]][["income_dist_df"]]

required_tables <- as.character(unique(income_dist_df$group_name))

selected_table <- filter_selected_categs("All", required_tables)

selected_bar_segments <- filter_selected_categs("All", names(EXPENSE_BARCOLS))

selected_columns <- EXPENSE_BARCOLS[names(EXPENSE_BARCOLS) %in% selected_bar_segments]

grouped_data <-group_summary_table(income_dist_df, selected_table)

plot_data <- prepare_GP1_data(income_dist_df, selected_columns, selected_table, game_round = "All", fill_values_all)


bar_df                <- plot_data$bar_df
scatter_df            <- plot_data$scatter_df
selected_bar_segments <- plot_data$selected_bar_segments
xlevels               <- plot_data$xlevels
