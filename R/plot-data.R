# # Set all default variables or global options and all the path variables at the top of the code.
# 
# FUNCTION_PATH <- file.path("R")
# 
# # Load required functions
# source(here(file.path(FUNCTION_PATH, "constants.R")))
# 
# # Build plot on the aggregated data (geom_col)
# create_GP1_barplot <- function(summary_df, ave_data, stacked_vec, fill_values_all, fill_labels_all, scale_factor) {
#   
#   stopifnot(is.data.frame(summary_df))
#   stopifnot(is.data.frame(ave_data))
#   
#   gp <- ggplot() +
#     
#     geom_col(data = summary_df, aes(x = .data[["xlabels"]], y = mean_value, fill = cost_type, group = cost_type),
#              position = "stack", na.rm = TRUE, width = BAR_WIDTH) +
#     
#     geom_line(data = ave_data,
#       aes(x = .data[["xlabels"]], y = ave_satisfaction_scaled, color = series, group = 1),
#       linewidth = 1.2) +
#     
#     geom_point(data = ave_data,
#               aes(x = .data[["xlabels"]], y = ave_satisfaction_scaled, color = series, group = 1),
#               size = 2) +
#     
#     scale_color_manual(
#       values = c(
#         "Average total satisfaction" = "darkgreen")
#     ) +
#     
#     scale_fill_manual(
#       values = fill_values_all[names(fill_values_all) %in% stacked_vec],
#       labels = fill_labels_all[names(fill_labels_all) %in% stacked_vec]
#     ) +
#              
#     scale_y_continuous(labels = function(y) y / K_FACTOR,
#                        name = "Game Currency (k)",
#                        
#                        sec.axis = sec_axis(
#                          ~ . / scale_factor,
#                          name = "Average total satisfaction"
#                        )) +
#       
#     scale_x_discrete(name = "Round income (k) \n Players per class") +
#     labs(x = NULL, fill = NULL, color = NULL) +
#     
#     theme_minimal() +
#     theme(axis.text.x = element_markdown(angle = 0, hjust = 0.5)) ##takes rich html
#   
#   return(list(plot = gp, data = summary_df, barfill = stacked_vec))
# }