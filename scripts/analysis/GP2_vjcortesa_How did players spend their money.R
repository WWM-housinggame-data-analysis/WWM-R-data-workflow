# Load necessary libraries
library(readxl)
library(readr)
library(openxlsx)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load for database manipulation
library(sqldf)
# Load for data manipulation
library(dplyr)
# Load for excel manipulation
library(writexl)
# Load for data visualisation
library(tidyr)
library(ggplot2)
library(ggtext)

# # Load required libraries
# required_libs <- c("readr", "openxlsx", "rstudioapi")
# for (lib in required_libs) {
#   if (!requireNamespace(lib, quietly = TRUE)) {
#     stop(paste("Package `", lib, "` not found. Please run `install.packages('", lib, "') to proceed", sep = ""))
#   }
# }
# 
# library(readr)
# library(openxlsx)
# library(rstudioapi)

# Get the path of the current script (works in RStudio)
getwd() # when you open Rstudio by clinking on .Rproj, default working directory is R-data-analysis-BEPs/

# Load required functions
function_path <- file.path("scripts", "functions","manage-data")
source(file.path(function_path, "combine_csvs_to_excel.R"))
source(file.path(function_path, "read_all_csvs.R"))
source(file.path(function_path, "retrieve_dbtables.R"))
source(file.path(function_path, "format_income_dist.R"))


# Read the database folder to create accordingly the dataframe tables
folder_path <- file.path("data", "database-tables") 
folder_name <- "251007-housinggame-session-20-verzekeraars-masterclass"

# Create a combined excel with all database tables to have as a reference their initial configuration
#combine_csvs_to_excel(folder_path, folder_name) #avoid repeating read_all_csvs workflow within this function

# retrieve matched database tables inside list
list_income_dist_2409 <- retrieve_dbtables(folder_path, "housinggame_session_16_240924_EPA_IntroDays_Ommen")
list_income_dist_2509 <- retrieve_dbtables(folder_path, "housinggame_session_19_250923_EPA_IntroDays_Overasselt")
list_income_dist_2510 <- retrieve_dbtables(folder_path, "housinggame_session_20_251007_VerzekeraarsMasterClass")

# Assign each table to a variable in the global environment
# Not ideal because makes the global environment crowded with unnecessary variables
# list2env(csv_data_list, envir = .GlobalEnv)

df_income_dist_2409 <- format_income_dist(list_income_dist_2409$df_income_dist)
df_income_dist_2509 <- format_income_dist(list_income_dist_2509$df_income_dist)
df_income_dist_2510 <- format_income_dist(list_income_dist_2510$df_income_dist)



# trying script for the plot
session_name <- folder_name
group <- "all"
round <- "all"


# # Calculate the number of players per round income
# if (round != "all") {
#   income_dist_x <- df_income_dist %>% filter(groupround_round_number == round)
# } else {
#   income_dist_x <- df_income_dist %>% filter(groupround_round_number == "0")
# }
# 
# income_dist_x <- income_dist_x %>% count(round_income, name = "players_count")
# income_dist_x$round_income <- income_dist_x$round_income/1000
# income_dist_x$label <- c("Very Low", 
#                          "Low" , 
#                          "Low-average", 
#                          "High-average", 
#                          "High", 
#                          "Very High")
# 
# for (i in 1:nrow(income_dist_x)) {
#   income_dist_x$label[i] <- paste(income_dist_x$round_income[i],"<br><img src='icons/Player.png' width='5'/>", income_dist_x$players_count[i])
# }

# Filter the dataset with all players plot
# plot_player_all <- data.frame(p_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7"),
#                               selected = c(1, 1, 1, 1, 1, 1, 1))
# 
# players <- plot_player_all
# player_plot = "all"

# player_plot <- ""

# for (i in 1:nrow(players)) {
#   if (players$selected[i] != "0") {
#     if (nchar(player_plot) == 0) {
#       player_plot <- players$p_code[i]
#       nplayer <- 1
#       fdataset <- dataset %>% filter(p_code == players$p_code[i])
#     } else {
#       player_plot <- paste(player_plot, "-", players$p_code[i])
#       nplayer <- nplayer +1
#       fdataset <- rbind(fdataset, dataset[dataset$p_code == players$p_code[i], ])
#     }
#   }
# }
# 
# if (nplayer == nrow(players)) {
  player_plot = "all"
# }

# Filter the dataset according to the player(s) to plot
income_dist <- df_income_dist



# Plot title definition
plot_title <- "How did players spend their money in average?"
plot_subtitle <- paste("Session:", session_name, "\nGroup:", group, "\nPlayer(s):", player_plot, "\nRound:", round)
plot_name <- paste("IncomeDistribution_","Session_",session_name, "Group_", group, "Player_", player_plot,"Round_", round,".png")

# Calculate the mean values per dataset variable
# income_dist_ave1 <- income_dist %>%
#   group_by(round_income_grp, p_code) %>%
#   summarise(
#     income_minus_living = sum(income_minus_living, na.rm = TRUE),
#     profit_minus_spent_savings_house_moving = sum(profit_minus_spent_savings_house_moving, na.rm = TRUE),
#     mortgage_payment = sum(mortgage_payment, na.rm = TRUE),
#     cost_taxes = sum(cost_taxes, na.rm = TRUE),
#     paid_debt = sum(paid_debt, na.rm = TRUE),
#     cost_house_measures_bought = sum(cost_house_measures_bought, na.rm = TRUE),
#     cost_personal_measures_bought = sum(cost_personal_measures_bought, na.rm = TRUE),
#     cost_fluvial_damage = sum(cost_fluvial_damage, na.rm = TRUE),
#     cost_pluvial_damage = sum(cost_pluvial_damage, na.rm = TRUE),
#     spendable_income = sum(spendable_income, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   group_by(round_income_grp) %>%
#   summarise(
#     income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
#     profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
#     mortgage_payment = round(mean(mortgage_payment, na.rm = TRUE), 2),
#     cost_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
#     paid_debt = round(mean(paid_debt, na.rm = TRUE), 2),
#     cost_house_measures_bought = round(mean(cost_house_measures_bought, na.rm = TRUE), 2),
#     cost_personal_measures_bought = round(mean(cost_personal_measures_bought, na.rm = TRUE), 2),
#     cost_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
#     cost_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
#     spendable_income = round(mean(spendable_income, na.rm = TRUE), 2)
#   ) %>%
#   ungroup()
  
income_dist_ave <- income_dist %>%
  group_by(round_income_grp) %>%
  summarise(
    ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
    ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
    ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
    ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
    ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
    ave_measures = round(mean(cost_house_measures_bought, na.rm = TRUE), 2),
    ave_satisfaction = round(mean(cost_personal_measures_bought, na.rm = TRUE), 2),
    ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
    ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
    ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
  ) %>%
  ungroup()

income_dist_n <- income_dist %>%
  select(round_income_grp, p_code) %>%
  group_by(round_income_grp) %>%
  summarise(N = n()) %>%
  ungroup()

# income_dist_n1 <- income_dist %>%
#   select(round_income_grp, p_code) %>%
#   group_by(round_income_grp) %>%
#   summarise(N = n_distinct(p_code)) %>%
#   ungroup()

income_dist_sumtable <- income_dist_n %>%
                        inner_join(income_dist_ave, by = join_by(name))

#Reference dataset to draw area and line
income_dist_plt_ref <- income_dist %>%
  group_by(round_income) %>% 
  summarise(
    ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
    ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
  )

# Adding an index to plot the area and bars together
income_dist_plt_ref$Index <- seq_len(nrow(income_dist_plt_ref))
income_dist_plt$Index <- seq_len(nrow(income_dist_plt))

# Categorise the income distribution per plot category
line_spendable = income_dist_plt_ref %>% select(ave_Spendable, Index)
bars_expenses <- income_dist_plt %>% select(ave_debt, ave_mortgage, ave_taxes, ave_profit_minus_spent_savings_house_moving, ave_satisfaction, ave_measures, ave_fluvial_damage, ave_pluvial_damage, Index)
area_income <- income_dist_plt_ref %>% select(ave_income_minus_living, Index)

# Set x range of the plot
# Calculate limits
x_min <- min(income_dist$Index) -0.5 #starts from zero
x_max <- max(income_dist$Index) + 0.5
y_max <- max(income_dist_plt$ave_income_minus_living)
y_min <- min(income_dist_plt$ave_profit_minus_spent_savings_house_moving)
w = 0.9

# Formatting the dataset to plot per category
income_dist_formatted <- income_dist %>%
  pivot_longer(cols = where(is.numeric), names_to = "Cost_Type",
               values_to = "Cost_Value") %>%
  mutate(Cost_Type = factor(Cost_Type))



bars_expenses_formatted <- bars_expenses %>%
  pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")

area_income_formatted <- area_income %>%
  pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")

# Formatting the dataset to stack the bars following the given order

bar_expenses_cols <- c("cost_personal_measures_bought", "cost_fluvial_damage",
                      "cost_pluvial_damage", "cost_house_measures_bought",
                      "paid_debt", "cost_taxes", "mortgage_payment",
                      "profit_minus_spent_savings_house_moving")


bars_expenses_formatted$Type <- factor(
  bars_expenses_formatted$Type,
  levels = c(
    "ave_satisfaction",
    "ave_fluvial_damage",
    "ave_pluvial_damage",
    "ave_measures",
    "ave_debt",
    "ave_taxes",
    "ave_mortgage",
    "ave_profit_minus_spent_savings_house_moving"
  )
)

plot <- ggplot() +
  geom_area(data = area_income_formatted,
            aes(x = Index, y = Value, fill = Type),
            alpha = 0.6
  ) +
  geom_bar(data = bars_expenses_formatted,
           aes(x = Index, y = Value, fill = Type),
           stat = "identity",
           position = "stack",
           width = w
  ) +
  geom_line(
    data = line_spendable,
    aes(x = Index,
        y = ave_Spendable,
        color = "ave_Spendable"),
    linewidth = 1.2) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    color = "Category"
  ) +
  # Custom fill colors to what is plotted in the legend
  scale_color_manual(
    name = "Round Spendable \n Income",
    values = c(
      "ave_Spendable" = "black"),
    labels = c(
      "ave_Spendable" = "Round income - costs") 
  ) +
  scale_fill_manual(
    name = "Round costs",
    values = c(
      "ave_income_minus_living" = "#E1BB70",
      "ave_debt" = "black",
      "ave_satisfaction" = "#dfaba3",
      "ave_measures" = "white",
      "ave_profit_minus_spent_savings_house_moving" =  "#a3a3a3",
      "ave_mortgage" = "#cccccc",
      "ave_taxes" = "#dddddd",
      "ave_fluvial_damage" = "#79A2C5",
      "ave_pluvial_damage" = "#79BCC5"),
    labels = c(
      "ave_income_minus_living" = "Income - Living costs",
      "ave_debt" = "Debt",
      "ave_satisfaction" = "Satisfaction",
      "ave_measures" = "Measures",
      "ave_mortgage" = "Mortgage",
      "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
      "ave_taxes" = "Taxes",
      "ave_fluvial_damage" = "River damage",
      "ave_pluvial_damage" = "Rain damage")
  ) +
  guides(
    fill = guide_legend(title = "Round costs"),
    color = guide_legend(title = "Round Spendable Income")
  ) +
  #Y-axis formatting
  scale_y_continuous(
    labels = function(y) y / 1000,
    name = "Game Currency (k)"
  ) +
  scale_x_continuous(
    name = "Round income (k) \n Players per class",
    breaks = c(1, 2, 3, 4, 5, 6),
    labels = income_dist_x$label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 0, hjust = 0.5), ##takes rich html
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.title.position = "plot"
  )

plot(plot)

plot <- ggplot(income_dist_formatted) +
  
  geom_bar(data = ~ .x |>
             dplyr::filter(Cost_Type %in% bar_expenses_cols) |>
             dplyr::mutate(Cost_Type = forcats::fct_relevel(Cost_Type, bar_expenses_cols)),
           aes(x = round_income_grp, y = Cost_Value, fill = Cost_Type),
           stat = "summary", fun = "mean", position = "stack",
           na.rm = TRUE, width = w) +
  
  scale_fill_manual(
    name = "Round costs",
    values = c(
      "paid_debt" = "black",
      "cost_personal_measures_bought" = "#dfaba3",
      "cost_house_measures_bought" = "white",
      "profit_minus_spent_savings_house_moving" = "#a3a3a3",
      "mortgage_payment" = "#cccccc",
      "cost_taxes" = "#dddddd",
      "cost_fluvial_damage" = "#79A2C5",
      "cost_pluvial_damage" = "#79BCC5"),
    labels = c(
      "paid_debt" = "Debt",
      "cost_personal_measures_bought" = "Satisfaction",
      "cost_house_measures_bought" = "Measures",
      "mortgage_payment" = "Mortgage",
      "profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
      "cost_taxes" = "Taxes",
      "cost_fluvial_damage" = "River damage",
      "cost_pluvial_damage" = "Rain damage")
    ) +

  guides(
    fill = guide_legend(title = "Round costs")
    ) +
  
  #Y-axis formatting
  scale_y_continuous(
    labels = function(y) y / 1000,
    name = "Game Currency (k)"
  ) +
  
  scale_x_discrete(
    name = "Round income (k) \n Players per class",
    labels = income_dist_x$round_income
  ) +
  
  theme_minimal() +

  theme(
    axis.text.x = element_markdown(angle = 0, hjust = 0.5) ##takes rich html
  )

#interactive_plot <- ggplotly(plot)
#htmlwidgets::saveWidget(interactive_plot, "interactive_plot.html")
#browseURL("interactive_plot.html")
ggsave(plot_name, width = 12, height = 6, dpi = 300)
plot(plot)

# Write to Excel with sheet names matching table names
#write_xlsx(list_income_dist, "folder_name_income_dist.xlsx")
# tryCatch({
#   write_xlsx(list_income_dist, paste0(dirname(function_path),"/data_output/Income_dist_", folder_name, ".xlsx"))
#   message("File written successfully.")
# }, error = function(e) {
#   message("Error: ", e$message)
# })
