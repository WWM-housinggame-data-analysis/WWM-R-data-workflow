# Load necessary libraries ----
library(readxl)
library(readr)
library(openxlsx)

## Load for database manipulation
library(sqldf)

## Load for data manipulation
library(dplyr)
library(stringr)
library(tidyr)

## Load for excel manipulation
library(writexl)

## Load for data visualisation
library(ggplot2)
library(ggtext)

library(shiny)
library(bslib)
library(plotly)

# Set defaults ----
# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")
RAWDATA_PATH <- file.path("data", "raw-dbtables")
PREPRDATA_PATH <- file.path("data", "preprocessed-dbtables")

SELECTED_DBTABLES <- c("gamesession", "group", "groupround",
                       "playerround", "player","measuretype",
                       "personalmeasure","housemeasure", "housegroup",
                       "community","house","initialhousemeasure",
                       "question","questionitem","questionscore")

INCOME_DIST_CATEGCOLS <- c("gamesession_name", "group_name", "playerround_id", "player_id", "player_code", "house_code",
                           "groupround_id", "groupround_round_number", "welfaretype_id", "community_name")


# Central colour/label dictionaries (names must match cost_type in data)
EXPENSE_BARCOLS <- c("cost_personal_measures_bought", "cost_fluvial_damage",
                       "cost_pluvial_damage", "cost_house_measures_bought",
                       "paid_debt", "cost_taxes", "mortgage_payment",
                       "profit_minus_spent_savings_house_moving")
w = 0.9

fill_values_all <- c(
  "paid_debt"                         = "black",
  "cost_personal_measures_bought"     = "#dfaba3",
  "cost_house_measures_bought"        = "white",
  "profit_minus_spent_savings_house_moving" = "#a3a3a3",
  "mortgage_payment"                  = "#cccccc",
  "cost_taxes"                        = "#dddddd",
  "cost_fluvial_damage"               = "#79A2C5",
  "cost_pluvial_damage"               = "#79BCC5"
)


fill_labels_all <- c(
  "paid_debt"                         = "Debt",
  "cost_personal_measures_bought"     = "Satisfaction",
  "cost_house_measures_bought"        = "Measures",
  "mortgage_payment"                  = "Mortgage",
  "profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
  "cost_taxes"                        = "Taxes",
  "cost_fluvial_damage"               = "River damage",
  "cost_pluvial_damage"               = "Rain damage"
)

# Source files ----

# Get the path of the current script
## when you open Rstudio by clinking on .Rproj, default working directory is folder where .Rproj is stored
getwd()

# Load required functions
source(file.path(FUNCTION_PATH, "list-upload-export-dbtables.R"))
source(file.path(FUNCTION_PATH, "preprocess-dbtables.R"))


# Data Workflow ----

# Read all tables in the database folder to create accordingly the dataframe tables inside list
gamesession_data_list <- upload_selected_dbtables(RAWDATA_PATH, "housinggame_session_20_251007_VerzekeraarsMasterClass")

income_dist_list <- list()

for (session_path in names(gamesession_data_list)) {
  income_dist_list[[session_path]] <- preprocess_dbtables(gamesession_data_list[[session_path]])
}

income_dist_data <- income_dist_list[[session_path]][["income_dist_df"]]

# Shiny App ----

ui <- page_navbar(
  title = "WhereWeMove Dashboard",
  bg = "#2D89C8",
  inverse = TRUE,
  
  nav_panel(
    title = "Game Play",
    page_sidebar(
      sidebar = sidebar(
        title = "Choices and effects",
        bg = "white",
        accordion(
          multiple = FALSE,   # only one open at a time
          accordion_panel("1: Where players live"),
          accordion_panel(
            "2: Player spending",
            # varSelectInput(
            #   "color_by", "Color by",
            #   penguins[c("species", "island", "sex")],
            #   selected = "species"
            # ),
            # varSelectInput(
            #   "table_by", "Table",
            #   penguins[c("species", "island", "sex")],
            #   selected = "species"
            # ),
            checkboxGroupInput("player", "Player:",
                               choices = c("All", as.character(unique(df_income_dist$player_code))),
                               selected = "All"),
            checkboxGroupInput("cost_type", "Cost_Types:",
                               choices = c("All", EXPENSE_BARCOLS),
                               selected = "All")
          ),
          accordion_panel("3: Selected measures"),
          accordion_panel("4: Flood in gameplay"),
          accordion_panel("5: Damage & satisfaction")
        )
      ),
      
      mainPanel(
        accordion(
          open = c("All Rounds"),
          accordion_panel(
            "All Rounds",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("plot_all"), verbatimTextOutput("debug")),
                        tabPanel("Summary", verbatimTextOutput("summary_all")),
                        tabPanel("Table", tableOutput("table_all"))
            )
          ),
          accordion_panel(
            "Round 1",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot_r1")),
                        tabPanel("Summary", verbatimTextOutput("summary_r1")),
                        tabPanel("Table", tableOutput("table_r1"))
            )
          ),
          accordion_panel(
            "Round 2",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot_r2")),
                        tabPanel("Summary", verbatimTextOutput("summary_r2")),
                        tabPanel("Table", tableOutput("table_r2"))
            )
          ),
          accordion_panel(
            "Round 3",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot_r3")),
                        tabPanel("Summary", verbatimTextOutput("summary_r3")),
                        tabPanel("Table", tableOutput("table_r3"))
            )
          )
        )
      )
    )
  ),
  
  
  
  nav_panel(title = "Game Settings", p("First page content.")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("About WhereWeMove", href = "https://seriousgaming.tudelft.nl/games/")),
    nav_item(tags$a("WhereWeMove info", href = "https://pure.tudelft.nl/ws/portalfiles/portal/180909041/WhereWeMove-Brochure_Final.pdf")),
    nav_item(tags$a("Facilitator website", href = "https://housing-game.tbm.tudelft.nl/housinggame-facilitator/jsp/facilitator/login.jsp")),
    nav_item(tags$a("Player website", href = "https://housing-game.tbm.tudelft.nl/housinggame-player/jsp/player/login.jsp"))
  )
)





server <- function(input, output) {
  
  income_dist_reactive <- reactive({df_income_dist})
  
  selected_players <- reactive({filter_selected_categs(input$player, as.character(unique(income_dist_reactive()$player_code)))})
  
  # selected_players <- reactive({
  #   req(input$player)
  #   # remove the special label
  #   req_types <- as.character(unique(income_dist_reactive()$player_code))
  #   # if All is selected OR none selected -> treat as all
  #   if ("All" %in% as.vector(input$player)) {
  #     req_types
  #   } else {
  #     intersect(input$player, req_types)
  #   }
  # })
  
  selected_costtypes <- reactive({filter_selected_categs(input$cost_type, EXPENSE_BARCOLS)})
  
  # selected_costtypes <- reactive({
  #   req(input$cost_type)
  #   # remove the special label
  #   req_types <- EXPENSE_BARCOLS
  #   # if All is selected OR none selected -> treat as all
  #   if ("All" %in% as.vector(input$cost_type)) {
  #     req_types
  #   } else {
  #     intersect(input$cost_type, req_types)
  #   }
  # })
  
  # Reactive dataset grouped by the chosen color_by variable
  income_dist_ave <- reactive({retrieve_average_table(income_dist_reactive())})
  
  # income_dist_ave <- reactive({income_dist_reactive() %>%
  #   group_by(income_grp) %>%
  #   summarise(
  #     ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
  #     ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
  #     ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
  #     ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
  #     ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
  #     ave_measures = round(mean(cost_house_measures_bought, na.rm = TRUE), 2),
  #     ave_satisfaction = round(mean(cost_personal_measures_bought, na.rm = TRUE), 2),
  #     ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
  #     ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
  #     ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
  #   ) %>%
  #   ungroup()
  # })
  
  income_dist_n <- reactive({retrieve_n_table(plot_data)})
  
  # income_dist_n <- reactive({income_dist_reactive() %>%
  #   select(income_grp, player_code) %>%
  #   group_by(income_grp) %>%
  #   summarise(N = n()) %>%
  #   ungroup()
  # })
  
  grouped_data <- reactive({income_dist_n() %>%
    inner_join(income_dist_ave(), by = join_by(income_grp))
  })
  
  
  gg_plot <- reactive({get_costs_barplot(income_dist_reactive, selected_costtypes, selected_players)})
  gg_plot1 <- reactive({get_costs_barplot(reactive({income_dist_reactive() %>% filter(groupround_round_number %in% 1)}), selected_costtypes, selected_players)})
  gg_plot2 <- reactive({get_costs_barplot(reactive({income_dist_reactive() %>% filter(groupround_round_number %in% 2)}), selected_costtypes, selected_players)})
  gg_plot3 <- reactive({get_costs_barplot(reactive({income_dist_reactive() %>% filter(groupround_round_number %in% 3)}), selected_costtypes, selected_players)})
  
  # Connect plots
  output$plot_all <- renderPlotly({
    
    obj <- gg_plot()         # obj is list(plot, data)
    gp  <- obj$plot
    df  <- obj$data          # summary_df with mean_value & n
    stacked_vec <- obj$barfill
    
    plt <- ggplotly(gp)
    plt <- layout(plt, hovermode = "closest")
    
    # We need per-trace (cost_type) vectors of value_k and n in the same order as trace points.
    # Plotly creates one trace per cost_type.
    # For each trace name (fullData.name), subset df and order by the x (round_income) factor
    # to match bar positions.
    
    # Get x positions order as they appear in the first trace
    x_order <- plt$x$data[[1]]$x
    
    for (i in seq_along(plt$x$data)) {
      tr      <- plt$x$data[[i]]
      catname <- tr$name                 # equals legend label (fill_labels_all)
      xs      <- tr$x                    # x values for this trace
      
      # Map legend label back to cost_type value. If you used labels, we need a reverse map:
      # build it once outside and keep it around; for demo we rebuild quickly:
      # Suppose you still have 'stacked_vec' and 'fill_labels_all' in scope. If not, create a reverse map:
      rev_map <- setNames(names(fill_labels_all[stacked_vec]), fill_labels_all[stacked_vec])
      
      # If catname equals the label, translate to original cost_type:
      # cost_type_value <- rev_map[catname]
      # If you didn't customize labels, catname is directly the cost_type.
      
      # If using labels, do:
      cost_type_value <- if (!is.na(rev_map[catname])) rev_map[catname] else catname
      
      # For simplicity here, assume catname == cost_type (no label remap). If you used labels,
      # add the reverse mapping shown above.
      cost_type_value <- catname
      
      # Subset summary data for this cost_type and order by x
      sub <- df %>% filter(cost_type == cost_type_value)
      
      # Ensure the same x order
      sub <- sub %>% mutate(income_grp = factor(income_grp, levels = x_order)) %>%
        arrange(income_grp)
      
      value_k <- sub$mean_value / 1000
      n_vec   <- sub$n
      
      plt$x$data[[i]]$customdata <- cbind(value_k, n_vec)
      plt$x$data[[i]]$hovertemplate <- paste0(
        "<b>%{fullData.name}</b><br>",
        "Mean: %{customdata[0]:.2f}k<br>",
        "N: %{customdata[1]}",
        "<extra></extra>"
      )
    }
    
    plt
  })
  
  
  
  # output$plot_all <- renderPlotly({
  #   
  #   # Build the ggplot first
  #   gp <- gg_plot()              # this is your reactive() that returns a ggplot
  #   
  #   # Convert to plotly
  #   plt <- ggplotly(gp)          # do not pass tooltip=...; we’ll control via hovertemplate
  #   
  #   # 1) Use unified hover → single tooltip box per x (bar)
  #   plt <- layout(plt, hovermode = "x unified")
  #   
  #   # 2) Format each stacked trace’s line in the unified tooltip
  #   #    - Use the trace name (matches your fill labels) + y value
  #   #    - Show values in 'k' using customdata (y/1000)
  #   for (i in seq_along(plt$x$data)) {
  #     yi <- plt$x$data[[i]]$y
  #     if (!is.null(yi)) {
  #       plt$x$data[[i]]$customdata <- yi / 1000  # value in k for hover
  #       # Bold category name, show value in k, and hide the extra box
  #       plt$x$data[[i]]$hovertemplate <- paste0(
  #         "<b>%{fullData.name}</b>: %{customdata:.1f}k<extra></extra>"
  #       )
  #     }
  #   }
  #   
  #   plt
  # })
  
  output$plot_r1  <- renderPlot({ gg_plot1() })
  output$plot_r2  <- renderPlot({ gg_plot2() })
  output$plot_r3  <- renderPlot({ gg_plot3() })
  
  
  # Optional: inspect reactive rows
  output$debug <- renderPrint({
    paste(
      paste("Rows:", nrow(income_dist_reactive())),
      paste("Costs:", length(selected_players())),
      sep = "\n")
  })

  
  # Summaries (update based on color_by choice)
  output$summary_all <- renderPrint({ summary(grouped_data()) })
  output$summary_r1  <- renderPrint({ summary(grouped_data()) })
  output$summary_r2  <- renderPrint({ summary(grouped_data()) })
  output$summary_r3  <- renderPrint({ summary(grouped_data()) })
  
  # Tables (update based on color_by choice)
  output$table_all <- renderTable({ grouped_data() })
  output$table_r1  <- renderTable({ grouped_data() })
  output$table_r2  <- renderTable({ grouped_data() })
  output$table_r3  <- renderTable({ grouped_data() })
}
shinyApp(ui, server)