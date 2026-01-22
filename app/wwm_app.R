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

library(here)
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


fill_values_all <- c(
  "ave_income_minus_living" = "#E1BB70",
  "ave_debt" = "black",
  "ave_satisfaction" = "#dfaba3",
  "ave_measures" = "#433E5E",
  "ave_profit_minus_spent_savings_house_moving" =  "#a3a3a3",
  "ave_mortgage" = "#cccccc",
  "ave_taxes" = "#dddddd",
  "ave_fluvial_damage" = "#79A2C5",
  "ave_pluvial_damage" = "#79BCC5")


fill_labels_all <- c(
  "ave_income_minus_living" = "Income - Living costs",
  "ave_debt" = "Debt",
  "ave_satisfaction" = "Satisfaction",
  "ave_measures" = "Measures",
  "ave_mortgage" = "Mortgage",
  "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
  "ave_taxes" = "Taxes",
  "ave_fluvial_damage" = "River damage",
  "ave_pluvial_damage" = "Rain damage")

# Source files ----

# Get the path of the current script
## when you open Rstudio by clinking on .Rproj, default working directory is folder where .Rproj is stored
getwd()

# Load required functions
source(here(file.path(FUNCTION_PATH, "list-upload-export-dbtables.R")))
source(here(file.path(FUNCTION_PATH, "preprocess-dbtables.R")))
source(here(file.path(FUNCTION_PATH, "transform-data.R")))
source(here(file.path(FUNCTION_PATH, "plot-data.R")))
source(here(file.path(FUNCTION_PATH, "interact-data.R")))


# Data Workflow ----

# Read all tables in the database folder to create accordingly the dataframe tables inside list
gamesession_data_list <- upload_selected_dbtables(RAWDATA_PATH, "housinggame_session_20_251007_VerzekeraarsMasterClass")

income_dist_list <- list()

for (session_path in names(gamesession_data_list)) {
  income_dist_list[[session_path]] <- preprocess_dbtables(gamesession_data_list[[session_path]])
}

income_dist_df <- income_dist_list[[session_path]][["income_dist_df"]]

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
          
          accordion_panel("1: Select Table",
                          selectInput("selected_table", "Table:",
                                         c("All", as.character(unique(income_dist_df$group_name))),
                                         selected = "All")
          ),
          
          accordion_panel("2: Where players live"),
          
          accordion_panel("3: Player spending",
                          
            checkboxGroupInput("cost_type", "Cost_Types:",
                               choices = c("All", EXPENSE_BARCOLS),
                               selected = "All")
          ),
          accordion_panel("4: Selected measures"),
          accordion_panel("5: Flood in gameplay"),
          accordion_panel("6: Damage & satisfaction")
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
  
  income_dist_reactive <- reactive({income_dist_df})
  
  selected_table <- reactive({filter_selected_categs(input$selected_table, as.character(unique(income_dist_reactive()$group_name)))})
  
  selected_costtypes <- reactive({filter_selected_categs(input$cost_type, EXPENSE_BARCOLS)})
  

  # Reactive dataset grouped by the chosen color_by variable
  group_col <- reactive({update_group_col(income_dist_reactive(), selected_table())})
  
  
  plot_data <- reactive({
    
    if (identical(group_col(), "player_code")) {
      
      income_dist_reactive() %>% filter(group_name %in% selected_table) %>% droplevels()
      
    } else {
      income_dist_reactive()
    }
  })
  
  income_dist_ave <- reactive({retrieve_average_table(plot_data(), group_col())})
  
  income_dist_n <- reactive({retrieve_n_table(plot_data(), group_col())})
  
  grouped_data <- reactive({income_dist_n() %>% inner_join(income_dist_ave(), by = join_by(across(all_of(group_col()))))})
  
  
  gg_plot <- reactive({get_costs_barplot(income_dist_reactive, income_dist_ave, selected_costtypes, selected_table, game_round = "All", fill_values_all, fill_labels_all)})
  gg_plot1 <- reactive({get_costs_barplot(income_dist_reactive, income_dist_ave, selected_costtypes, selected_table, game_round = "1", fill_values_all, fill_labels_all)})
  gg_plot2 <- reactive({get_costs_barplot(income_dist_reactive, income_dist_ave, selected_costtypes, selected_table, game_round = "2", fill_values_all, fill_labels_all)})
  gg_plot3 <- reactive({get_costs_barplot(income_dist_reactive, income_dist_ave, selected_costtypes, selected_table, game_round = "3", fill_values_all, fill_labels_all)})
  
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
      #cost_type_value <- catname
      
      # Subset summary data for this cost_type and order by x
      sub <- df %>% filter(cost_type == cost_type_value)
      
      # Ensure the same x order
      sub <- sub %>%
        mutate(across(all_of(group_col()), ~ factor(.x, levels = x_order))) %>%
        arrange(.data[[group_col()]])
      
      value_k <- sub$mean_value / 1000
      n_vec   <- sub$n
      
      plt$x$data[[i]] <- create_hovering(plt$x$data[[i]], list(value_k = value_k, n_vec = n_vec))

    }
    
    plt
  })
  
  
  output$plot_r1  <- renderPlot({ gg_plot1() })
  output$plot_r2  <- renderPlot({ gg_plot2() })
  output$plot_r3  <- renderPlot({ gg_plot3() })
  
  
  # Optional: inspect reactive rows
  output$debug <- renderPrint({
    paste(
      paste("Rows:", nrow(income_dist_reactive())),
      paste("Costs:", length(selected_table())),
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