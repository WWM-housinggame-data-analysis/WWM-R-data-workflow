library(shiny)
library(bslib)
library(ggplot2)
data(penguins, package = "palmerpenguins")

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
            varSelectInput(
              "color_by", "Color by",
              penguins[c("species", "island", "sex")],
              selected = "species"
            ),
            varSelectInput(
              "table_by", "Table",
              penguins[c("species", "island", "sex")],
              selected = "species"
            ),
            checkboxGroupInput("player", "Player:",
                               choices = c("All", as.character(unique(income_dist_formatted$player_id))),
                               selected = "All"),
            checkboxGroupInput("cost_type", "Cost_Types:",
                               choices = c("All", bar_expenses_cols),
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
                        tabPanel("Plot", plotOutput("plot_all")),
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

# Reactive plot based on user input
get_costs_barplot <- function(input_data, stacked_vars, xlabels) {
  
  costs_barplot <- reactive({
    
    ggplot(input_data) +
      
      geom_bar(data = ~ .x |>
                 dplyr::filter(Cost_Type %in% stacked_vars) |>
                 dplyr::mutate(Cost_Type = forcats::fct_relevel(Cost_Type, stacked_vars)),
               aes(x = round_income, y = Cost_Value, fill = Cost_Type),
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
        labels = xlabels
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_markdown(angle = 0, hjust = 0.5) ##takes rich html
      )
  })
  
  return(costs_barplot)
  
  }
  
server <- function(input, output) {
  
  # Reactive dataset grouped by the chosen color_by variable
  grouped_data <- reactive({
    income_dist_formatted |>
      dplyr::group_by(.data[[input$color_by]]) |>
      dplyr::summarise(
        count = dplyr::n(),
        mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
        mean_bill_depth  = mean(bill_depth_mm, na.rm = TRUE),
        mean_body_mass   = mean(body_mass_g, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # if ("All" %in% reactive({as.vector(input$player)}) == False) {
  #   
  #   income_dist_filtered <- reactive({
  #     income_dist_formatted |> 
  #       filter( player_id %in% as.vector(input$player))
  #     
  #   })
  # } else {
  #   income_dist_filtered <- income_dist_formatted
  # }
  # 
  # if ("All" %in% reactive({as.vector(input$cost_type)}) == False) {
  #   
  #   income_dist_filtered <- reactive({
  #     income_dist_filtered |> 
  #       filter( Cost_Type %in% as.vector(input$cost_type))
  #     
  #   })
  # }
    
  gg_plot <- get_costs_barplot(income_dist_formatted, bar_expenses_cols, income_dist_x$round_income)
  gg_plot1 <- get_costs_barplot(income_dist_formatted |> dplyr::filter(groupround_round_number %in% 1), bar_expenses_cols, income_dist_x$round_income)
  gg_plot2 <- get_costs_barplot(income_dist_formatted |> dplyr::filter(groupround_round_number %in% 2), bar_expenses_cols, income_dist_x$round_income)
  gg_plot3 <- get_costs_barplot(income_dist_formatted |> dplyr::filter(groupround_round_number %in% 3), bar_expenses_cols, income_dist_x$round_income)
  
  # Connect plots
  output$plot_all <- renderPlot({ gg_plot() })
  output$plot_r1  <- renderPlot({ gg_plot1() })
  output$plot_r2  <- renderPlot({ gg_plot2() })
  output$plot_r3  <- renderPlot({ gg_plot3() })
  
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