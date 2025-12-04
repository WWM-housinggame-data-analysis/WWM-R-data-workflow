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
            checkboxGroupInput("Player_by", "Player:",
                               choices = c("All", "Player 1", "Player 2"),
                               selected = "All"),
            checkboxGroupInput("var_by", "Variable:",
                               choices = c("All", "var 1", "var 2"),
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

server <- function(input, output) {
  
  # Reactive plot based on user input
  gg_plot <- reactive({
    ggplot() +
      
      geom_bar(data = bars_expenses_formatted,
               aes(x = Index, y = Value, fill = Type),
               stat = "identity",
               position = "stack",
               width = w) +
      
      scale_fill_manual(
        name = "Round costs",
        values = c(
          "ave_debt" = "black",
          "ave_satisfaction" = "#dfaba3",
          "ave_measures" = "white",
          "ave_profit_minus_spent_savings_house_moving" =  "#a3a3a3",
          "ave_mortgage" = "#cccccc",
          "ave_taxes" = "#dddddd",
          "ave_fluvial_damage" = "#79A2C5",
          "ave_pluvial_damage" = "#79BCC5"),
        labels = c(
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
        fill = guide_legend(title = "Round costs")
      ) +
      
      #Y-axis formatting
      scale_y_continuous(
        labels = function(y) y / 1000,
        name = "Game Currency (k)"
      ) +
      
      scale_x_continuous(
        name = "Round income (k) \n Players per class",
        breaks = c(1, 2, 3, 4, 5, 6),
        labels = income_dist_x$round_income) +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_markdown(angle = 0, hjust = 0.5), ##takes rich html
      )
  })
  
  # Reactive dataset grouped by the chosen color_by variable
  grouped_data <- reactive({
    penguins |>
      dplyr::group_by(.data[[input$color_by]]) |>
      dplyr::summarise(
        count = dplyr::n(),
        mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
        mean_bill_depth  = mean(bill_depth_mm, na.rm = TRUE),
        mean_body_mass   = mean(body_mass_g, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Connect plots
  output$plot_all <- renderPlot({ gg_plot() })
  output$plot_r1  <- renderPlot({ gg_plot() })
  output$plot_r2  <- renderPlot({ gg_plot() })
  output$plot_r3  <- renderPlot({ gg_plot() })
  
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