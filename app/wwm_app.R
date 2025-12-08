library(shiny)
library(bslib)
library(ggplot2)
library(ggtext)
library(plotly)
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

# Reactive plot based on user input
get_costs_barplot <- function(input_data_reactive, stacked_vars_reactive, selected_players_reactive) {
  
  reactive({
    
    # Pull the latest data and selection from the reactives
    plot_data   <- input_data_reactive()
    stacked_vec <- stacked_vars_reactive()
    selected_players_vec <-selected_players_reactive()
    
    # Guard against empty states
    req(nrow(plot_data) > 0, length(stacked_vec) > 0)
    
    
    # Filter and prepare just before plotting
    plot_data <- plot_data |>
      dplyr::filter( player_id %in% selected_players_vec) |> 
      droplevels() |>
      dplyr::filter(Cost_Type %in% stacked_vec) |>
      droplevels() |>
      dplyr::mutate(
        Cost_Type  = forcats::fct_relevel(Cost_Type, stacked_vec),
        Cost_Value = as.numeric(gsub(",", "", as.character(Cost_Value))) # safe numeric
      )
    
    xlabels <- paste(as.numeric(levels(plot_data$round_income))/1000, "k", sep="")
    
    
    # Central colour/label dictionaries (names must match Cost_Type in data)
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
    
    
          
    
    ggplot(plot_data) +
      
      geom_bar(aes(x = round_income, y = Cost_Value, fill = Cost_Type),
               stat = "summary", fun = "mean", position = "stack",
               na.rm = TRUE, width = w) +
      
      scale_fill_manual(
        name = "Round costs",
        values = fill_values_all[stacked_vec],
        labels = fill_labels_all[stacked_vec]
      ) +
      
      guides(fill = guide_legend(title = "Round costs")) +
      scale_y_continuous(labels = function(y) y / 1000, name = "Game Currency (k)") +
      scale_x_discrete(name = "Round income (k) \n Players per class", labels = xlabels) +
      
      theme_minimal() +
      theme(axis.text.x = element_markdown(angle = 0, hjust = 0.5)) ##takes rich html
  })
  
  }
  
server <- function(input, output) {
  
  income_dist_reactive <- reactive({income_dist_formatted})
  
  selected_players <- reactive({
    req(input$player)
    # remove the special label
    req_types <- as.character(unique(income_dist_formatted$player_id))
    # if All is selected OR none selected -> treat as all
    if ("All" %in% as.vector(input$player)) {
      req_types
    } else {
      intersect(input$player, req_types)
    }
  })
  
  selected_costtypes <- reactive({
    req(input$cost_type)
    # remove the special label
    req_types <- bar_expenses_cols
    # if All is selected OR none selected -> treat as all
    if ("All" %in% as.vector(input$cost_type)) {
      req_types
    } else {
      intersect(input$cost_type, req_types)
    }
  })
  
  # Reactive dataset grouped by the chosen color_by variable
  grouped_data <- reactive({
    income_dist_reactive() |>
      dplyr::group_by(.data[[input$color_by]]) |>
      dplyr::summarise(
        count = dplyr::n(),
        mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
        mean_bill_depth  = mean(bill_depth_mm, na.rm = TRUE),
        mean_body_mass   = mean(body_mass_g, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  gg_plot <- get_costs_barplot(income_dist_reactive, selected_costtypes, selected_players)
  gg_plot1 <- get_costs_barplot(reactive({income_dist_reactive() |> dplyr::filter(groupround_round_number %in% 1)}), selected_costtypes, selected_players)
  gg_plot2 <- get_costs_barplot(reactive({income_dist_reactive() |> dplyr::filter(groupround_round_number %in% 2)}), selected_costtypes, selected_players)
  gg_plot3 <- get_costs_barplot(reactive({income_dist_reactive() |> dplyr::filter(groupround_round_number %in% 3)}), selected_costtypes, selected_players)
  
  # Connect plots
  output$plot_all <- renderPlotly({
    # Build the ggplot first
    gp <- gg_plot()              # this is your reactive() that returns a ggplot
    
    # Convert to plotly
    plt <- ggplotly(gp)          # do not pass tooltip=...; we’ll control via hovertemplate
    
    # 1) Use unified hover → single tooltip box per x (bar)
    plt <- layout(plt, hovermode = "x unified")
    
    # 2) Format each stacked trace’s line in the unified tooltip
    #    - Use the trace name (matches your fill labels) + y value
    #    - Show values in 'k' using customdata (y/1000)
    for (i in seq_along(plt$x$data)) {
      yi <- plt$x$data[[i]]$y
      if (!is.null(yi)) {
        plt$x$data[[i]]$customdata <- yi / 1000  # value in k for hover
        # Bold category name, show value in k, and hide the extra box
        plt$x$data[[i]]$hovertemplate <- paste0(
          "<b>%{fullData.name}</b>: %{customdata:.1f}k<extra></extra>"
        )
      }
    }
    
    plt
  })
  
  output$plot_r1  <- renderPlot({ gg_plot1() })
  output$plot_r2  <- renderPlot({ gg_plot2() })
  output$plot_r3  <- renderPlot({ gg_plot3() })
  
  
  # Optional: inspect reactive rows
  output$debug <- renderPrint({
    paste("Rows:", nrow(income_dist_reactive()))
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