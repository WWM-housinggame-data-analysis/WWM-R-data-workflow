mod_input_reset_ui <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("input_value"), label, choices = NULL),
    actionButton(ns("reset"), "Reset", class = "btn-outline-secondary btn-sm mt-3")
  )
}

mod_input_reset_server <- function(id, default_value, get_choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize UI with default + initial choices
    observe({
      updateSelectInput(session, "input_value",
                        choices = get_choices(),
                        selected = default_value())
    })
    
    # Reset button
    observeEvent(input$reset, {
      updateSelectInput(session, "input_value",
                        selected = default_value())
    })
    
    # return reactive value
    return(reactive(input$input_value))
  })
}


# 
# accordion_panel("2: Select Table",
#                 mod_input_reset_ui("table", "Table")
# )


# selected_gamesession <- mod_input_reset_server(
#   id = "gamesession",
#   default_value = reactive(gamesession_selection),
#   get_choices = reactive(names(preprocess_data_list))
# )
# 
# selected_table <- mod_input_reset_server(
#   id = "table",
#   default_value = reactive(role_selection),
#   get_choices = reactive(c("All", unique(income_dist_df()$group_name)))
# )
