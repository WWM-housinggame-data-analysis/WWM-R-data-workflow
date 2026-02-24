mod_input_reset_ui <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("input_value"), label, choices = NULL),
    actionButton(ns("reset"), "Reset", class = "btn-outline-secondary btn-sm mt-3")
  )
}

# Update your mod_input_reset_server() so it skips UI updates until there are actual choices, and ensures the selected value is in those choices
# Why this helps: Even if get_choices() briefly returns character(0) during app start, the module won’t try to update the UI and won’t trigger process_config_selection() with empty valid_values.

mod_input_reset_server <- function(id, default_value, get_choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize UI with default + initial choices
    observe({
      
      choices <- get_choices()
      # Skip updating until choices are available
      req(length(choices) > 0)
      
      # Ensure selected default is inside choices; fallback to first
      sel <- default_value()
      if (is.null(sel) || length(sel) != 1 || !(sel %in% choices)) {
        sel <- choices[[1]]
      }
      
      updateSelectInput(session, "input_value",
                        choices = choices,
                        selected = sel)
    })
    
    
    # Reset button returns to default (if present), else first option
    observeEvent(input$reset, {
      
      choices <- get_choices()
      req(length(choices) > 0)
      
      sel <- default_value()
      if (is.null(sel) || length(sel) != 1 || !(sel %in% choices)) {
        sel <- choices[[1]]
      }
      
      updateSelectInput(session, "input_value",
                        selected = sel)
    })
    
    
    # Expose a reset function via session$userData
    session$userData[[paste0(id, "_reset")]] <- function() {
      updateSelectInput(session, "input_value", selected = default_value())
    }
    
    
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
