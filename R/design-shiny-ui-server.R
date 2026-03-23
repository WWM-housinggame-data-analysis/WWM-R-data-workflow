
# R/design-shiny-ui-server.R

# ============================
# Single-select + Reset module
# ============================


mod_input_reset_ui <- function(id, label) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::selectInput(ns("input_value"), label, choices = NULL),
    shiny::actionButton(ns("reset"), "Reset", class = "btn-outline-secondary btn-sm mt-3")
  )
}

# Update your mod_input_reset_server() so it skips UI updates until there are actual choices, and ensures the selected value is in those choices
# Why this helps: Even if get_choices() briefly returns character(0) during app start, the module won’t try to update the UI and won’t trigger process_config_selection() with empty valid_values.

mod_input_reset_server <- function(id, default_value, get_choices) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize UI with default + initial choices
    shiny::observe({
      
      choices <- get_choices()
      # Skip updating until choices are available
      shiny::req(length(choices) > 0)
      
      # Ensure selected default is inside choices; fallback to first
      sel <- default_value()
      if (is.null(sel) || length(sel) != 1 || !(sel %in% choices)) {
        sel <- choices[[1]]
      }
      
      shiny::updateSelectInput(session, "input_value",
                        choices = choices,
                        selected = sel)
    })
    
    
    # Reset button returns to default (if present), else first option
    shiny::observeEvent(input$reset, {
      
      choices <- get_choices()
      shiny::req(length(choices) > 0)
      
      sel <- default_value()
      if (is.null(sel) || length(sel) != 1 || !(sel %in% choices)) {
        sel <- choices[[1]]
      }
      
      shiny::updateSelectInput(session, "input_value",
                        selected = sel)
    })
    
    
    # Expose a reset function via session$userData
    session$userData[[paste0(id, "_reset")]] <- function() {
      shiny::updateSelectInput(session, "input_value", selected = default_value())
    }
    
    
    # return reactive value
    return(shiny::reactive(input$input_value))
  })
}



# Reusable accordion panel for a game round (or "All")
make_round_panel <- function(round_id, label) {
  
  # Build output IDs dynamically
  plot_id    <- paste0("plot_",  round_id)
  summary_id <- paste0("summary_", round_id)
  table_id   <- paste0("table_",   round_id)
  
  bslib::accordion_panel(
    title = label,
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Plot",    plotly::plotlyOutput(plot_id)),
      shiny::tabPanel("Summary", shiny::verbatimTextOutput(summary_id)),
      shiny::tabPanel("Table",   shiny::tableOutput(table_id))
    )
  )
}



# ==================================
# Multi-select (checkboxGroupInput) + Reset module ====
# ==================================

mod_multicheck_reset_ui <- function(id, label) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxGroupInput(
      ns("input_values"),
      label = label,
      choices = NULL
    ),
    shiny::actionButton(ns("reset"), "Reset", class = "btn-outline-secondary btn-sm mt-3")
  )
}

# Handle multi-select with "All" semantics (optional helper)
# If user selects "All", you can choose to keep only "All" OR expand to all real choices.
normalize_multicheck_selection <- function(selection, choices, all_label = "All", expand_all = FALSE) {
  if (is.null(selection) || length(selection) == 0) return(character(0))
  selection <- intersect(selection, choices)  # sanitize
  if (all_label %in% choices && all_label %in% selection) {
    if (expand_all) {
      # Expand to all (excluding All if you want)
      # return(setdiff(choices, all_label))  # if you want all except "All"
      return(choices)                         # include "All" too if desired
    } else {
      # Keep only "All"
      return(all_label)
    }
  }
  selection
}

mod_multicheck_reset_server <- function(id, default_values, get_choices, all_label = "All", expand_all = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize / update when choices or defaults change
    shiny::observe({
      choices <- get_choices()
      shiny::req(length(choices) > 0)
      
      # default_values is reactive(): can be a single value "All" or a vector of types
      sel <- default_values()
      
      # Ensure selection is within choices; fallback to "All" if available, else first choice
      if (is.null(sel) || length(intersect(sel, choices)) == 0) {
        sel <- if (all_label %in% choices) all_label else choices[[1]]
      }
      
      shiny::updateCheckboxGroupInput(session, "input_values",
                               choices = choices,
                               selected = sel)
    })
    
    # Reset button -> back to defaults, normalized
    shiny::observeEvent(input$reset, {
      choices <- get_choices()
      shiny::req(length(choices) > 0)
      
      sel <- default_values()
      if (is.null(sel) || length(intersect(sel, choices)) == 0) {
        sel <- if (all_label %in% choices) all_label else choices[[1]]
      }
      
      shiny::updateCheckboxGroupInput(session, "input_values", selected = sel)
    })
    
    
    # Inside mod_multicheck_reset_server after the observeEvent for reset:
    session$userData[[paste0(id, "_reset")]] <- function() {
      choices <- get_choices()
      shiny::req(length(choices) > 0)
      sel <- default_values()
      if (is.null(sel) || length(intersect(sel, choices)) == 0) {
        sel <- if (all_label %in% choices) all_label else choices[[1]]
      }
      shiny::updateCheckboxGroupInput(session, "input_values", selected = sel)
    }
    
    
    # Returned reactive selection, normalized (enforce All semantics consistently)
    return(shiny::reactive({
      choices <- get_choices()
      normalize_multicheck_selection(input$input_values, choices, all_label = all_label, expand_all = expand_all)
    }))
  })
}