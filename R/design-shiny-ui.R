# R/design-shiny-ui.R

# ---------------------------------------------------------------
# Set defaults ----
# ---------------------------------------------------------------

## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))


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

# Reusable accordion panel for a game round (or SELECT_ALL)
make_round_panels <- function(round_ids, plot_height) {
  
  shiny::req(length(round_ids) > 0)
  
  # ---- build panels ----
  panels <- lapply(unname(round_ids), function(rid) {
    
    label <- names(round_ids)[round_ids == rid]
    
    # Build output IDs dynamically
    plot_id    <- paste0("plot_",  rid)
    summary_id <- paste0("summary_", rid)
    table_id   <- paste0("table_",   rid)
    
    bslib::accordion_panel(
      title = label,
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel("Plot",    plotly::plotlyOutput(plot_id,
                                                        height = plot_height,
                                                        width = "100%")),
        shiny::tabPanel("Summary", shiny::verbatimTextOutput(summary_id)),
        shiny::tabPanel("Table",   shiny::tableOutput(table_id))
      )
    )
  })
  
  # ---- return accordion ----
  do.call(bslib::accordion,
          c(
            list(open = DEFAULT_OPEN_ACCORDIONS),
            panels
          )
  )
  
}