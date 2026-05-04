# R/make-data-reactive.R

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






# Handle multi-select with SELECT_ALL semantics (optional helper)
# If user selects SELECT_ALL, you can choose to keep only SELECT_ALL OR expand to all real choices.
normalize_multicheck_selection <- function(selection, choices, all_label = SELECT_ALL, expand_all = FALSE) {
  if (is.null(selection) || length(selection) == 0) return(character(0))
  selection <- intersect(selection, choices)  # sanitize
  if (all_label %in% choices && all_label %in% selection) {
    if (expand_all) {
      # Expand to all (excluding All if you want)
      # return(setdiff(choices, all_label))  # if you want all except SELECT_ALL
      return(choices)                         # include SELECT_ALL too if desired
    } else {
      # Keep only SELECT_ALL
      return(all_label)
    }
  }
  selection
}

mod_multicheck_reset_server <- function(id, default_values, get_choices, all_label = SELECT_ALL, expand_all = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize / update when choices or defaults change
    shiny::observe({
      choices <- get_choices()
      shiny::req(length(choices) > 0)
      
      # default_values is reactive(): can be a single value SELECT_ALL or a vector of types
      sel <- default_values()
      
      # Ensure selection is within choices; fallback to SELECT_ALL if available, else first choice
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


# ------------------------------------------------------------------------------
# Helper: global reset-all-filters observer
# 
# Call this inside your server() AFTER modules have registered their
# session$userData$<id>_reset functions.
# ------------------------------------------------------------------------------

add_global_reset_observer <- function(input, session, reset_button_id = "reset_all_filters") {
  
  shiny::observeEvent(input[[reset_button_id]], {
    
    # Only call reset functions that exist
    if (!is.null(session$userData$gamesession_reset)) session$userData$gamesession_reset()
    if (!is.null(session$userData$table_reset))       session$userData$table_reset()
    if (!is.null(session$userData$cost_types_reset))  session$userData$cost_types_reset()
    
  })
}



# ---- Gamesession reactives helper -------------------------------------------
# Returns a list with:
#   $selected_gamesession  -> reactive() with the selected session name
#   $income_dist_df        -> reactive() with the selected session's income_dist_df

# Add a req() or a safe fallback for the case where income_dist_df() doesn’t yet contain group_names.
#Why this helps: You’ll never send character(0) to process_config_selection() or the module. The module also won’t try to update until choices are non-empty.

make_gamesession_reactives <- function(preprocess_data_list, gamesession_selection, id = "gamesession") {
  force(preprocess_data_list)
  force(gamesession_selection)
  
  # This function must be called inside a server() or moduleServer() context
  # because it uses Shiny reactives and your input module.
  
  # 1) Choices reactive
  gamesession_choices <- shiny::reactive({
    if (identical(gamesession_selection, SELECT_ALL)) {
      names(preprocess_data_list)
    } else {
      # When YAML or config pre-filters the sessions
      gamesession_selection
    }
  })
  
  # 2) Selected gamesession (uses your existing module)
  selected_gamesession <- mod_input_reset_server(
    id = id,
    default_value = shiny::reactive({
      ch <- gamesession_choices()
      # fallback to last choice if available
      if (length(ch) > 0) ch[[length(ch)]] else NULL
    }),
    get_choices = gamesession_choices
  )
  
  # 3) Derived income_dist_df reactive for the selected session
  income_dist_df <- shiny::reactive({
    sess <- selected_gamesession()
    shiny::req(!is.null(sess), sess %in% names(preprocess_data_list))
    # Guard against missing table
    tbls <- preprocess_data_list[[sess]]
    if (is.null(tbls) || is.null(tbls[["income_dist_df"]])) {
      # Return an empty tibble to avoid errors downstream
      return(tibble::tibble())
    }
    tbls[["income_dist_df"]]
  })
  
  # Return both reactives
  list(
    selected_gamesession = selected_gamesession,
    income_dist_df       = income_dist_df
  )
}


# ------------------------------------------------------------------------------
# Helper: Create role_selection(), table_choices(), and selected_table()
# ------------------------------------------------------------------------------

make_role_table_reactives <- function(income_dist_df,
                                      selected_username = SELECTED_USERNAME,
                                      id = "table") {
  
  # -- role_selection ----------------------------------------------------------
  role_selection <- shiny::reactive({
    df <- income_dist_df()
    groups <- character(0)
    
    if (!is.null(df) && nrow(df) > 0 && TABLE_GROUPCOL %in% names(df)) {
      groups <- as.character(unique(df[, TABLE_GROUPCOL]))
    }
    
    process_config_selection(groups, selected_username, fallback = SELECT_ALL)
  })
  
  
  # -- table_choices -----------------------------------------------------------
  table_choices <- shiny::reactive({
    df <- income_dist_df()
    
    # Guard: return SELECT_ALL if no usable data
    if (is.null(df) || nrow(df) == 0 || TABLE_GROUPCOL %in% names(df) == FALSE) {
      return(SELECT_ALL)
    }
    
    # If user did not fix a particular role in YAML…
    if (identical(role_selection(), SELECT_ALL)) {
      c(SELECT_ALL, as.character(unique(df[, TABLE_GROUPCOL])))
    } else {
      # If YAML specified a single role → lock the choices to that
      role_selection()
    }
  })
  
  
  # -- selected_table via your existing module ---------------------------------
  selected_table <- mod_input_reset_server(
    id = id,
    default_value = role_selection,   # reactive
    get_choices   = table_choices     # reactive
  )
  
  
  # -- RETURN ------------------------------------------------------------------
  list(
    role_selection  = role_selection,
    table_choices   = table_choices,
    selected_table  = selected_table
  )
}

make_cost_types_reactive <- function(id = "cost_types") {
  
  # --- Cost Types (checkbox) ---
  
  # Choices reactive (can be dynamic if needed)
  cost_types_choices <- shiny::reactive({
    c(SELECT_ALL, names(COST_BAR_SEGMENTS))
  })
  
  # Default selection reactive (from config or static)
  # If you have a YAML default like CONFIG$defaults$cost_types, wire it here.
  # Otherwise, default to SELECT_ALL.
  cost_types_default <- shiny::reactive({
    SELECT_ALL
    # or CONFIG$defaults$cost_types
  })
  
  selected_cost_types <- mod_multicheck_reset_server(
    id            = "cost_types",
    default_values = cost_types_default,   # reactive() returning a vector (e.g., SELECT_ALL or c("Mortgage payment", ...))
    get_choices    = cost_types_choices,
    all_label      = SELECT_ALL,
    expand_all     = FALSE                  # keep only SELECT_ALL when All is selected (set TRUE to expand to all)
  )
  
  return(selected_cost_types)
}

# ------------------------------------------------------------------------------
# Module 1: derive round_ids + round_labels from income_dist_df()
# ------------------------------------------------------------------------------

make_rounds_reactive <- function(df) {
  
  shiny::reactive({
    
    df <- df()
    
    shiny::req(nrow(df) > 0)
    
    rounds <- df |>
      dplyr::pull(ROUND_NUMBER_COL) |>
      unique() |>
      sort()
    
    # work on returning no round panels
    if (length(rounds) <= 2) {
      
      warning(
        "No intermediate rounds found",
        "Proceeding with detected rounds."
      )
      
      round_ids <- SELECT_ALL
      names(round_ids) <- ROUND_ACCORDION_LABELALL
      
    } else {
      
      interm_rounds <- as.character(rounds[2:(length(rounds)-1)])
      
      # Optional check against expected intermediate rounds
      if (exists("INTERM_ROUNDS", inherits = TRUE) &&
          !identical(interm_rounds, INTERM_ROUNDS)) {
        warning(
          "Detected intermediate rounds differ from INTERM_ROUNDS. ",
          "Proceeding with detected rounds."
        )
      }
      
      # IDs used internally (All + r1, r2, ...)
      round_ids <- c(SELECT_ALL,
                     paste0(ROUND_ACCORDION_IDPREF, interm_rounds)
      )
      
      names(round_ids) <- c(ROUND_ACCORDION_LABELALL,
                            paste(names(ROUND_ACCORDION_IDPREF), interm_rounds))
    }
    
    round_ids
  })
}