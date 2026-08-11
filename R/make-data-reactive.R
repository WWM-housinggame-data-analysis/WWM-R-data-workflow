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


# Update your mod_input_reset_server() so it skips UI updates until there are actual options, and ensures the selected value is in those options
# Why this helps: Even if get_options() briefly returns character(0) during app start, the module won’t try to update the UI and won’t trigger process_dashboard_choice() with empty valid_values.

mod_input_reset_server <- function(id, choice, get_options) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize UI with default + initial options
    shiny::observe({
      
      options <- get_options()
      # Skip updating until options are available
      shiny::req(length(options) > 0)
      
      # Ensure selected default is inside options; fallback to first
      ch <- choice()
      if (is.null(ch) || length(ch) != 1 || !(ch %in% options)) {
        ch <- options[[1]]
      }
      
      shiny::updateSelectInput(session, "input_value",
                               options = options,
                               selected = ch)
    })
    
    
    # Reset button returns to default (if present), else first option
    shiny::observeEvent(input$reset, {
      
      options <- get_options()
      shiny::req(length(options) > 0)
      
      ch <- choice()
      if (is.null(ch) || length(ch) != 1 || !(ch %in% options)) {
        ch <- options[[1]]
      }
      
      shiny::updateSelectInput(session, "input_value",
                               selected = ch)
    })
    
    
    # Expose a reset function via session$userData
    session$userData[[paste0(id, "_reset")]] <- function() {
      shiny::updateSelectInput(session, "input_value", selected = choice())
    }
    
    
    # return reactive value
    return(shiny::reactive(input$input_value))
  })
}






# Handle multi-select with SELECT_ALL semantics (optional helper)
# If user selects SELECT_ALL, you can choose to keep only SELECT_ALL OR expand to all real options.
normalize_multicheck_selection <- function(input_values, options, all_label = SELECT_ALL, expand_all = FALSE) {
  
  if (is.null(options) || length(options) == 0)
    return(character(0))
  
  if (is.null(input_values) || length(input_values) == 0) {
    
    if (all_label %in% options)
      return(all_label)
    
    return(options[1])
  }
  
  input_values <- intersect(input_values, options)  # sanitize
  
  if (all_label %in% options && all_label %in% input_values) {
    if (expand_all) {
      # Expand to all (excluding All if you want)
      # return(setdiff(options, all_label))  # if you want all except SELECT_ALL
      return(options)                         # include SELECT_ALL too if desired
    } else {
      # Keep only SELECT_ALL
      return(all_label)
    }
  }

  input_values
}

mod_multicheck_reset_server <- function(id, choice, get_options, all_label = SELECT_ALL, expand_all = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize / update when options or defaults change
    shiny::observe({
      options <- get_options()
      shiny::req(length(options) > 0)
      
      # choice is reactive(): can be a single value SELECT_ALL or a vector of types
      ch <- choice()
      
      # Ensure options is within options; fallback to SELECT_ALL if available, else first choice
      if (is.null(ch) || length(intersect(ch, options)) == 0) {
        ch <- if (all_label %in% options) all_label else options[[1]]
      }
      
      shiny::updateCheckboxGroupInput(session, "input_values",
                                      options = options,
                                      selected = ch)
    })
    
    # Reset button -> back to defaults, normalized
    shiny::observeEvent(input$reset, {
      options <- get_options()
      shiny::req(length(options) > 0)
      
      ch <- choice()
      if (is.null(ch) || length(intersect(ch, options)) == 0) {
        ch <- if (all_label %in% options) all_label else options[[1]]
      }
      
      shiny::updateCheckboxGroupInput(session, "input_values", selected = ch)
    })
    
    
    # Inside mod_multicheck_reset_server after the observeEvent for reset:
    session$userData[[paste0(id, "_reset")]] <- function() {
      options <- get_options()
      shiny::req(length(options) > 0)
      ch <- choice()
      if (is.null(ch) || length(intersect(ch, options)) == 0) {
        ch <- if (all_label %in% options) all_label else options[[1]]
      }
      shiny::updateCheckboxGroupInput(session, "input_values", selected = ch)
    }
    
    
    # Returned reactive options, normalized (enforce All semantics consistently)
    shiny::reactive({
      
      options <- get_options()
      
      normalize_multicheck_selection(
        input$input_values,
        options,
        all_label = all_label,
        expand_all = expand_all
      )
      
    })
    
  }
  )
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
#Why this helps: You’ll never send character(0) to process_dashboard_choice() or the module. The module also won’t try to update until options are non-empty.

make_question_reactives <- function(dashboard_data_list, question_options, id = "question") {
  force(dashboard_data_list)
  force(question_options)
  
  # This function must be called inside a server() or moduleServer() context
  # because it uses Shiny reactives and your input module.
  
  # 1) options reactive
  question_options_reactive <- shiny::reactive({question_options})
  
  # 2) Selected gamesession (uses your existing module)
  selected_question_reactive <- mod_input_reset_server(
    id = id,
    choice = shiny::reactive({
      ch <- question_options_reactive()
      # fallback to last choice if available
      if (length(ch) > 0) ch[[length(ch)]] else NULL
    }),
    get_options = question_options_reactive
  )
  
  # 3) Derived income_dist_df reactive for the selected session
  selected_question_list <- shiny::reactive({
    quest <- selected_question_reactive()
    shiny::req(!is.null(quest), quest %in% names(dashboard_data_list))
    # Guard against missing table
    tbl_list <- dashboard_data_list[[quest]]
    if (is.null(tbl_list)) {
      # Return an empty tibble to avoid errors downstream
      return(list(tibble::tibble()))
    }
    tbl_list
  })
  
  # Return both reactives
  list(
    selected_question = selected_question_reactive,
    selected_question_list  = selected_question_list
  )
}



# ---- Gamesession reactives helper -------------------------------------------
# Returns a list with:
#   $selected_gamesession  -> reactive() with the selected session name
#   $income_dist_df        -> reactive() with the selected session's income_dist_df

# Add a req() or a safe fallback for the case where income_dist_df() doesn’t yet contain group_names.
#Why this helps: You’ll never send character(0) to process_dashboard_choice() or the module. The module also won’t try to update until options are non-empty.

make_gamesession_reactives <- function(session_data_list, gamesession_options, id = "gamesession") {
  
  force(session_data_list)
  force(gamesession_options)
  
  
  
  # This function must be called inside a server() or moduleServer() context
  # because it uses Shiny reactives and your input module.
  
  # 1) options reactive
  gamesession_options_reactive <- shiny::reactive({gamesession_options})
  
  # 2) Selected gamesession (uses your existing module)
  selected_gamesession_reactive <- mod_input_reset_server(
    id = id,
    choice = shiny::reactive({
      ch <- gamesession_options_reactive()
      # fallback to last choice if available
      if (length(ch) > 0) ch[[length(ch)]] else NULL
    }),
    get_options = gamesession_options_reactive
  )
  
  # 3) Derived income_dist_df reactive for the selected session
  selected_gamesession_df <- shiny::reactive({
    sess <- selected_gamesession_reactive()
    shiny::req(!is.null(sess), sess %in% names(session_data_list()))
    # Guard against missing table
    tbls <- session_data_list()[[sess]]
    if (is.null(tbls)) {
      # Return an empty tibble to avoid errors downstream
      return(tibble::tibble())
    }
    tbls
  })
  
  # Return both reactives
  list(
    selected_gamesession = selected_gamesession_reactive,
    selected_gamesession_df  = selected_gamesession_df
  )
}


# ------------------------------------------------------------------------------
# Helper: Create role_choice(), table_options(), and selected_table()
# ------------------------------------------------------------------------------

make_role_table_reactives <- function(reactive_df,
                                      selected_username = SELECTED_USERNAME,
                                      id = "table") {
  
  # -- role_choice ----------------------------------------------------------
  role_choice <- shiny::reactive({
    df <- reactive_df()
    groups <- character(0)
    
    if (!is.null(df) && nrow(df) > 0 && TABLE_GROUPCOL %in% names(df)) {
      groups <- as.character(unique(df[, TABLE_GROUPCOL]))
    }
    
    process_dashboard_choice(groups, selected_username, fallback = SELECT_ALL)
  })
  
  
  # -- table_options -----------------------------------------------------------
  table_options <- shiny::reactive({
    df <- reactive_df()
    
    # Guard: return SELECT_ALL if no usable data
    if (is.null(df) || nrow(df) == 0 || TABLE_GROUPCOL %in% names(df) == FALSE) {
      return(SELECT_ALL)
    }
    
    # If user did not fix a particular role in YAML…
    if (identical(role_choice(), SELECT_ALL)) {
      c(SELECT_ALL, as.character(unique(df[, TABLE_GROUPCOL])))
    } else {
      # If YAML specified a single role → lock the options to that
      role_choice()
    }
  })
  
  
  # -- selected_table via your existing module ---------------------------------
  selected_table <- mod_input_reset_server(
    id = id,
    choice = role_choice,   # reactive
    get_options   = table_options     # reactive
  )
  
  
  # -- RETURN ------------------------------------------------------------------
  list(
    role_choice     = role_choice,
    table_options   = table_options,
    selected_table  = selected_table
  )
}

make_cost_types_reactive <- function(id = "cost_types") {
  
  # --- Cost Types (checkbox) ---
  
  # options reactive (can be dynamic if needed)
  cost_types_options <- shiny::reactive({
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
    choice        = cost_types_default,   # reactive() returning a vector (e.g., SELECT_ALL or c("Mortgage payment", ...))
    get_options   = cost_types_options,
    all_label     = SELECT_ALL,
    expand_all    = FALSE                  # keep only SELECT_ALL when All is selected (set TRUE to expand to all)
  )
  
  return(selected_cost_types)
}

# ------------------------------------------------------------------------------
# Module 1: derive round_ids + round_labels from reactive_df()
# ------------------------------------------------------------------------------

make_rounds_reactive <- function(reactive_df) {
  
  shiny::reactive({
    
    df <- reactive_df()
    
    shiny::req(nrow(df) > 0)
    
    get_round_ids(df)
    
  })
}