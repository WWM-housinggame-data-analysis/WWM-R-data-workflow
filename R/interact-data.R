#R/interact-data.R

## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))

# -----------------------------------------------
# Robust selection of configuration defaults
# -----------------------------------------------


#Right now it stops when valid_values is empty. For robustness, allow empty and return the fallback (with a warning). Also, ensure fallback is length 1 and not empty.
# Why this helps: During initial reactivity when data hasn’t arrived, you’ll get a sane fallback instead of a hard error.
process_dashboard_choice <- function(valid_values, choice, fallback = character(0), return_choice = TRUE) {
  
  # Coerce to character to avoid factor issues
  valid_values <- as.character(valid_values)
  choice <- as.character(choice)
  fallback <- as.character(fallback)
  
  # If no valid values yet, return fallback (or stop with a clear message)
  if (length(valid_values) == 0) {
    if (length(fallback) == 1 && nzchar(fallback)) {
      warning("process_dashboard_choice: No valid_values available. fallback value is returned.")
      return(fallback)
    } else {
      stop("process_dashboard_choice: No valid_values available and fallback is missing/invalid.")
    }
  }
  
  
  stopifnot("(Only) one choice needs to be provided in choice" = length(choice) == 1)
  
  if(identical(SELECT_ALL, choice)){
    if (return_choice) {
      return(choice)
    } else {
      return(valid_values)
    }
    
  }
  
  chosen_value <- valid_values[grep(choice, valid_values)]
  
  if (length(chosen_value) == 0) {
    if (length(fallback) == 1 && nzchar(fallback)) {
      chosen_value <- fallback
      warning("process_dashboard_choice: No choice found in valid_values. fallback value is returned.")
    } else {
      stop("process_dashboard_choice: default not found and fallback is missing/invalid.")
    }
  }
  
                                               
  stopifnot("(Only) one choice needs to be found in chosen_value" = length(chosen_value) == 1)
  
  return(chosen_value)
}


# -----------------------------------------------
# Handle SELECT_ALL and selected filtering
# -----------------------------------------------

translate_selected_categs <- function(input_categs, req_categs) {
  
    shiny::req(input_categs, req_categs)
    
    # if All is selected OR none selected -> treat as all
    
    if (SELECT_ALL %in% as.vector(input_categs)) {
      
      return(req_categs)
      
    } else {
      
      return(intersect(input_categs, req_categs))
    }
}

update_selected_features <- function(checked_features, available_features) {
  
  checked_features <- translate_selected_categs(checked_features, names(available_features))
  
  selected_features <- available_features[names(available_features) %in% checked_features]
  
  names(selected_features) <- names(available_features)[names(available_features) %in% checked_features]
  
  return(selected_features)
}

translate_table_selection <- function(df, selected_table) {
  
  table_options <- as.character(unique(df[, TABLE_GROUPCOL]))
  
  selected_table <- translate_selected_categs(selected_table, table_options)
  
  return(selected_table)
}


# -----------------------------------------------
# Determine grouping column for plot logic
# -----------------------------------------------


update_grouping_choice <- function(df, selected_table) {
  
  table_options <- as.character(unique(df[, TABLE_GROUPCOL]))
  
  if (all(table_options %in% selected_table)) {
    
    groupcol <- INCOME_GRP_COL
    
  } else if (any(table_options %in% selected_table) && length(selected_table) == 1) {
    
    groupcol <- PLAYER_CODE_COL
    
  } else {
    
    stop("Unexpected number of tables selected. Either all or a single table is expected.")
    
  }
  
  return(groupcol)
}


get_round_ids <- function(df, show_final_round = FALSE) {
  
  rounds <- df |>
    dplyr::filter(.data[[ROUND_NUMBER_COL]] %in% EXPECTED_ROUNDS[1] == FALSE) |>
    dplyr::pull(.data[[ROUND_NUMBER_COL]]) |>
    unique() |>
    sort()
  
  # work on returning no round panels
  if (length(rounds) == 1) {
    
    warning(
      "No intermediate rounds found",
      "Proceeding with detected rounds."
    )
    
    round_ids <- SELECT_ALL
    names(round_ids) <- ROUND_ACCORDION_LABELALL
    
  } else {
    
    indiv_rounds <- as.character(rounds)
    
    if (show_final_round) {
      
      # Optional check against expected completed rounds
      if (!identical(indiv_rounds, EXPECTED_COMPLET_ROUNDS)) {
        warning(
          "Detected completed rounds differ from EXPECTED_COMPLET_ROUNDS. ",
          "Proceeding with detected rounds."
        )
      }
      
    } else {
      
      indiv_rounds <- indiv_rounds[1 : (length(indiv_rounds) - 1)]
      
      # Optional check against expected intermediate rounds
      if (exists("EXPECTED_INTERM_ROUNDS", inherits = TRUE) &&
          !identical(indiv_rounds, EXPECTED_INTERM_ROUNDS)) {
        warning(
          "Detected intermediate rounds differ from EXPECTED_INTERM_ROUNDS. ",
          "Proceeding with detected rounds."
        )
      }
    }
    
    # IDs used internally (All + r1, r2, ...)
    round_ids <- c(SELECT_ALL,
                   paste0(ROUND_ACCORDION_IDPREF, indiv_rounds)
    )
    
    names(round_ids) <- c(ROUND_ACCORDION_LABELALL,
                          paste(names(ROUND_ACCORDION_IDPREF), indiv_rounds))
  }
  
  return(round_ids)
}