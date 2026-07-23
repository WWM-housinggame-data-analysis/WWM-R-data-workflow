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
process_config_selection <- function(valid_values, default_value, fallback = character(0)) {
  
  # Coerce to character to avoid factor issues
  valid_values <- as.character(valid_values)
  default_value <- as.character(default_value)
  fallback <- as.character(fallback)
  
  # If no valid values yet, return fallback (or stop with a clear message)
  if (length(valid_values) == 0) {
    if (length(fallback) == 1 && nzchar(fallback)) {
      return(fallback)
    } else {
      stop("process_config_selection: No valid_values available and fallback is missing/invalid.")
    }
  }
  
  
  stopifnot("(Only) one choice needs to be provided in default_value" = length(default_value) == 1)
  
  chosen_value <- valid_values[grep(default_value, valid_values)]
  
  
  if (length(chosen_value) == 0) {
    if (length(fallback) == 1 && nzchar(fallback)) {
      chosen_value <- fallback
    } else {
      stop("process_config_selection: default not found and fallback is missing/invalid.")
    }
  }
  
                                               
  stopifnot("(Only) one choice needs to be found in chosen_value" = length(chosen_value) == 1)
  
  return(chosen_value)
}


# -----------------------------------------------
# Handle SELECT_ALL and selected filtering
# -----------------------------------------------

filter_selected_categs <- function(input_categs, req_categs) {
  
    shiny::req(input_categs, req_categs)
    
    # if All is selected OR none selected -> treat as all
    
    if (SELECT_ALL %in% as.vector(input_categs)) {
      
      return(req_categs)
      
    } else {
      
      return(intersect(input_categs, req_categs))
    }
}

update_bar_segments <- function(checked_features) {
  
  checked_features <- filter_selected_categs(checked_features, names(COST_BAR_SEGMENTS))
  
  bar_segs <- COST_BAR_SEGMENTS[names(COST_BAR_SEGMENTS) %in% checked_features]
  
  names(bar_segs) <- names(COST_BAR_SEGMENTS)[names(COST_BAR_SEGMENTS) %in% checked_features]
  
  return(bar_segs)
}

update_table_groups <- function(df, selected_table) {
  
  table_choices <- as.character(unique(df[, TABLE_GROUPCOL]))
  
  selected_table <- filter_selected_categs(selected_table, table_choices)
  
  return(selected_table)
}


# -----------------------------------------------
# Determine grouping column for plot logic
# -----------------------------------------------


update_bar_groupcol <- function(df, selected_table) {
  
  table_choices <- as.character(unique(df[, TABLE_GROUPCOL]))
  
  if (all(table_choices %in% selected_table)) {
    
    groupcol <- INCOME_GRP_COL
    
  } else if (any(table_choices %in% selected_table) && length(selected_table) == 1) {
    
    groupcol <- PLAYER_CODE_COL
    
  } else {
    
    stop("Unexpected number of tables selected. Either all or a single table is expected.")
    
  }
  
  return(groupcol)
}


get_intermediate_rounds <- function(df) {
  
  rounds <- df |>
    dplyr::pull(ROUND_NUMBER_COL) |>
    unique() |>
    sort()
  
  # work on returning no round panels
  if (length(rounds) < length(EXPECTED_INTERM_ROUNDS)) {
    
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
  
  return(round_ids)
}