
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


filter_selected_categs <- function(input_categs, required_categs) {
  
    req(input_categs)
  
    # remove the special label
    req_types <- required_categs
    
    # if All is selected OR none selected -> treat as all
    
    if ("All" %in% as.vector(input_categs)) {
      
      req_types
      
    } else {
      
      intersect(input_categs, req_types)
    }
}

update_group_col <- function(plot_data, selected_table) {
  
  if (all(as.character(unique(plot_data$group_name)) %in% selected_table)) {
    
    group_col <- "income_grp"
    
  } else if (any(as.character(unique(plot_data$group_name)) %in% selected_table) && length(selected_table) == 1) {
    
    group_col <- "player_code"
    
  } else {
    
    stop("Unexpected number of tables selected. Either all or a single table is expected.")
    
  }
  
  return(group_col)
}