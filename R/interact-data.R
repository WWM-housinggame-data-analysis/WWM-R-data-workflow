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