filter_selected_categs <- function(input_categs, required_categs) {
  
    req(input_categs)
    # remove the special label
    req_types <- as.character(unique(income_dist_reactive()$player_code))
    # if All is selected OR none selected -> treat as all
    if ("All" %in% as.vector(input_categs)) {
      req_types
    } else {
      intersect(input_categs, req_types)
    }
}