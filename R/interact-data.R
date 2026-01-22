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