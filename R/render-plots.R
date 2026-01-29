trim_colname <- function(nm) {
  
  # Remove leading "(" and trailing ")"
  nm <- gsub("^\\(|\\)$", "", nm)
  
  # Remove trailing ", 1" (or ",1") if present
  nm <- sub(",\\s*\\d+$", "", nm)
  
  return(nm)
}

structure_legend <- function(stacked_column_list, line1_legendname, bars_legendname) {
  
  if (stacked_column_list$type == "scatter") {
    
    stacked_column_list$legendgroup <- "line1"
    
    stacked_column_list$legendgrouptitle <- list(text = line1_legendname)
    
    stacked_column_list$legendrank <- 1
    
  } else if (stacked_column_list$type == "bar") {
    
    stacked_column_list$legendgroup <- "costs"
    
    stacked_column_list$legendgrouptitle <- list(text = bars_legendname)
    
    stacked_column_list$legendrank <- 2
  }
  
  return(stacked_column_list)
}


convert2plotly_legend <- function(plt) {
  
  seen_colnames <- character()
  
  for (i in seq_along(plt$x$data)) {
    
    stacked_column_list <- plt$x$data[[i]]
    
    stacked_colname <- stacked_column_list$name %||% ""
    
    stacked_column_list$name <- trim_colname(stacked_colname)
    
    stacked_column_list <- structure_legend(stacked_column_list, line1_legendname, bars_legendname)
    
    # show only one legend entry per name
    if (stacked_colname %in% seen_colnames) {
      
      stacked_column_list$showlegend <- FALSE
      
    } else {
      
      stacked_column_list$showlegend <- TRUE
      
      seen_colnames <- c(seen_colnames, stacked_colname)
      
    }
    
    plt$x$data[[i]] <- stacked_column_list
  }
  
  return(plt)
}


render_plots <- function(obj) {
  
  # obj is list(plot, data)
  gp  <- obj$plot
  df  <- obj$data          # summary_df with mean_value & n
  stacked_vec <- obj$barfill
  
  plt <- ggplotly(gp)
  
  
  output$debug <- renderPrint({
    unique(vapply(plt$x$data, function(tr) tr$name %||% "", character(1)))
  })
  
  plt <- convert2plotly_legend(plt, "Round Spendable Income", "Round costs")
  
  plt <- layout(plt, hovermode = "closest")
  
  # We need per-trace (cost_type) vectors of value_k and n in the same order as trace points.
  # Plotly creates one trace per cost_type.
  # For each trace name (fullData.name), subset df and order by the x (round_income) factor
  # to match bar positions.
  
  # # Get x positions order as they appear in the first trace
  # x_order <- plt$x$data[[1]]$x
  
  
  # find first BAR trace for x order (safer than [[1]])
  bar_idx <- which(vapply(plt$x$data, function(tr) tr$type %||% "", character(1)) == "bar")[1]
  x_order <- plt$x$data[[bar_idx]]$x
  
  
  # reverse mapping label -> cost_type (if you used fill_labels_all)
  rev_map <- setNames(names(fill_labels_all[stacked_vec]), fill_labels_all[stacked_vec])
  
  
  
  for (i in seq_along(plt$x$data)) {
    tr      <- plt$x$data[[i]]
    # catname <- tr$name                 # equals legend label (fill_labels_all)
    # xs      <- tr$x                    # x values for this trace
    
    
    # Only add cost hover to bar traces
    if ((tr$type %||% "") != "bar") next
    
    catname <- tr$name %||% ""
    
    
    # Map legend label back to cost_type value. If you used labels, we need a reverse map:
    # build it once outside and keep it around; for demo we rebuild quickly:
    # Suppose you still have 'stacked_vec' and 'fill_labels_all' in scope. If not, create a reverse map:
    # rev_map <- setNames(names(fill_labels_all[stacked_vec]), fill_labels_all[stacked_vec])
    
    # If catname equals the label, translate to original cost_type:
    # cost_type_value <- rev_map[catname]
    # If you didn't customize labels, catname is directly the cost_type.
    
    # If using labels, do:
    cost_type_value <- if (!is.na(rev_map[catname])) rev_map[catname] else catname
    
    # For simplicity here, assume catname == cost_type (no label remap). If you used labels,
    # add the reverse mapping shown above.
    #cost_type_value <- catname
    
    # # Subset summary data for this cost_type and order by x
    # sub <- df %>% filter(cost_type == cost_type_value)
    # 
    # # Ensure the same x order
    # sub <- sub %>%
    #   mutate(across(all_of(group_col()), ~ factor(.x, levels = x_order))) %>%
    #   arrange(.data[[group_col()]])
    
    
    sub <- df %>%
      filter(cost_type == cost_type_value) %>%
      mutate(xlabels = factor(xlabels, levels = x_order)) %>%
      arrange(xlabels)
    
    
    value_k <- sub$mean_value / 1000
    n_vec   <- sub$n
    
    plt$x$data[[i]] <- create_hovering(plt$x$data[[i]], list(value_k = value_k, n_vec = n_vec))
    
  }
  
  plt
}       

