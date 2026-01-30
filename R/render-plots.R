trim_colname <- function(nm) {
  
  nm <- nm %||% ""
  
  # Remove leading "(" and trailing ")"
  nm <- gsub("^\\(|\\)$", "", nm)
  
  # Remove trailing ", 1" (or ",1") if present
  nm <- sub(",\\s*\\d+$", "", nm)
  
  return(nm)
}

structure_legend <- function(plotted_col_metadata, line1_legendname, bars_legendname) {
  
  if (plotted_col_metadata$type == "scatter") {
    
    plotted_col_metadata$legendgroup <- "line1"
    
    plotted_col_metadata$legendgrouptitle <- list(text = line1_legendname)
    
    plotted_col_metadata$legendrank <- 1
    
  } else if (plotted_col_metadata$type == "bar") {
    
    plotted_col_metadata$legendgroup <- "bars"
    
    plotted_col_metadata$legendgrouptitle <- list(text = bars_legendname)
    
    plotted_col_metadata$legendrank <- 2
  }
  
  return(plotted_col_metadata)
}


convert2plotly_legend <- function(plotted_col_metadata, line1_legendname, bars_legendname, seen_colnames) {
  
  plotted_col_metadata$name <- trim_colname(plotted_col_metadata$name)
  
  plotted_col_metadata <- structure_legend(plotted_col_metadata, line1_legendname, bars_legendname)
  
  # show only one legend entry per name
  if (plotted_col_metadata$name %in% seen_colnames) {
    
    plotted_col_metadata$showlegend <- FALSE
    
  } else {
    
    plotted_col_metadata$showlegend <- TRUE
    
    seen_colnames <- c(seen_colnames, plotted_col_metadata$name)
    
  }
  
  return(plotted_col_metadata)
}

retrieve_barplot_xlabels <- function(plot_metadata, plot_type = "bar") {
  
  # find first BAR trace for x order (safer than [[1]])
  barplot_metadata_finder <- vapply(plot_metadata, function(metadata) metadata$type %||% "", character(1)) == plot_type
  
  barplot_xlabels <- plot_metadata[which(barplot_metadata_finder)[1]]$x
  
  return(barplot_xlabels)
}

design_hovering <- function(plotted_col_metadata, hovering_sumstats) {
  
  plotted_col_metadata$customdata <- do.call(cbind, hovering_sumstats)
  
  plotted_col_metadata$hovertemplate <- "<b>%{fullData.name}</b><br>"
  
  if ("value_k" %in% names(hovering_sumstats)) {
    
    plotted_col_metadata$hovertemplate <- paste0(plotted_col_metadata$hovertemplate,
                                                 paste0("Mean: %{customdata[",
                                                        which(names(hovering_sumstats) %in% "value_k") - 1,
                                                        "]:.2f}k<br>"))
  }
  
  if ("n_vec" %in% names(hovering_sumstats)){
    
    plotted_col_metadata$hovertemplate <- paste0(plotted_col_metadata$hovertemplate,
                                                 paste0("N: %{customdata[",
                                                        which(names(hovering_sumstats) %in% "n_vec") - 1,
                                                        "]}"))
  }
  
  plotted_col_metadata$hovertemplate <- paste0(plotted_col_metadata$hovertemplate, "<extra></extra>")
  
  return(plotted_col_metadata)
  
}

create_hovering <- function(plotted_col_metadata, pivoted_df, stacked_colgroup, stacked_vec, rev_map, xlabels_levels){
  
  # Only add cost hover to bar traces
  if ((plotted_col_metadata$type %||% "") != "bar") {
    
    return(plotted_col_metadata)
    
  } else {
    
    stacked_colname <- plotted_col_metadata$name %||% ""
    
    # Map plotted_colname to original df collumn name:
    
    stacked_colname <- if (!is.na(rev_map[stacked_colname])) rev_map[stacked_colname] else stacked_colname
    
    
    # Subset summary data for this cost_type and order by x and Ensure the same x order
    
    sub <- pivoted_df %>%
      filter(.data[[stacked_colgroup]] == stacked_colname) %>%
      mutate(xlabels = factor(xlabels, levels = xlabels_levels)) %>%
      arrange(xlabels)
    
    
    value_k <- sub$mean_value / K_FACTOR
    n_vec   <- sub$n
    
    plotted_col_metadata <- design_hovering(plotted_col_metadata, list(value_k = value_k, n_vec = n_vec))
    
    return(plotted_col_metadata)
  }
}

render_plots <- function(obj) {
  
  # obj is list(plot, data)
  gp  <- obj$plot
  df  <- obj$data          # summary_df with mean_value & n
  stacked_vec <- obj$barfill
  
  plt <- ggplotly(gp)
  
  seen_colnames <- character()
  
  plt <- layout(plt, hovermode = "closest")
  
  # We need per-trace (cost_type) vectors of value_k and n in the same order as trace points.
  # Plotly creates one trace per cost_type.
  # For each trace name (fullData.name), subset df and order by the x (round_income) factor
  # to match bar positions.
  
  xlabels_levels <- retrieve_barplot_xlabels(plt$x$data)
  
  # Map legend label back to cost_type value. If you used labels, we need a reverse map:
  # build it once outside and keep it around; for demo we rebuild quickly:
  # Suppose you still have 'stacked_vec' and 'fill_labels_all' in scope. If not, create a reverse map:
  # reverse mapping label -> cost_type (if you used fill_labels_all)
  rev_map <- setNames(names(fill_labels_all[stacked_vec]), fill_labels_all[stacked_vec])
  
  for (i in seq_along(plt$x$data)) {
    
    plt$x$data[[i]] <- convert2plotly_legend(plt$x$data[[i]], "Round Spendable Income", "Round costs", seen_colnames)
    
    plt$x$data[[i]] <- create_hovering(plt$x$data[[i]], df, "cost_type", stacked_vec, rev_map, xlabels_levels)
    
  }
  
  plt
}       

