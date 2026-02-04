# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))

calculate_bar_maxs <- function(bar_df, group_col, y_col) {
  bar_maxs <- bar_df %>%
    filter(y_col > 0) %>%
    droplevels() %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      bar_max = sum(.data[[y_col]]),
      .groups    = "drop"
    ) %>%
    pull(bar_max)
  
  return(bar_maxs)
}

calculate_bar_mins <- function(bar_df, group_col, y_col) {
  bar_mins <- bar_df %>%
    filter(y_col < 0) %>%
    droplevels() %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      bar_min = sum(.data[[y_col]]),
      .groups    = "drop"
    ) %>%
    ungroup() %>%
    pull(bar_min)
  
  return(bar_mins)
}

calculate_y_min <- function(bar_df, group_col, y_col) {
  
  bar_mins <- calculate_bar_mins(bar_df, group_col, y_col)
  
  y_min <- min(0, bar_mins, na.rm = TRUE) + 0.05 * min(0, bar_mins, na.rm = TRUE)
  
  return(y_min)
}

calculate_y_max <- function(bar_df, group_col, y_col) {
  
  bar_maxs <- calculate_bar_maxs(bar_df, group_col, y_col)
  
  y_max <- max(bar_maxs, na.rm = TRUE) + 0.05 * max(bar_maxs, na.rm = TRUE)
  
  return(y_max)
}

create_GP1_plotly <- function(plot_data) {
  
  bar_df                <- plot_data$bar_df
  scatter_df            <- plot_data$scatter_df
  selected_bar_segments <- plot_data$selected_bar_segments
  xlevels               <- plot_data$xlevels
  
  # keep only colors/labels for selected stacks
  bar_colors <- fill_values_all[names(fill_values_all) %in% selected_bar_segments]
  
  # compute a symmetric-ish range so negatives are visible (optional but helps)
  bar_y_min <- calculate_y_min(bar_df, "xlabels", "mean_k")
  bar_y_max <- calculate_y_max(bar_df, "xlabels", "mean_k")
  
  # Start plotly
  p <- plot_ly() %>%
    layout(
      barmode   = "relative",
      hovermode = "closest",
      
      xaxis = list(
        title         = "Round income (k)<br>Players per class",
        categoryorder = "array",
        categoryarray = xlevels,
        
        # (ii) vertical grid lines
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.08)",
        gridwidth = 1,
        zeroline  = FALSE
      ),
      
      yaxis = list(
        title    = "Game Currency (k)",
        rangemode = "normal",
        range     = c(bar_y_min, bar_y_max),
        showgrid  = TRUE,
        gridcolor = "rgba(0,0,0,0.06)",
        gridwidth = 1,
        zeroline  = TRUE,
        zerolinecolor = "rgba(0,0,0,0.25)",
        zerolinewidth = 1
      ),
      
      yaxis2 = list(
        title     = "Average total satisfaction",
        overlaying = "y",
        side      = "right",
        rangemode = "tozero",
        showgrid  = FALSE,
        zeroline  = FALSE
      ),
      
      # (iv) legend position near top/right (over/near y2 title)
      
      legend = list(
        x = 1.10, y = 1.08,          # moved left (inside/closer to plot)
        xanchor = "left",           # anchor from right edge so it pulls inward
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.65)",
        traceorder = "grouped",     # <-- THIS makes legend split by 
        tracegroupgap = 12
      ),
      
      margin = list(r = 240, t = 60)  # smaller right margin since legend moved left
      
    )
  
  # ---- (iii) legend group titles: set only once per group ----
  first_bar <- TRUE
  
  legend_label_order <- selected_bar_segments             # desired legend order (unchanged)
  legend_label_match  <- rev(selected_bar_segments)         # stacking order (reversed)
  
  # --- Add stacked bar traces ---
  for (label in legend_label_match) {
    
    segment_df <- bar_df %>% filter(mean_label == label)
    
    # label + color fallbacks
    bar_color <- fill_values_all[names(fill_values_all) %in% label] %||% "#808080"
    
    p <- p %>%
      add_bars(
        data = segment_df,
        x = ~xlabels,
        y = ~mean_k,
        name = label,
        marker = list(color = bar_color),
        
        legendgroup = "bars",
        legendgrouptitle = if (first_bar) list(text = "Round costs") else NULL,
        legendrank = match(label, legend_label_order), # keep legend order the same as original stacked_vec
        
        customdata = ~N,
        hovertemplate = paste0(
          "<b>", label, "</b><br>",
          "Mean: %{y:.2f}k<br>",
          "N: %{customdata}<extra></extra>"
        )
      )
    
    first_bar <- FALSE
  }
  
  # --- Add satisfaction line+markers on y2 ---
  
  for (label in unique(scatter_df$mean_label)) {
    
    line_df <- scatter_df %>% filter(mean_label == label)
    
    # label + color fallbacks
    line_color <- "darkgreen"
    line_width <- 2
    marker_size <- 7
    legend_title <- "Satisfaction"
    
    p <- p %>%
      add_trace(
        data = line_df,
        x = ~xlabels,
        y = ~mean_value,
        type = "scatter",
        mode = "lines+markers",
        name = label,
        showlegend = TRUE,             # <--- add this
        
        yaxis = "y2",
        legendgroup = "line1",
        legendgrouptitle = list(text = legend_title),
        
        
        # ensure this group appears AFTER the bars
        legendrank = length(unique(scatter_df$mean_label)) + 100,
        
        line = list(color = line_color, width = line_width),
        marker = list(color = line_color, size = marker_size),
        
        customdata = ~N,
        hovertemplate = paste0(
          "<b>", label, "</b><br>",
          "Mean: %{y:.2f}k<br>",
          "N: %{customdata}<extra></extra>"
        )
      )
  }

  p
}