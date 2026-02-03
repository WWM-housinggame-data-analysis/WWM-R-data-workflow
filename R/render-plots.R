render_plots <- function(obj) {
  
  df          <- obj$summary_df
  ave         <- obj$ave_data
  stacked_vec <- obj$stacked_vec
  xlevels     <- obj$xlevels
  fill_values <- obj$fill_values
  fill_labels <- obj$fill_labels
  
  # Ensure ordering matches for all traces
  df <- df %>%
    mutate(xlabels = factor(xlabels, levels = xlevels)) %>%
    arrange(xlabels)
  
  ave <- ave %>%
    mutate(xlabels = factor(xlabels, levels = xlevels)) %>%
    arrange(xlabels)
  
  # Convert bars to k for left axis # ---- ensure negatives for "spent savings" (EDIT this code to match your real cost_type) ----
  df <- df %>%
    mutate(
      #mean_value = if_else(cost_type == "spent_savings", -abs(mean_value), mean_value),
      mean_k = mean_value / K_FACTOR
    )
  
  bar_total <- df %>%
    group_by(xlabels) %>%
    summarise(
      colsum = sum(mean_k),
      .groups    = "drop"
    ) %>%
    as.data.frame
  
  # compute a symmetric-ish range so negatives are visible (optional but helps)
  y_min <- min(0, df$mean_k, na.rm = TRUE)
  y_max <- max(0, bar_total$colsum, na.rm = TRUE)
  
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
        range     = c(y_min, y_max),
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
  
  stacked_vec_legend <- stacked_vec              # desired legend order (unchanged)
  stacked_vec_stack  <- rev(stacked_vec)         # stacking order (reversed)
  
  # --- Add stacked bar traces ---
  for (ct in stacked_vec_stack) {
    
    sub <- df %>% filter(cost_type == ct)
    
    # label + color fallbacks
    nm  <- fill_labels[[ct]] %||% ct
    col <- fill_values[[ct]] %||% "#808080"
    
    p <- p %>%
      add_bars(
        data = sub,
        x = ~xlabels,
        y = ~mean_k,
        name = nm,
        marker = list(color = col),
        
        legendgroup = "bars",
        legendgrouptitle = if (first_bar) list(text = "Round costs") else NULL,
        legendrank = match(ct, stacked_vec_legend), # keep legend order the same as original stacked_vec
        
        customdata = ~n,
        hovertemplate = paste0(
          "<b>", nm, "</b><br>",
          "Mean: %{y:.2f}k<br>",
          "N: %{customdata}<extra></extra>"
        )
      )
    
    first_bar <- FALSE
  }
  
  # --- Add satisfaction line+markers on y2 ---
  p <- p %>%
    add_trace(
      data = ave,
      x = ~xlabels,
      y = ~ave_satisfaction,
      type = "scatter",
      mode = "lines+markers",
      name = "Average total satisfaction",
      showlegend = TRUE,             # <--- add this
      
      yaxis = "y2",
      legendgroup = "line1",
      legendgrouptitle = list(text = "Satisfaction"),
      
      
      # ensure this group appears AFTER the bars
      legendrank = length(stacked_vec_legend) + 100,
      
      line = list(color = "darkgreen", width = 2),
      marker = list(color = "darkgreen", size = 7),
      
      hovertemplate = paste0(
        "<b>Average total satisfaction</b><br>",
        "%{y:.2f}<extra></extra>"
      )
    )
  
  p
}