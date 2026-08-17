#R/create-GP2-plot.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))

source(here::here(file.path(FUNCTION_PATH, "help-plot-creation.R")))


adjust_GP2_plotly_height <- function(barlevels) {
  
  "600px"
}


create_GP2_plotly_layout <- function(xtitle, xlevels, y_title, y_axis_range, y2_title = NA, nl_char = " - ") {
  
  xtitle <- signal_newline(xtitle, nl_char)
  y_title <- signal_newline(y_title, nl_char)
  y2_title <- ifelse(is.na(y2_title), NA, signal_newline(y2_title, " - "))
  
  out_plot <- plotly::plot_ly() %>%
    plotly::layout(
      barmode   = "relative",
      hovermode = "closest",
      
      xaxis = list(
        title         = xtitle,
        categoryorder = "array",
        categoryarray = xlevels,
        
        # (ii) vertical grid lines
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.08)",
        gridwidth = 1,
        zeroline  = FALSE
      ),
      
      yaxis = list(
        title    = y_title,
        rangemode = "normal",
        range     = y_axis_range,
        showgrid  = TRUE,
        gridcolor = "rgba(0,0,0,0.06)",
        gridwidth = 1,
        zeroline  = TRUE,
        zerolinecolor = "rgba(0,0,0,0.25)",
        zerolinewidth = 1
      ),
      
      # (iv) legend position near top/right (over/near y2 title)
      
      legend = list(
        x = 1.1, y = 1.08,          # moved left (inside/closer to plot)
        xanchor = "left",           # anchor from right edge so it pulls inward
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.65)",
        traceorder = "grouped",     # <-- THIS makes legend split by 
        tracegroupgap = 12
      ),
      
      margin = list(r = 280, t = 60),  # smaller right margin since legend moved left
      autosize = TRUE
    )
  
  if (is.na(y2_title) == FALSE) {
    
    out_plot <- out_plot %>%
      plotly::layout(
        yaxis2 = list(
          title     = y2_title,
          overlaying = "y",
          side      = "right",
          rangemode = "tozero",
          showgrid  = FALSE,
          zeroline  = FALSE
        )
      )
  }
  
  return(out_plot)
}

add_GP2_bar_data <- function(out_plot, bar_df, selected_bar_labels, bar_legend_title) {
  
  # ---- (iii) legend group titles: set only once per group ----
  first_bar <- TRUE
  
  legend_label_order <- selected_bar_labels             # desired legend order (unchanged)
  legend_label_match  <- rev(selected_bar_labels)         # stacking order (reversed)
  
  # --- Add stacked bar traces ---
  for (label in legend_label_match) {
    
    segment_df <- bar_df %>% dplyr::filter(mean_label == label)
    
    # label + color fallbacks
    bar_color <- rlang::`%||%`(fill_values_all[names(fill_values_all) %in% label], "#808080")
    
    out_plot <- out_plot %>%
      plotly::add_bars(
        data = segment_df,
        x = segment_df[, "xlabels"],
        y = segment_df[, "mean_k"],
        name = label,
        marker = list(color = bar_color),
        
        legendgroup = "bars",
        legendgrouptitle = if (first_bar) list(text = bar_legend_title) else NULL,
        legendrank = match(label, legend_label_order), # keep legend order the same as original stacked_vec
        
        customdata = segment_df[, "N"],
        hovertemplate = paste0(
          "<b>", label, "</b><br>",
          "Mean: %{y:.2f}k<br>",
          "N: %{customdata}<extra></extra>"
        )
      )
    
    first_bar <- FALSE
  }
  
  return(out_plot)
}

add_GP2_scatter_data <- function(out_plot, scatter_df, scatter_legend_title) {
  # --- Add satisfaction line+markers on y2 ---
  
  # label + color fallbacks
  line_color <- "darkgreen"
  line_width <- 2
  marker_size <- 7
  
  for (label in unique(scatter_df$mean_label)) {
    
    line_df <- scatter_df %>% dplyr::filter(mean_label == label)
    
    
    out_plot <- out_plot %>%
      plotly::add_trace(
        data = line_df,
        x = line_df[, "xlabels"],
        y = line_df[, "mean_value"],
        type = "scatter",
        mode = "lines+markers",
        name = label,
        showlegend = TRUE,             # <--- add this
        
        yaxis = "y2",
        legendgroup = "line1",
        legendgrouptitle = list(text = scatter_legend_title),
        
        
        # ensure this group appears AFTER the bars
        legendrank = length(unique(scatter_df$mean_label)) + 100,
        
        line = list(color = line_color, width = line_width),
        marker = list(color = line_color, size = marker_size),
        
        customdata = line_df[, "N"],
        hovertemplate = paste0(
          "<b>", label, "</b><br>",
          "Mean: %{y:.2f}k<br>",
          "N: %{customdata}<extra></extra>"
        )
      )
  }
  
  return(out_plot)
}


create_GP2_plotly <- function(plot_data) {
  
  bar_df                <- plot_data$bar_df
  scatter_df            <- plot_data$scatter_df
  selected_bar_labels   <- names(plot_data$selected_bar_segments)
  xlevels               <- plot_data$barlevels
  
  # keep only colors/labels for selected stacks
  bar_colors <- fill_values_all[names(fill_values_all) %in% selected_bar_labels]
  
  # compute a symmetric-ish range so negatives are visible (optional but helps)
  bar_y_min <- calculate_axis_min(bar_df, "xlabels", "mean_k")
  bar_y_max <- calculate_axis_max(bar_df, "xlabels", "mean_k")
  
  # Start plotly
  
  GP2_plot <- create_GP2_plotly_layout("Round income (k) - Players per class",
                                   xlevels,
                                   "Game Currency (k)",
                                   c(bar_y_min, bar_y_max),
                                   "Average total satisfaction", " - ")
  
  GP2_plot <- add_GP2_bar_data(GP2_plot, bar_df, selected_bar_labels, "Round costs") 
  
  
  GP2_plot <- add_GP2_scatter_data(GP2_plot, scatter_df, "Satisfaction") 
  
  GP2_plot
}


save_and_view_GP2_plot <- function(plot_data,
                                   file = file.path(RESULTS_PATH, "GP2_plot.png"),
                                   vwidth = 1600,
                                   vheight = 800) {

  ## Create the interactive Plotly widget
  GP2_plot <- create_GP2_plotly(plot_data)
  
  save_and_view_plotly(GP2_plot, file, vwidth, vheight)
  
  ## Return the PNG file path
  return(invisible(file))
}
