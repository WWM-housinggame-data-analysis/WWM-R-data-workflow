#R/create-GP3-plot.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))

source(here::here(file.path(FUNCTION_PATH, "help-plot-creation.R")))


adjust_GP3_plotly_height <- function(barlevels) {
  
  n_measures <- length(barlevels)
  
  height_px <- max(600, n_measures * 50)
  
  paste0(height_px, "px")

}



create_GP3_plotly_layout <- function(xtitle, ylevels, x_axis_range, plotly_configs, nl_char = " - ") {
  
  xtitle <- signal_newline(xtitle, nl_char)
  yaxis_title <- signal_newline(plotly_configs[["yaxis_title"]], nl_char)
  
  tick_vals <- round(seq(x_axis_range[1], round(x_axis_range[2]), (round(x_axis_range[2]) - x_axis_range[1]) / 5), 0)
  
  out_plot <- plotly::plot_ly() %>%
    plotly::layout(
      
      title = list(
        text = paste0(
          "Limited information scenario distribution", LINEBREAK,
          "<sub style='color:#666666;font-size:16px'>",
          "</sub>"
        ),
        x = 0.5,
        xanchor = "center",
        font = list(size = 22, color = "#333333")
      ),
      
      barmode   = "stack",
      hovermode = "closest",
      
      xaxis = list(
        title         = xtitle,
        
        range = x_axis_range,
        rangemode = "normal",
        
        tickmode = "array",
        tickvals = tick_vals,
        ticktext = tick_vals,
        
        zeroline = TRUE,
        zerolinecolor = "#aaaaaa",
        zerolinewidth = 1,
        
        # (ii) vertical grid lines
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.08)",
        gridwidth = 1
      ),
      
      yaxis = list(
        title    = list(text = yaxis_title,
                        standoff = plotly_configs[["yaxis_standoff"]]),
        
        categoryorder = "array",
        categoryarray = ylevels,
        
        showticklabels = FALSE,
        
        showgrid  = TRUE,
        gridcolor = "rgba(0,0,0,0.06)",
        gridwidth = 1,
        
        zeroline  = TRUE,
        zerolinecolor = "rgba(0,0,0,0.25)",
        zerolinewidth = 1
      ),
      
      # (iv) legend position near top/right (over/near y2 title)
      
      legend = list(
        x = 1.05, y = 1.08,          # moved left (inside/closer to plot)
        xanchor = "left",           # anchor from right edge so it pulls inward
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.65)",
        traceorder = "grouped",     # <-- THIS makes legend split by 
        tracegroupgap = 12
      ),

      margin = list(t = plotly_configs[["top_margin"]],
                    r = plotly_configs[["right_margin"]],
                    l = plotly_configs[["left_margin"]]),  # smaller right margin since legend moved left
      autosize = TRUE
    )
  
  return(out_plot)
}


add_GP3_bar_data <- function(out_plot, bar_df, selected_bar_segments, bar_legend_title) {
  
  # ---- (iii) legend group titles: set only once per group ----
  first_bar <- TRUE
  
  legend_label_order <- selected_bar_segments             # desired legend order (unchanged)
  legend_label_match  <- selected_bar_segments        # stacking order (reversed)
  
  barseg_colors <- WELFARE_BARSEG_COLORPALT(length(legend_label_match))
  
  
  
  # --- Add stacked bar traces ---
  for (label in legend_label_match) {
    
    segment_df <- bar_df %>% dplyr::filter(.data[[GP3_BARGEGLABEL_COL]] == label)
    
    if (nrow(segment_df) > 0) {
      
      # label + color fallbacks
      bar_color <- rlang::`%||%`(barseg_colors[which(legend_label_match %in% label)], "#808080")
      
      out_plot <- out_plot %>%
        plotly::add_bars(
          data = segment_df,
          x = ~get("N"),
          y = ~get(GP3_YLABEL_COL),
          customdata = segment_df[, "measure_total_N"],
          
          name = label,
          text = segment_df[, FREQUENT_ROUND_COL],
          marker = list(color = bar_color),
          
          legendgroup = "bars",
          legendgrouptitle = if (first_bar) list(text = bar_legend_title) else NULL,
          legendrank = match(label, legend_label_order), # keep legend order the same as original stacked_vec
          
         # texttemplate="%{text}",
          textposition="inside",
          
          hovertemplate = paste0(
            bar_legend_title, ": ", label, "<br>",
            "N", ": %{x}", "<br>",
            "Measure Total N", ": %{customdata}<extra></extra>"
          )
        )
      
      first_bar <- FALSE
    }
  }
  return(out_plot)
}

create_plotly_icon_list <- function(df, path_col, axislabel_col) {
  
  icon_map <- df %>%
    dplyr::select(tidyselect::all_of(c(axislabel_col, path_col))) %>%
    distinct() %>%
    mutate(icons_path = as.character(.data[[path_col]])) %>%
    mutate(src = vapply(.data[[path_col]], encode_b64, FUN.VALUE = character(1)))
  
  lapply(seq_len(nrow(icon_map)), function(i) {
    list(
      source   = icon_map$src[i],
      xref     = "paper",
      yref     = "y",
      x        = -0.03,
      y        = icon_map$ylabels[i],
      sizex    = .8,
      sizey    = .8,
      xanchor  = "center",
      yanchor  = "middle",
      layer    = "above"
    )
  })
}

create_plotly_axislabels_annotations <- function(axislabels) {
  
  annotations <- lapply(seq_along(axislabels), function(i) {
    list(
      xref = "paper",
      yref = "y",
      x = -0.06,
      y = axislabels[i],
      text = axislabels[i],
      showarrow = FALSE,
      xanchor = "right"
    )
  })
}


create_GP3_plotly <- function(plot_data, plotly_configs) {
  
  bar_df                  <- plot_data$n_df
  selected_bar_segments   <- levels(bar_df[, "barseglabel"])
  ylevels                 <- plot_data$barlevels
  group_col               <- plot_data$grouping_choice
  
  # compute a symmetric-ish range so negatives are visible (optional but helps)
  bar_x_min <- calculate_axis_min(bar_df, "ylabels", "N")
  bar_x_max <- calculate_axis_max(bar_df, "ylabels", "N")

  # Start plotly
  
  GP3_plot <- create_GP3_plotly_layout("Frequency",
                                       ylevels,
                                       c(bar_x_min, bar_x_max),
                                       plotly_configs)
  
  GP3_plot <- add_GP3_bar_data(GP3_plot, bar_df, selected_bar_segments, names(PLAYER_AGGREGATION_OPTIONS)[PLAYER_AGGREGATION_OPTIONS %in% group_col]) 
  
  GP3_ylabels_annotations <- create_plotly_axislabels_annotations(ylevels)
  
  
  GP3_yaxis_icons <- create_plotly_icon_list(bar_df, MEASURE_ICONS_COL, GP3_YLABEL_COL)
  
  plotly::layout(GP3_plot,
                 annotations = GP3_ylabels_annotations,
                 images = GP3_yaxis_icons)
  
}


save_and_view_GP3_plot <- function(plot_data,
                                   file = file.path(RESULTS_PATH, "GP3_plot.png"),
                                   plotly_configs,
                                   vwidth = 2000,
                                   vheight = 800) {
  
  ## Create the interactive Plotly widget
  GP3_plot <- create_GP3_plotly(plot_data, plotly_configs)
  
  Sys.setenv(
    CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  
  save_and_view_plotly(GP3_plot, file, vwidth, vheight)
  
  ## Return the PNG file path
  return(invisible(file))
}

