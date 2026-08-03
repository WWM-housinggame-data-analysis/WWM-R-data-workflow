#R/create-GP2-plot.R

# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

# Load required functions
source(here::here(file.path(FUNCTION_PATH, "constants.R")))

# ---------------------------------------------------------
# Helper: base64 encode icon file for Plotly images
# ---------------------------------------------------------
encode_b64 <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  mime <- if (ext %in% c("jpg", "jpeg")) "image/jpeg" else if (ext == "svg") "image/svg+xml" else "image/png"
  raw  <- readBin(path, "raw", n = file.info(path)$size)
  paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
}

view_image_in_rstudio <- function(image_path) {
  html_file <- tempfile(fileext = ".html")
  
  html <- sprintf(
    '<html>
       <head>
         <style>
           body { margin: 0; background: #ffffff; }
           img  { width: 100%%; height: auto; }
         </style>
       </head>
       <body>
         <img src="%s" />
       </body>
     </html>',
    basename(image_path)
  )
  
  writeLines(html, html_file)
  
  # Copy image next to HTML so relative paths work
  file.copy(image_path,
            file.path(dirname(html_file), basename(image_path)),
            overwrite = TRUE)
  
  rstudioapi::viewer(html_file)
}

save_and_view_plotly <- function(plotly_plot, file = "plotly_plot.png", vwidth = 1600, vheight = 800) {
  
  # 1. Check input is interactive Plotly widget
  stopifnot(inherits(plotly_plot, "htmlwidget"))
  
  # 2. Save to a temporary HTML file
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(plotly_plot, html_file, selfcontained = TRUE)
  
  # 3. Convert the HTML to a PNG using webshot2 (NO Python required)
  webshot2::webshot(
    url = html_file,
    file = file,
    vwidth = vwidth,
    vheight = vheight
  )
  
  # 4. Display PNG INSIDE RStudio Viewer
  view_image_in_rstudio(normalizePath(file))
  
  # 5. Return the PNG file path
  return(invisible(file))
}

calculate_bar_maxs <- function(bar_df, group_col, vals_col) {
  bar_maxs <- bar_df %>%
    dplyr::filter(.data[[vals_col]] > 0) %>%
    droplevels() %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      bar_max = sum(.data[[vals_col]]),
      .groups    = "drop"
    ) %>%
    dplyr::pull(bar_max)
  
  return(bar_maxs)
}

calculate_bar_mins <- function(bar_df, group_col, vals_col) {
  bar_mins <- bar_df %>%
    dplyr::filter(.data[[vals_col]] < 0) %>%
    droplevels() %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      bar_min = sum(.data[[vals_col]]),
      .groups    = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::pull(bar_min)

  return(bar_mins)
}

calculate_axis_min <- function(bar_df, group_col, vals_col) {
  
  bar_mins <- calculate_bar_mins(bar_df, group_col, vals_col)
  
  y_min <- min(0, bar_mins, na.rm = TRUE) + 0.05 * min(0, bar_mins, na.rm = TRUE)
  
  return(y_min)
}

calculate_axis_max <- function(bar_df, group_col, vals_col) {
  
  bar_maxs <- calculate_bar_maxs(bar_df, group_col, vals_col)
  
  y_max <- max(0, bar_maxs, na.rm = TRUE) + 0.05 * max(0, bar_maxs, na.rm = TRUE)
  
  return(y_max)
}

signal_newline <- function(in_string, nl_char) {
  gsub(nl_char, "<br>", in_string)
}

create_plotly_layout <- function(xtitle, ylevels, y_title = "Improvement type", x_axis_range, nl_char = " - ") {
  
  xtitle <- signal_newline(xtitle, nl_char)
  y_title <- signal_newline(y_title, nl_char)
  y2_title <- ifelse(is.na(y2_title), NA, signal_newline(y2_title, " - "))
  
  tick_vals <- pretty(max(x_axis_range), 6)
  
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
        title    = y_title,
        
        categoryorder = "array",
        categoryarray = xlevels,
        
        showgrid  = TRUE,
        gridcolor = "rgba(0,0,0,0.06)",
        gridwidth = 1,
        
        zeroline  = TRUE,
        zerolinecolor = "rgba(0,0,0,0.25)",
        zerolinewidth = 1
      ),
      
      # (iv) legend position near top/right (over/near y2 title)
      
      legend = list(
        x = 1.20, y = 1.08,          # moved left (inside/closer to plot)
        xanchor = "left",           # anchor from right edge so it pulls inward
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.65)",
        traceorder = "grouped",     # <-- THIS makes legend split by 
        tracegroupgap = 12
      ),
      
      margin = list(r = 280, t = 60)  # smaller right margin since legend moved left
      
    )
  
  return(out_plot)
}

add_bar_data <- function(out_plot, bar_df, selected_bar_labels, bar_legend_title) {
  
  # ---- (iii) legend group titles: set only once per group ----
  first_bar <- TRUE
  
  legend_label_order <- selected_bar_labels             # desired legend order (unchanged)
  legend_label_match  <- rev(selected_bar_labels)        # stacking order (reversed)
  
  barseg_colors <- WELFARE_BARSEG_COLORPALT(length(legend_label_match))
  
  # --- Add stacked bar traces ---
  for (label in legend_label_match) {
    
    segment_df <- bar_df %>% dplyr::filter(mean_label == label)
    
    # label + color fallbacks
    bar_color <- rlang::`%||%`(barseg_colors[which(legend_label_match %in% label)], "#808080")
    
    out_plot <- out_plot %>%
      plotly::add_bars(
        data = segment_df,
        x = segment_df[, "N"],
        y = segment_df[,  MEASURE_ALIAS_COL],
        name = label,
        marker = list(color = bar_color),
        
        legendgroup = "bars",
        legendgrouptitle = if (first_bar) list(text = bar_legend_title) else NULL,
        legendrank = match(label, legend_label_order), # keep legend order the same as original stacked_vec
        
        hovertemplate = paste0(
          "<b>", label, "</b><br>",
          "Mean: %{y}<extra></extra>"
        )
      )
    
    first_bar <- FALSE
  }
  
  return(out_plot)
}


create_GP3_plotly <- function(plot_data) {
  
  bar_df                <- plot_data$n_df
  selected_bar_groups   <- plot_data$selected_bar_groups
  ylevels               <- plot_data$ylevels
  
  # compute a symmetric-ish range so negatives are visible (optional but helps)
  bar_x_min <- calculate_axis_min(bar_df, "ylabels", "N")
  bar_x_max <- calculate_axis_max(bar_df, "ylabels", "N")
  x_off  <- -0.12 * bar_x_max
  
  icon_map <- bar_df %>%
    select(short_alias, label, icons_path) %>%
    distinct() %>%
    mutate(
      icon_file = ifelse(
        grepl("\\.(png|jpg|jpeg|svg)$", icons_path, ignore.case = TRUE),
        icons_path,
        paste0(icons_path, ".png")
      )
    ) %>%
    filter(file.exists(icon_file)) %>%
    mutate(src = vapply(icon_file, encode_b64, FUN.VALUE = character(1)))
  
  # Start plotly
  
  GP3_plot <- create_plotly_layout("Frequency",
                                   ylevels,
                                   "Private adaptation measures",
                                   c(bar_x_min, bar_x_max))
  
  GP3_plot <- add_bar_data(GP3_plot, bar_df, selected_bar_labels, "Round costs") 
  
  images_list <- lapply(seq_len(nrow(icon_map)), function(i) {
    list(
      source   = icon_map$src[i],
      xref     = "x", yref = "y",
      x        = x_off,
      y        = as.character(icon_map$label[i]),
      sizex    = 0.08 * bar_x_max,
      sizey    = 0.8,
      xanchor  = "left",
      yanchor  = "middle",
      layer    = "above"
    )
  })
  
  GP3_plot <- layout(GP3_plot, images = images_list)
  
  GP3_plot
}


save_and_view_GP3_plot <- function(plot_data,
                                   file = file.path(RESULTS_PATH, "GP3_plot.png"),
                                   vwidth = 1600,
                                   vheight = 800) {

  ## Create the interactive Plotly widget
  GP3_plot <- create_GP3_plotly(plot_data)
  
  save_and_view_plotly(GP3_plot, file, vwidth, vheight)
  
  ## Return the PNG file path
  return(invisible(file))
}
