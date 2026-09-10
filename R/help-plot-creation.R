#R/create-GP3-plot.R

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