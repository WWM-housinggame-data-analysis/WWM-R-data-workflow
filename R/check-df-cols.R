# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Check data frame is in the expected format and columns exist
check_df_cols <- function(df, cols) {
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df),
            "No columns names found in data frame" = length(names(df)) > 0,
            "No rows found in data frame" = nrow(df) > 0)
  
  ## Check cols are character
  stopifnot("cols expected to be character" = is.character(cols),
            "No cols found" = length(cols) > 0)
  
  ## Check columns are found in data frame
  if (any(cols %in% names(df) == FALSE)){
    stop(paste0("These df columns could not be found: ",
                paste(cols[cols %in% names(df) == FALSE], collapse = ", "),
                "."))
  }
}

## Check data frame is in the expected format and columns exist and are numeric
check_num_cols <- function(df, cols) {
  
  ## Check is data frame and columns are found in data frame
  check_df_cols(df, cols)
  
  ## check columns are numeric
  detect_num <- unlist(lapply(df[, cols], is.numeric))
  
  if (any(detect_num == FALSE)) {
    
    stop(paste0("These df columns expected to be numeric: ",
                paste(names(detect_num)[detect_num == FALSE], collapse = ", "),
                ".")
    )
  }
}

## Check data frame is in the expected format and columns exist and are character or factor
check_char_cols <- function(df, cols) {
  
  ## Check is data frame and columns are found in data frame
  check_df_cols(df, cols)
  
  ## Check character columns are defined as such or as factor
  detect_char <- unlist(lapply(df[, cols], is.character)) + unlist(lapply(df[, cols], is.factor))
  
  if (any(detect_char == 0)) {
    stop(paste0("These df columns expected to be factor or character: ",
                paste(names(detect_char)[detect_char == 0], collapse = ", "),
                ".")
    )
  }
}

## Check data frame is in the expected format, columns to which constants refer in calculation exist and are logical
check_logical_cols <- function(df, cols) {
  
  ## Check is data frame and columns are found in data frame
  check_df_cols(df, cols)
  
  ## Check logical columns are defined as such
  detect_logical <- unlist(lapply(df[, cols], is.logical))
  
  if (any(detect_logical == FALSE)) {
    stop(paste0("These df columns expected to be logical: ",
                paste(names(detect_logical)[detect_logical == FALSE], collapse = ", "),
                ".")
    )
  }
}
## Report missing cols for a given dataframe
report_missing_cols <- function(df, in_cols, out_cols) {
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  ## Assumption that there are no missing columns
  missing_all <- FALSE
  
  ## Warning for if all input columns are missing
  if (any(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("(All) expected collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in df. Column(s) ", 
                   paste(out_cols, collapse = ", "),
                   " cannot be added to this dataframe."
    )
    )
    
    missing_all <- TRUE
    
  } else if (all(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("Expected Collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in df. They will not be used in Calculating collumn(s) ",
                   paste(out_cols, collapse = ", "), "."
    )
    )
  }
  
  return(missing_all)
}