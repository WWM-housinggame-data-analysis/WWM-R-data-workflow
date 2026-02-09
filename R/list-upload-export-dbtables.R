# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here(file.path(FUNCTION_PATH, "constants.R")))

# Functions ---

## Find paths to subfolders within given main folder path and matching a given pattern

list_matching_subfolders <- function(folder_path, subfolder_pattern) {
  
  ## Check inputs are characters with length = 1
  stopifnot("folder_path expected to be character" = is.character(folder_path),
            "folder_path expected to have only element 1" = length(folder_path) == 1,
            "subfolder_pattern expected to be character" = is.character(subfolder_pattern),
            "subfolder_pattern expected to have only element 1" = length(subfolder_pattern) == 1)

  ## List all subfolders inside the main folder
  subfolder_paths <- list.dirs(path = here(folder_path), full.names = TRUE, recursive = FALSE)
  
  ## Check any subfolder found
  if (length(subfolder_paths) == 0) {
    stop(sprintf("No subfolder in '%s' found.", folder_path))
  }
  
  ## Filter subfolders that matches your pattern
  subfolder_paths <- subfolder_paths[grepl(subfolder_pattern, basename(subfolder_paths))]
  
  ## Check any subfolder matching pattern found
  if (length(subfolder_paths) == 0) {
    stop(sprintf("No subfolder matching pattern '%s' found.", folder_path))
  }
  
  return(subfolder_paths)
}

## Find dbtable files found in subfolders found inside main folder whose names match given pattern

list_matching_dbtables <- function(folder_path, subfolder_pattern) {
  
  ## List paths to subfolders within given main folder path and matching a given pattern
  subfolder_paths <- list_matching_subfolders(folder_path, subfolder_pattern)
  
  # Check IMPORTED_TABLE_TYPE exists
  stopifnot("Default variable IMPORTED_TABLE_TYPE not found in R/constants.R" = exists(deparse(substitute(IMPORTED_TABLE_TYPE))))
  
  ## Create list to store dbtables found within each subfolder
  dbtable_filenames <- list()
  
  for (subfolder_path in subfolder_paths) {
    
    i <- length(dbtable_filenames) + 1
    
    dbtable_filenames[[i]] <- list.files(path = here(subfolder_path), pattern = paste("\\", IMPORTED_TABLE_TYPE, "$", sep = ""), full.names = TRUE)
    
    ## Warning if no .csv table is found within a given subfolder
    if (length(dbtable_filenames[[i]]) == 0) {
      dbtable_filenames[[i]] <- NA
      warning(paste0("No ", IMPORTED_TABLE_TYPE, "files found in the target subfolder ", subfolder_path, "."))
    }
    
    names(dbtable_filenames)[i] <- basename(subfolder_path)
    
  }
  
  ## Error if no subfolder contains .csv tables
  if (all(is.na(dbtable_filenames) == TRUE)) {
    stop(paste0("No ", IMPORTED_TABLE_TYPE, "files found in any subfolder within ", folder_path, "."))
  }
  
  return(dbtable_filenames)
}

 
## Export Excel file for data relative to a given session (expected to be stored in a subfolder), with sheet names matching table names

export_excel <- function(sessiontable_list, session_name, preprocessed = TRUE) {
  
  ## Generate timestamp to append to excel file name
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  stopifnot("Default variable PREPROCESSED_DBTABLES not found in R/constants.R" = exists(deparse(substitute(PREPROCESSED_DBTABLES))))
  
  if (preprocessed && any(PREPROCESSED_DBTABLES %in% names(sessiontable_list) == TRUE)) {
    
    parent_path <- PREPRDATA_PATH
    
  } else if (preprocessed && "income_dist_df" %in% names(sessiontable_list) == FALSE) {
    
    stop(paste0("Expected table named '", "income_dist_df", "' not found in table list"))
    
  } else if (preprocessed == FALSE && "income_dist_df" %in% names(sessiontable_list) == TRUE) {
    
    stop(paste0("Non-expected table named '", "income_dist_df", "' found in table list"))
    
  } else if (preprocessed == FALSE && "income_dist_df" %in% names(sessiontable_list) == FALSE) {
    
    parent_path = RAWDATA_PATH
  }
  
  workflow_stage <- WORKFLOW_STAGES[names(WORKFLOW_STAGES) %in% parent_path]
  
  tryCatch({
    write_xlsx(sessiontable_list,
               here(file.path(parent_path,
                              paste0(paste(session_name, timestamp, workflow_stage, sep = "-"), ".xlsx"))))
    
    message("File written successfully.")
    
  }, error = function(e) {
    
    message("Error: ", e$message)
  })
}


# Retrieve all tables into a named list
upload_dbtables <- function(folder_path, subfolder_pattern, excel = FALSE, selection = TRUE) {
  
  dbtable_filenames <- list_matching_dbtables(folder_path, subfolder_pattern)
  
  dbtables_list <- dbtable_filenames
  
  for (subfolder in names(dbtable_filenames)) {
    dbtables_list[[subfolder]] <- lapply(dbtable_filenames[[subfolder]], readr::read_csv)
    
    names(dbtables_list[[subfolder]]) <- tools::file_path_sans_ext(basename(dbtable_filenames[[subfolder]]))
    
    if (selection) { 
      dbtables_list[[subfolder]] <- dbtables_list[[subfolder]][names(dbtables_list[[subfolder]]) %in% SELECTED_DBTABLES == TRUE]
    }
    
    if(excel) {
      export_excel(dbtables_list[[subfolder]], subfolder, preprocessed = FALSE)
    }
  }
  
  return(dbtables_list) 
}