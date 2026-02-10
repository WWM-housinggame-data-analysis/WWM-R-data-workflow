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
  
  ## Check PREPROCESSED_DBTABLES exists
  stopifnot("Default variable PREPROCESSED_DBTABLES not found in R/constants.R" = exists(deparse(substitute(PREPROCESSED_DBTABLES))))
  
  ## Define excel_outpath based on consistency between input logical argument `preprocessed` and presence/absence of PREPROCESSED_DBTABLES in sessiontable_list
  ## (In)consistencies are tested.
  if (preprocessed && all(PREPROCESSED_DBTABLES %in% names(sessiontable_list) == TRUE)) {
    
    excel_outpath <- PREPRDATA_PATH
    
  } else if (preprocessed && any(PREPROCESSED_DBTABLES %in% names(sessiontable_list) == FALSE)) {
    
    stop(paste0("Expected table(s) named ",
                "'", paste(PREPROCESSED_DBTABLES[PREPROCESSED_DBTABLES %in% names(sessiontable_list) == FALSE], collapse = ", "), "'",
                " not found in table list."))
    
  } else if (preprocessed == FALSE && any(PREPROCESSED_DBTABLES %in% names(sessiontable_list) == TRUE)) {
    
    stop(paste0("Non-expected table named '",
                "'", paste(PREPROCESSED_DBTABLES[PREPROCESSED_DBTABLES %in% names(sessiontable_list) == TRUE], collapse = ", "), "'",
                "income_dist_df", " found in table list."))
    
  } else if (preprocessed == FALSE && all(PREPROCESSED_DBTABLES %in% names(sessiontable_list) == FALSE)) {
    
    excel_outpath = RAWDATA_PATH
  }
  
  # Define workflow stage label to be appended to excel file name
  workflow_stage <- WORKFLOW_STAGES[names(WORKFLOW_STAGES) %in% excel_outpath]
  
  # Export data available for a given session to excel file
  tryCatch({
    write_xlsx(sessiontable_list,
               here(file.path(excel_outpath,
                              paste0(paste(session_name, timestamp, workflow_stage, sep = "-"), ".xlsx"))))
    
    message("File written successfully.")
    
  }, error = function(e) {
    
    message("Error: ", e$message)
  })
}


## Retrieve tables as dataframes stored inside a named list
upload_dbtables <- function(folder_path, subfolder_pattern, excel = FALSE, selection = TRUE) {
  
  ## Retrieve table files inside subfolders of folder_path whose names match subfolder_pattern
  dbtable_filenames <- list_matching_dbtables(folder_path, subfolder_pattern)
  
  ## Check SELECTED_DBTABLES exists
  stopifnot("Default variable SELECTED_DBTABLES not found in R/constants.R" = exists(deparse(substitute(SELECTED_DBTABLES))))
  
  # Check IMPORTED_TABLE_TYPE exists
  stopifnot("Default variable IMPORTED_TABLE_TYPE not found in R/constants.R" = exists(deparse(substitute(IMPORTED_TABLE_TYPE))))
  
  ## Define named list where table files are stored as dataframes based on list containing the respective filenames
  dbtables_list <- dbtable_filenames
  
  ## Populate named list with table data as dataframes
  for (subfolder in names(dbtable_filenames)) {
    
    ## For a given list named after a given subfolder, import into the list each table file as dataframe
    
    if (identical(IMPORTED_TABLE_TYPE, ".csv")) {
      dbtables_list[[subfolder]] <- lapply(dbtable_filenames[[subfolder]], readr::read_csv)
    } else {
      stop(paste0("Non-expected table type ", IMPORTED_TABLE_TYPE, " to be imported."))
    }
    
    ## Name list after subfolder
    names(dbtables_list[[subfolder]]) <- basename(dbtable_filenames[[subfolder]])
    
    ## If logical argument asking for table selection based on SELECTED_DBTABLES is true
    if (selection) { 
      dbtables_list[[subfolder]] <- dbtables_list[[subfolder]][names(dbtables_list[[subfolder]]) %in% SELECTED_DBTABLES == TRUE]
    }
    
    ## If logical argument asking for excel export of named list is true
    if(excel) {
      export_excel(dbtables_list[[subfolder]], subfolder, preprocessed = FALSE)
    }
  }
  
  return(dbtables_list) 
}