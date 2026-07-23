# ------------------------------------------------------------
# Script: R/list-upload-export-dbtables.R
# Purpose: Store functions required for listing, uploading and exporting data
#
# Details:
#   - Contains reusable functions for listing, uploading and exporting data tables
#
# Usage:
#   source("R/list-upload-export-dbtables.R")
#
# Exposed functions:
#   - list_matching_subfolders: Find paths to subfolders within given main folder path and matching a given pattern
#   - list_matching_dbtables:   Find dbtable files found in subfolders found inside main folder whose names match given pattern
#   - export_excel:             Export Excel file for data relative to a given session (expected to be stored in a subfolder), with sheet names matching table names
#   - upload_dbtables:          Retrieve tables as dataframes stored inside a named list
#
# Dependencies:
#   - here
#   - readr
#   - tools
#
# Notes:
#   - Errors are surfaced to the caller
# ------------------------------------------------------------

# Set defaults ----

## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "check-df-cols.R")))


# ------------------------------------------------------------
# Function: list_matching_subfolders
# Purpose: Find paths to subfolders within given main folder path and matching a given pattern
#
# Params:
#   - folder_path (character): path to main folder expected to contain subfolders
#   - subfolder_pattern (character): pattern to filter subfolders whose names include the given string
#
# Returns:
#   - character vector listing found subfolder paths
#
# Called by:
#   - list_matching_dbtables
# ------------------------------------------------------------

list_matching_subfolders <- function(folder_path, subfolder_pattern) {
  
  ## Check inputs are characters with length = 1
  stopifnot("folder_path expected to be character" = is.character(folder_path),
            "folder_path expected to have only element 1" = length(folder_path) == 1,
            "subfolder_pattern expected to be character" = is.character(subfolder_pattern),
            "subfolder_pattern expected to have only element 1" = length(subfolder_pattern) == 1)

  ## List all subfolders inside the main folder
  subfolder_paths <- list.dirs(path = here::here(folder_path), full.names = TRUE, recursive = FALSE)
  
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


# ------------------------------------------------------------
# Function: list_matching_dbtables
# Purpose: Find dbtable files found in subfolders found inside main folder whose names match given pattern
#
# Params:
#   - folder_path (character): path to main folder expected to contain subfolders
#   - subfolder_pattern (character): pattern to filter subfolders whose names include the given string
#
# Returns:
#   - character vector listing found paths to dbtable files
#
# Called by:
#   - upload_dbtables
# ------------------------------------------------------------

list_matching_dbtables <- function(folder_path, subfolder_pattern) {
  
  ## List paths to subfolders within given main folder path and matching a given pattern
  subfolder_paths <- list_matching_subfolders(folder_path, subfolder_pattern)
  
  # Check IMPORTED_TABLE_TYPE is a single character and that subfolder_paths is character with length > 0
  stopifnot("Default variable IMPORTED_TABLE_TYPE not found in R/constants.R" = exists(deparse(substitute(IMPORTED_TABLE_TYPE))),
            "IMPORTED_TABLE_TYPE expected to be character" = is.character(IMPORTED_TABLE_TYPE),
            "IMPORTED_TABLE_TYPE expected to have only element 1" = length(IMPORTED_TABLE_TYPE) == 1,
            "No subfolder paths found" = length(subfolder_paths) > 0)
  
  ## Create list to store dbtables found within each subfolder
  dbtable_filenames <- list()
  
  for (subfolder_path in subfolder_paths) {
    
    i <- length(dbtable_filenames) + 1
    
    dbtable_filenames[[i]] <- list.files(path = here::here(subfolder_path), pattern = paste("\\", IMPORTED_TABLE_TYPE, "$", sep = ""), full.names = TRUE)
    
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


# ------------------------------------------------------------
# Function: export_excel
# Purpose: Export Excel file for data relative to a given session (expected to be stored in a subfolder), with sheet names matching table names
#
# Params:
#   - sessiontable_list (character): character vector listing paths to dbtable files
#   - session_name (character): name of game session which should match folder name where dbtable files listed above are stored
#   - preprocessed (logical): Excel exporting assumes dbtables should be stored in data/preprocessed, otherwise they are stored in data/raw
#
# Outputs:
#   - data/raw/*.xlsx
#   - data/preprocessed/*.xlsx
#
# Called by:
#   - upload_dbtables
#   - ./app.R
#   - scripts/GP2_How_did_players_spend_their_money_example.R
# ------------------------------------------------------------

export_excel <- function(sessiontable_list, session_name, preprocessed = TRUE) {
  
  ## Generate timestamp to append to excel file name
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  ## Check Default variables exists, are character with length > 0 or = 1.
  stopifnot("Default variable PREPROCESSED_DBTABLES not found in R/constants.R" = exists(deparse(substitute(PREPROCESSED_DBTABLES))),
            "PREPROCESSED_DBTABLES expected to be character" = is.character(PREPROCESSED_DBTABLES),
            "PREPROCESSED_DBTABLES expected to have length > 0" = length(PREPROCESSED_DBTABLES) > 0,
            
            "Default variable PREPRDATA_PATH not found in R/constants.R" = exists(deparse(substitute(PREPRDATA_PATH))),
            "PREPRDATA_PATH expected to be character" = is.character(PREPROCESSED_DBTABLES),
            "PREPRDATA_PATH expected to have only element 1" = length(PREPRDATA_PATH) == 1,
            
            "Default variable RAWDATA_PATH not found in R/constants.R" = exists(deparse(substitute(RAWDATA_PATH))),
            "PREPRDATA_PATH expected to be character" = is.character(RAWDATA_PATH),
            "PREPRDATA_PATH expected to have only element 1" = length(RAWDATA_PATH) == 1)
  
  # check sessiontable_list is named list containing data.frames only
  stopifnot(
    "sessiontable_list expected to be list" = is.list(sessiontable_list),
    "sessiontable_list expected to have length > 0" = length(sessiontable_list) > 0,
    "sessiontable_list expected to contain data.frames only" = all(vapply(sessiontable_list, is.data.frame, logical(1))),
    "sessiontable_list should have names" = !is.null(names(sessiontable_list)),
    "sessiontable_list should have names" = all(names(sessiontable_list) != ""),
    "sessiontable_list should have names" = !any(is.na(names(sessiontable_list)))
  )
  
  # check session_name is a single character
  stopifnot(
    "PREPRDATA_PATH expected to be character" = is.character(session_name),
    "PREPRDATA_PATH expected to have only element 1" = length(session_name) == 1,
  )
  
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
               here::here(file.path(excel_outpath,
                              paste0(paste(session_name, timestamp, workflow_stage, sep = "-"), ".xlsx"))))
    
    message("File written successfully.")
    
  }, error = function(e) {
    
    message("Error: ", e$message)
  })
}


# ------------------------------------------------------------
# Function: upload_dbtables
# Purpose: Retrieve tables as dataframes stored inside a named list
#
# Params:
#   - folder_path (character): path to main folder expected to contain subfolders
#   - subfolder_pattern (character): pattern to filter subfolders whose names include the given string
#   - dbtable_selection (character): record of dbtable names that should be kept for preprocessing. Assumed to be saved in SELECTED_DBTABLES
#   - excel (logical): assumes excel versions of the tables are not exported, otherwise exported_excel is executed
#
# Returns:
#   - 2-layer list containing dbtable content in dataframe format
#
# Called by:
#   - ./app.R
#   - scripts/GP2_How_did_players_spend_their_money_example.R
# ------------------------------------------------------------

upload_dbtables <- function(folder_path, subfolder_pattern, dbtable_selection = SELECTED_DBTABLES, excel = FALSE) {
  
  ## Retrieve table files inside subfolders of folder_path whose names match subfolder_pattern
  dbtable_filenames <- list_matching_dbtables(folder_path, subfolder_pattern)
  
  ## Check SELECTED_DBTABLES is non-empty character
  stopifnot("SELECTED_DBTABLES expected to be character" = is.character(SELECTED_DBTABLES),
            "SELECTED_DBTABLES expected to have length > 0" = length(SELECTED_DBTABLES) > 0)
  
  stopifnot("SELECTED_DBTABLES should not contain blank character entries" = contains_blank_char(SELECTED_DBTABLES) == FALSE)
  
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
    
    ## Name listed tables after their filename without extension
    names(dbtables_list[[subfolder]]) <- tools::file_path_sans_ext(basename(dbtable_filenames[[subfolder]]))
    
    ## If logical argument asking for table selection based on SELECTED_DBTABLES is true
    dbtables_list[[subfolder]] <- dbtables_list[[subfolder]][names(dbtables_list[[subfolder]]) %in% dbtable_selection == TRUE]

    ## If logical argument asking for excel export of named list is true
    if(excel) {
      export_excel(dbtables_list[[subfolder]], subfolder, preprocessed = FALSE)
    }
  }
  
  return(dbtables_list) 
}