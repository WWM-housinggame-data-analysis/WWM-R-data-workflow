# Set all default variables or global options and all the path variables at the top of the code.

FUNCTION_PATH <- file.path("R")

TABLE_TYPE <- ".csv"

WORKFLOW_STAGES <- c("raw", "preprocessed")
names(WORKFLOW_STAGES) <- c(RAWDATA_PATH, PREPRDATA_PATH)

# Load required functions
source(here(file.path(FUNCTION_PATH, "constants.R")))


list_all_subfolders <- function(folder_path, subfolder_pattern) {

  # List all subfolders inside the main folder
  subfolders <- list.dirs(path = here(folder_path), full.names = TRUE, recursive = FALSE)
  
  # Filter subfolders that matches your pattern
  subfolders <- subfolders[grepl(subfolder_pattern, basename(subfolders))]
  
  # Check subfolders found
  if (length(subfolders) == 0) {
    stop(paste("No subfolder matching pattern `", subfolder_pattern, "` found.", sep = ""))
  } else {
    for (subfolder in subfolders) {
      print(paste("Subfolder `", subfolder, "` found.", sep = ""))
    }
  }
  
  return(subfolders)
}

# List CSV files
list_all_dbtables <- function(folder_path, subfolder_pattern) {
  
  subfolders <- list_all_subfolders(folder_path, subfolder_pattern)
  
  dbtable_filenames <- list()
  
  for (subfolder in subfolders) {
    
    i <- length(dbtable_filenames) + 1
    
    dbtable_filenames[[i]] <- list.files(path = here(subfolder), pattern = paste("\\", TABLE_TYPE, "$", sep = ""), full.names = TRUE)
    
    names(dbtable_filenames)[i] <- subfolder
    
    if (length(dbtable_filenames[[i]]) == 0) {
      dbtable_filenames[[i]] <- NA
      warning(paste("No ", TABLE_TYPE, "files found in the target subfolder ", subfolder, sep = ""))
    }
  }
  
  if (all(is.na(dbtable_filenames) == TRUE)) {
    stop(paste("No ", TABLE_TYPE, " files found in any target subfolder.", sep = ""))
  }
  
  return(dbtable_filenames)
}

 
# Write to Excel with sheet names matching table names

export_excel <- function(sessiontable_list, session_name, preprocessed = TRUE) {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if (preprocessed == TRUE && "income_dist_df" %in% names(sessiontable_list) == TRUE) {
    
    parent_path <- PREPRDATA_PATH
    
  } else if (preprocessed == TRUE && "income_dist_df" %in% names(sessiontable_list) == FALSE) {
    
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
  
  dbtable_filenames <- list_all_dbtables(folder_path, subfolder_pattern)
  
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