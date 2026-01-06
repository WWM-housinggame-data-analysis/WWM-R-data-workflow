TABLE_TYPE <- ".csv"

list_all_subfolders <- function(folder_path, subfolder_pattern) {

  # List all subfolders inside the main folder
  subfolders <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
  
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
    
    dbtable_filenames[[i]] <- list.files(path = subfolder, pattern = paste("\\", TABLE_TYPE, "$", sep = ""), full.names = TRUE)
    
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

 
# Retrieve all tables into a named list
retrieve_all_dbtables <- function(folder_path, subfolder_pattern) {
  
  dbtable_filenames <- list_all_dbtables(folder_path, subfolder_pattern)

  dbtables <- dbtable_filenames
  
  for (subfolder in names(dbtable_filenames)) {
    dbtables[[subfolder]] <- lapply(dbtable_filenames[[subfolder]], readr::read_csv)
    
    names(dbtables[[subfolder]]) <- tools::file_path_sans_ext(basename(dbtable_filenames[[subfolder]]))
  }
  
  return(dbtables) 
}