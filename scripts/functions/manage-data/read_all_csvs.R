read_all_csvs <- function(folder_path = "local path", folder_pattern = "csv_folder", output_datalist = TRUE) {

  # List all subfolders inside the main folder
  folders <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
  
  output <- list()
  
  # Find the one that matches your pattern
  output$foldername <- folders[grepl(folder_pattern, basename(folders))][1]
  
  # Check result
  if (is.na(output$foldername)) {
    stop(paste("No folder matching pattern '", folder_pattern, "' found.", sep = ""))
  } else {
    print(paste("check_folder_exists:", folder_pattern, "found."))
  }
  
  # List CSV files
  output$filenames <- list.files(path = output$foldername, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(output$filenames) == 0) {
    stop("No CSV files found in the target folder.")
  }
  
  # Read all CSVs into a named list
  output$datalist <- FALSE
  
  if (output_datalist == TRUE){
    output$datalist <- lapply(output$filenames, readr::read_csv)
    names(output$datalist) <- tools::file_path_sans_ext(basename(output$filenames))
  }
  
  return(output) 
}