combine_csvs_to_excel <- function(folder_path = "local path", folder_pattern = "csv_folder", output_path = file.path("data", "combined-tables"), output_prefix = "Dataset_") {
  
  # Read all tables in the folder with the custom function
  csv_reading <- read_all_csvs(folder_path, folder_name, output_datalist = FALSE)
  target_folder <- csv_reading$foldername
  csv_files <- csv_reading$filenames
  
  # Create workbook
  wb <- createWorkbook()
  
  for (csv_file in csv_files) {
    df <- read_csv(csv_file)
    sheet_name <- tools::file_path_sans_ext(basename(csv_file))
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  
  # Create dynamic output filename
  folder_base <- basename(target_folder)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  #output_filename <- paste0(output_prefix, "_", folder_base, "_", timestamp, ".xlsx")
  output_filename <- paste0(output_prefix, "_", folder_base, ".xlsx")
  #data_output_dir <- paste0(script_dir,"/data_output")
  output_path <- file.path(output_path, output_filename)
  
  # Save workbook
  saveWorkbook(wb, output_path, overwrite = TRUE)
  message("Excel file created at: ", output_path)
}
