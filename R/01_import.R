import_csvs <- function(file_info) {
  
  datasets <- lapply(
    file_info$datapath,
    read.csv,
    stringsAsFactors = FALSE
  )
  
  names(datasets) <- file_info$name
  
  datasets
  
}