
import_csvs <- function(file_info) {
  
  datasets <- lapply(
    file_info$datapath,
    readr::read_csv,
    show_col_types = FALSE
  )
  
  names(datasets) <- file_info$name
  
  datasets
  
}