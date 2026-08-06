prepare_datasets <- function(file_info) {
  
  datasets <- import_csvs(file_info)
  
  datasets <- lapply(
    datasets,
    standardise_names
  )
  
  datasets <- lapply(
    datasets,
    clean_values
  )
  
  datasets
  
}