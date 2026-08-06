clean_values <- function(data) {
  
  # Common missing value representations
  missing_values <- c(
    "",
    " ",
    "NA",
    "N/A",
    "NULL",
    "null",
    "Null",
    "Unknown",
    "unknown",
    "."
  )
  
  data[] <- lapply(data, function(column) {
    
    if (is.character(column)) {
      
      # Remove leading/trailing whitespace
      column <- trimws(column)
      
      # Standardise missing values
      column[column %in% missing_values] <- NA
      
      # Standardise logical values
      column[tolower(column) %in% c("yes", "y", "true", "1")] <- "TRUE"
      column[tolower(column) %in% c("no", "n", "false", "0")] <- "FALSE"
      
    }
    
    column
    
  })
  
  data
  
}