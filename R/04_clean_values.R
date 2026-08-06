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
      
      # ---------- Date detection ----------
      
      parsed_dates <- suppressWarnings(
        lubridate::parse_date_time(
          column,
          orders = c(
            "ymd",
            "dmy",
            "Ymd",
            "dmY",
            "d b Y",
            "d B Y",
            "d-b-Y",
            "d/m/Y"
          )
        )
      )
      
      # Only convert if most non-missing values were recognised as dates
      non_missing <- !is.na(column)
      
      if (sum(non_missing) > 0) {
        
        proportion_dates <-
          sum(!is.na(parsed_dates[non_missing])) /
          sum(non_missing)
        
        if (proportion_dates >= 0.8) {
          
          column <- ifelse(
            is.na(parsed_dates),
            NA,
            format(as.Date(parsed_dates), "%Y-%m-%d")
          )
          
        }
        
      }
      
    }
    
    column
    
  })
  
  data
  
}