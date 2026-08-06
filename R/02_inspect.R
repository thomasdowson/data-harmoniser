inspect_dataset <- function(data) {
  
  data.frame(
    Metric = c(
      "Rows",
      "Columns",
      "Missing values",
      "Duplicate rows"
    ),
    
    Value = c(
      nrow(data),
      ncol(data),
      sum(is.na(data)),
      sum(duplicated(data))
    ),
    
    stringsAsFactors = FALSE
  )
  
}