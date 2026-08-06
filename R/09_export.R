export_data <- function(
    data,
    path,
    format = "csv"
) {
  
  if (format == "csv") {
    
    write.csv(
      data,
      path,
      row.names = FALSE
    )
    
  } else if (format == "rds") {
    
    saveRDS(
      data,
      path
    )
    
  } else {
    
    stop("Unsupported export format.")
    
  }
  
}