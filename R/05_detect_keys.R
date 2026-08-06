detect_keys <- function(datasets) {
  
  dataset_names <- names(datasets)
  
  candidates <- data.frame()
  
  for (i in 1:(length(datasets) - 1)) {
    
    for (j in (i + 1):length(datasets)) {
      
      data_1 <- datasets[[i]]
      data_2 <- datasets[[j]]
      
      common_columns <- intersect(
        names(data_1),
        names(data_2)
      )
      
      if (length(common_columns) == 0)
        next
      
      for (column in common_columns) {
        
        candidates <- rbind(
          candidates,
          data.frame(
            dataset_1 = dataset_names[i],
            dataset_2 = dataset_names[j],
            column = column,
            type_1 = class(data_1[[column]])[1],
            type_2 = class(data_2[[column]])[1],
            uniqueness_1 =
              dplyr::n_distinct(data_1[[column]], na.rm = TRUE) /
              sum(!is.na(data_1[[column]])),
            uniqueness_2 =
              dplyr::n_distinct(data_2[[column]], na.rm = TRUE) /
              sum(!is.na(data_2[[column]])),
            stringsAsFactors = FALSE
          )
        )
        
      }
      
    }
    
  }
  
  candidates
  
}