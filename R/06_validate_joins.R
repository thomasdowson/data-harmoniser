validate_join <- function(
    left_data,
    right_data,
    by
) {
  
  if (!(by %in% names(left_data))) {
    stop("Join key not found in left dataset.")
  }
  
  if (!(by %in% names(right_data))) {
    stop("Join key not found in right dataset.")
  }
  
  left_key <- left_data[[by]]
  right_key <- right_data[[by]]
  
  compatible_types <-
    
    (is.numeric(left_key) && is.numeric(right_key)) ||
    
    (is.character(left_key) && is.character(right_key)) ||
    
    (is.logical(left_key) && is.logical(right_key))
  
  data.frame(
    
    Check = c(
      "Compatible data types",
      "Duplicate keys (left)",
      "Duplicate keys (right)",
      "Missing keys (left)",
      "Missing keys (right)",
      "Common key values (%)"
    ),
    
    Result = c(
      
      compatible_types,
      
      sum(duplicated(left_key)),
      
      sum(duplicated(right_key)),
      
      sum(is.na(left_key)),
      
      sum(is.na(right_key)),
      
      round(
        100 *
          length(intersect(
            unique(left_key),
            unique(right_key)
          )) /
          length(unique(left_key)),
        1
      )
      
    ),
    
    stringsAsFactors = FALSE
    
  )
  
}