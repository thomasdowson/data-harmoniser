join_data <- function(
    left_data,
    right_data,
    by,
    join = "left"
) {
  
  if (join == "left") {
    
    result <- dplyr::left_join(
      left_data,
      right_data,
      by = by
    )
    
  } else if (join == "inner") {
    
    result <- dplyr::inner_join(
      left_data,
      right_data,
      by = by
    )
    
  } else if (join == "right") {
    
    result <- dplyr::right_join(
      left_data,
      right_data,
      by = by
    )
    
  } else if (join == "full") {
    
    result <- dplyr::full_join(
      left_data,
      right_data,
      by = by
    )
    
  } else {
    
    stop("Unknown join type.")
    
  }
  
  result
  
}