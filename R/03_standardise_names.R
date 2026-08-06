standardise_names <- function(data) {
  
  # Standardise formatting
  names(data) <- janitor::make_clean_names(names(data))
  
  # Dictionary of common synonyms
  name_dictionary <- c(
    cust_id = "customer_id",
    customerid = "customer_id",
    customer_number = "customer_id",
    customer_no = "customer_id",
    
    dob = "date_of_birth",
    birth_date = "date_of_birth",
    date_of_birth = "date_of_birth",
    
    postcode = "postcode",
    post_code = "postcode",
    zip_code = "postcode",
    
    firstname = "first_name",
    first_name = "first_name",
    
    lastname = "last_name",
    surname = "last_name",
    last_name = "last_name"
  )
  
  # Apply dictionary
  names(data) <- ifelse(
    names(data) %in% names(name_dictionary),
    name_dictionary[names(data)],
    names(data)
  )
  
  data
  
}