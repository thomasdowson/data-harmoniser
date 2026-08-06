customers <- data.frame(
  CustomerID = c(1001,1002,1003,1004,1005),
  
  FirstName = c(
    " John ",
    "Alice",
    "Bob",
    "Sarah",
    "Tom"
  ),
  
  Surname = c(
    "Smith",
    "Jones",
    "Brown",
    "Wilson",
    "Taylor"
  ),
  
  DOB = c(
    "01/01/1990",
    "1992-04-13",
    "Unknown",
    "",
    "14-07-1988"
  ),
  
  stringsAsFactors = FALSE
)


orders <- data.frame(
  
  customer_id = c(
    1001,
    1001,
    1002,
    1005,
    9999
  ),
  
  OrderDate = c(
    "2025-01-01",
    "01/02/2025",
    "",
    "Unknown",
    "2025-05-01"
  ),
  
  OrderValue = c(
    120,
    450,
    80,
    999,
    50
  ),
  
  stringsAsFactors = FALSE
)

addresses <- data.frame(
  
  Customer_No = c(
    1001,
    1002,
    1003,
    1004
  ),
  
  PostCode = c(
    "LN1 1AA",
    "LN2 3BB",
    "",
    "Unknown"
  ),
  
  Region = c(
    "East Midlands",
    "East Midlands",
    "Yorkshire",
    "Yorkshire"
  ),
  
  stringsAsFactors = FALSE
)


write.csv(customers, "customers.csv", row.names = FALSE)
write.csv(orders, "orders.csv", row.names = FALSE)
write.csv(addresses, "addresses.csv", row.names = FALSE)