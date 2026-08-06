customers <- data.frame(
  CustomerID = 1:5,
  Name = c("John", "Alice", "Bob", "Sarah", "Tom")
)

orders <- data.frame(
  CustomerID = c(1, 1, 2, 4),
  Order = c("TV", "Laptop", "Phone", "Table")
)

write.csv(customers, "customers.csv", row.names = FALSE)
write.csv(orders, "orders.csv", row.names = FALSE)