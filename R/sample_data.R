# ============================================================
# Data Harmoniser - Reproducible Messy Sample Data
# ============================================================

set.seed(123)

# ------------------------------------------------------------
# Output folder
# ------------------------------------------------------------

output_dir <- "sample_data"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

random_missing <- function(x, proportion = 0.05) {
  n_missing <- max(1, floor(length(x) * proportion))
  x[sample(seq_along(x), n_missing)] <- sample(
    c("", "Unknown", "NULL", "N/A", "."),
    n_missing,
    replace = TRUE
  )
  x
}

messy_text <- function(x, proportion = 0.15) {
  n_change <- max(1, floor(length(x) * proportion))
  idx <- sample(seq_along(x), n_change)
  
  for (i in idx) {
    x[i] <- sample(
      c(
        x[i],
        toupper(x[i]),
        tolower(x[i]),
        paste0(" ", x[i]),
        paste0(x[i], " ")
      ),
      1
    )
  }
  
  x
}

format_mixed_date <- function(x) {
  formats <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%d %b %Y",
    "%d-%b-%Y",
    "%Y/%m/%d"
  )
  
  vapply(
    seq_along(x),
    function(i) {
      format(x[i], sample(formats, 1))
    },
    character(1)
  )
}

format_messy_postcode <- function(x) {
  vapply(
    x,
    function(value) {
      compact <- gsub(" ", "", value)
      
      sample(
        c(
          value,
          tolower(value),
          compact,
          gsub(" ", "-", value),
          sub(" ", "  ", value)
        ),
        1
      )
    },
    character(1)
  )
}

# ------------------------------------------------------------
# Reference values
# ------------------------------------------------------------

first_names <- c(
  "John", "Alice", "Bob", "Sarah", "Tom", "Emma", "James",
  "Olivia", "Daniel", "Sophie", "Michael", "Lucy", "David",
  "Amelia", "Thomas", "Grace", "Jack", "Emily", "George", "Ella"
)

surnames <- c(
  "Smith", "Jones", "Brown", "Wilson", "Taylor", "Davies",
  "Evans", "Thomas", "Johnson", "Roberts", "Walker", "Wright",
  "Thompson", "White", "Hughes", "Edwards", "Green", "Hall",
  "Lewis", "Harris"
)

products <- c(
  "Laptop", "Monitor", "Keyboard", "Mouse", "Desk", "Chair",
  "Phone", "Tablet", "Printer", "Headphones", "Webcam", "Router"
)

towns <- c(
  "Lincoln", "Gainsborough", "Sheffield", "Derby", "Nottingham",
  "Leeds", "York", "Doncaster", "Newark", "Grantham"
)

regions <- c(
  "East Midlands",
  "Yorkshire",
  "West Midlands",
  "North East",
  "North West"
)

counties <- c(
  "Lincolnshire",
  "South Yorkshire",
  "Derbyshire",
  "Nottinghamshire",
  "North Yorkshire"
)

streets <- c(
  "High Street", "Church Lane", "Station Road", "Mill Road",
  "King Street", "Queen Street", "Park Avenue", "Main Street",
  "Victoria Road", "Market Place"
)

order_statuses <- c(
  "Complete", "Pending", "Cancelled", "Refunded"
)

# ------------------------------------------------------------
# customers.csv
# ------------------------------------------------------------

n_customers <- 250

customer_ids <- 1001:(1000 + n_customers)

customer_first_names <- sample(
  first_names,
  n_customers,
  replace = TRUE
)

customer_surnames <- sample(
  surnames,
  n_customers,
  replace = TRUE
)

dates_of_birth <- as.Date("1950-01-01") +
  sample(
    0:20000,
    n_customers,
    replace = TRUE
  )

customer_emails <- paste0(
  tolower(customer_first_names),
  ".",
  tolower(customer_surnames),
  customer_ids,
  "@example.com"
)

customer_phones <- paste0(
  "07",
  sprintf(
    "%09d",
    sample(
      0:999999999,
      n_customers,
      replace = TRUE
    )
  )
)

customers <- data.frame(
  CustomerID = customer_ids,
  FirstName = messy_text(customer_first_names, 0.20),
  Surname = messy_text(customer_surnames, 0.15),
  DOB = format_mixed_date(dates_of_birth),
  Email = messy_text(customer_emails, 0.12),
  PhoneNumber = customer_phones,
  Active = sample(
    c("Yes", "No", "TRUE", "FALSE", "Y", "N", "1", "0"),
    n_customers,
    replace = TRUE
  ),
  stringsAsFactors = FALSE
)

customers$DOB <- random_missing(customers$DOB, 0.08)
customers$Email <- random_missing(customers$Email, 0.05)
customers$PhoneNumber <- random_missing(customers$PhoneNumber, 0.06)

# Add a few duplicated customer records
duplicate_customers <- customers[
  sample(seq_len(nrow(customers)), 5),
]

customers <- rbind(
  customers,
  duplicate_customers
)

# ------------------------------------------------------------
# orders.csv
# ------------------------------------------------------------

n_orders <- 750

known_customer_ids <- sample(
  customer_ids,
  size = round(n_orders * 0.90),
  replace = TRUE
)

unknown_customer_ids <- sample(
  9000:9999,
  size = n_orders - length(known_customer_ids),
  replace = TRUE
)

order_customer_ids <- sample(
  c(known_customer_ids, unknown_customer_ids)
)

order_dates <- as.Date("2024-01-01") +
  sample(
    0:900,
    n_orders,
    replace = TRUE
  )

orders <- data.frame(
  customer_id = order_customer_ids,
  OrderDate = format_mixed_date(order_dates),
  Product = messy_text(
    sample(products, n_orders, replace = TRUE),
    0.12
  ),
  Quantity = sample(1:6, n_orders, replace = TRUE),
  OrderValue = round(
    runif(n_orders, 10, 1500),
    2
  ),
  Discount = sample(
    c("0", "5%", "10%", "0.15", "20", "", "N/A"),
    n_orders,
    replace = TRUE
  ),
  Status = messy_text(
    sample(order_statuses, n_orders, replace = TRUE),
    0.20
  ),
  stringsAsFactors = FALSE
)

orders$OrderDate <- random_missing(orders$OrderDate, 0.05)
orders$Product <- random_missing(orders$Product, 0.03)

# Add a few duplicated order rows
duplicate_orders <- orders[
  sample(seq_len(nrow(orders)), 12),
]

orders <- rbind(
  orders,
  duplicate_orders
)

# ------------------------------------------------------------
# addresses.csv
# ------------------------------------------------------------

n_addresses <- 275

address_customer_ids <- sample(
  customer_ids,
  n_addresses,
  replace = TRUE
)

postcode_areas <- c(
  "LN1 1AA", "LN2 3BB", "S1 2AB", "DE1 3CD", "NG1 4EF",
  "LS1 5GH", "YO1 6JK", "DN1 7LM", "NG24 8NP", "NG31 9QR"
)

addresses <- data.frame(
  Customer_No = address_customer_ids,
  HouseNumber = sample(
    c(
      as.character(1:200),
      "14A", "22B", "7 A", "101C"
    ),
    n_addresses,
    replace = TRUE
  ),
  Street = messy_text(
    sample(streets, n_addresses, replace = TRUE),
    0.15
  ),
  Town = messy_text(
    sample(towns, n_addresses, replace = TRUE),
    0.15
  ),
  County = messy_text(
    sample(counties, n_addresses, replace = TRUE),
    0.20
  ),
  PostCode = format_messy_postcode(
    sample(postcode_areas, n_addresses, replace = TRUE)
  ),
  Region = messy_text(
    sample(regions, n_addresses, replace = TRUE),
    0.15
  ),
  stringsAsFactors = FALSE
)

addresses$PostCode <- random_missing(addresses$PostCode, 0.10)
addresses$County <- random_missing(addresses$County, 0.05)

# ------------------------------------------------------------
# Write CSV files
# ------------------------------------------------------------

write.csv(
  customers,
  file.path(output_dir, "customers.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  orders,
  file.path(output_dir, "orders.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  addresses,
  file.path(output_dir, "addresses.csv"),
  row.names = FALSE,
  na = ""
)

cat(
  paste0(
    "\nSample data created successfully in '",
    output_dir,
    "/'\n\n",
    "customers.csv: ",
    nrow(customers),
    " rows\n",
    "orders.csv: ",
    nrow(orders),
    " rows\n",
    "addresses.csv: ",
    nrow(addresses),
    " rows\n"
  )
)