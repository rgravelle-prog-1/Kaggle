# Get the current working directory
current_directory <- getwd()

# Set the working directory to the present working directory
setwd(current_directory)

# Install packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
  
# Load the required libraries
library(dplyr)

# Load the training data
train_data <- read.csv("train.csv")

# Load the test data
test_data <- read.csv("test.csv")

# Load store metadata
stores_data <- read.csv("stores.csv")

# Load daily oil price
oil_data <- read.csv("oil.csv")

# Load holidays and events data
holidays_data <- read.csv("holidays_events.csv")

# Display the first few rows of each dataset to inspect the data
cat("Training Data:\n")
head(train_data)

cat("\nTest Data:\n")
head(test_data)

cat("\nStore Metadata:\n")
head(stores_data)

cat("\nOil Data:\n")
head(oil_data)

cat("\nHolidays and Events Data:\n")
head(holidays_data)


# NOw lets start with getting the correlation between the oil price and sales

# Assuming 'train_data' is your training data and 'oil_data' is your oil price data

# Merge data based on the 'date' column
merged_data <- merge(train_data, oil_data, by = "date", all.x = TRUE)

# Drop missing data
merged_data <- na.omit(merged_data)

# Plotting using ggplot2
library(ggplot2)

# Create a plot
ggplot(merged_data, aes(x = date)) +
  geom_line(aes(y = sales, color = "Sales"), size = 1.5) +
  geom_line(aes(y = dcoilwtico, color = "Oil Price"), size = 1.5) +
  scale_color_manual(values = c("Sales" = "orange", "Oil Price" = "blue")) +
  labs(title = "Sales and Oil Price Over Time",
       x = "Date",
       y = "Values") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Oil Price (dcoilwtico)"))

# Assuming 'merged_data' is your merged DataFrame

# Load the ggplot2 library
library(ggplot2)

# Create a scatter plot
ggplot(merged_data, aes(x = dcoilwtico, y = sales)) +
  geom_point() +
  labs(title = "Oil Price vs. Sales",
       x = "Oil Price (dcoilwtico)",
       y = "Sales") +
  theme_minimal()

# Scatter plot to visualize the relationship between promotion and sales 

# Load the ggplot2 library
library(ggplot2)

# Create a scatter plot with relevence to Promotion
ggplot(train_data, aes(x = onpromotion, y = sales)) +
  geom_point(alpha = 0.7, color = "#3498db") +  # Custom color for points
  labs(title = "Scatter Plot of Sales vs. Promotion",
       x = "Promotion Status",
       y = "Sales") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major = element_line(color = "lightgray"),  
        panel.background = element_rect(fill = "white"),  
        axis.line = element_line(color = "black")) 

# Bar chart for sales by holiday status
ggplot(train_data, aes(x = as.factor(onpromotion), y = sales, fill = as.factor(onpromotion))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Sales by Promotion Status",
       x = "Promotion Status",
       y = "Average Sales") +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "orange")) +
  theme_minimal()

# Bar chart for sales by family
ggplot(train_data, aes(x = family, y = sales, fill = family)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Sales by Family",
       x = "Family",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Bar chart for sales by date
ggplot(train_data, aes(x = date, y = sales, fill = date)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Sales by Date",
       x = "Date",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Merge data based on the 'store_nbr' column
merged_data <- merge(train_data, stores_data, by = "store_nbr", all.x = TRUE)

# Check the structure of merged_data
str(merged_data)

# Bar chart for sales by store type
ggplot(merged_data, aes(x = type, y = sales, fill = type)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Sales by Store Type",
       x = "Store Type",
       y = "Average Sales") +
  theme_minimal()


# Bar chart for sales by individual store
ggplot(train_data, aes(x = as.factor(store_nbr), y = sales, fill = as.factor(store_nbr))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Sales by Individual Store",
       x = "Store Number",
       y = "Average Sales") +
  theme_minimal()


