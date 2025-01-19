# Install and load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(dplyr)
library(stringr)

# Load the original dataset
original_file <- "path/to/MN-DS-news-classification.csv"
data <- read.csv(original_file, encoding = "latin1", stringsAsFactors = FALSE)

# Step 1: Retain only relevant columns
cleaned_data <- data %>%
  select(id, date, source, title, content, author, url, category_level_1, category_level_2)

# Step 2: Handle missing values
cleaned_data <- cleaned_data %>%
  filter(!is.na(content), !is.na(category_level_1), !is.na(category_level_2))

# Step 3: Clean the `date` column
cleaned_data <- cleaned_data %>%
  mutate(
    date = str_trim(date),  # Remove leading and trailing whitespaces
    date_cleaned = str_replace_all(date, "[^0-9/-]", ""),  # Remove non-date characters
    date_cleaned = ifelse(nchar(date_cleaned) > 10, NA, date_cleaned),  # Handle invalid lengths
    date_parsed = as.Date(date_cleaned, format = "%d/%m/%Y")  # Convert to Date format
  ) %>%
  filter(!is.na(date_parsed))  # Remove rows where the date couldn't be parsed

# Step 4: Clean text fields
cleaned_data <- cleaned_data %>%
  mutate(
    title = str_trim(title),
    content = str_trim(content),
    author = ifelse(is.na(author) | author == "", "Unknown", author)  # Replace missing author
  )

# Step 5: Select final columns in the desired order
cleaned_data <- cleaned_data %>%
  select(
    id, date_parsed, source, title, content, author, url, 
    category_level_1, category_level_2
  ) %>%
  rename(date = date_parsed)  # Rename `date_parsed` back to `date`

# Step 6: Save the cleaned dataset
output_file <- "Filtered_News_Dataset.csv"
write.csv(cleaned_data, output_file, row.names = FALSE)

summary(cleaned_data)