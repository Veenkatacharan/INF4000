# Load necessary libraries
library(dplyr)
library(ggplot2)

news_data <- read.csv("path/to/ Filtered_News_Dataset.csv ")

# Define valid categories
valid_categories <- c("sport", "society", "politics", "health", 
                      "science and technology", "environment", 
                      "economy, business and emergency incident", 
                      "crime, law and justice", 
                      "arts, culture, entertainment and media", 
                      "disaster, accident and emergency incident", 
                      "education", "religion and belief", 
                      "conflict, war and peace", "weather")

# Clean and filter the dataset
news_data <- news_data %>%
  mutate(category_level_1 = tolower(trimws(category_level_1))) %>% # Standardize categories
  filter(
    !is.na(category_level_1) &               # Remove NA values
      category_level_1 %in% valid_categories   # Retain only valid categories
  )

# Create the summary table
category_summary <- news_data %>%
  group_by(source, category_level_1) %>%
  summarise(article_count = n(), .groups = "drop") %>%
  arrange(desc(article_count))

# Print the summary table
print(category_summary)

# Create the most and least covered categories table
most_least_categories <- category_summary %>%
  group_by(source) %>%
  summarise(
    most_covered = category_level_1[which.max(article_count)],
    least_covered = category_level_1[which.min(article_count)],
    max_count = max(article_count),
    min_count = min(article_count)
  )

# Print the most and least covered categories table
print(most_least_categories)

# Aggregate data by category for visualization
category_summary_aggregated <- news_data %>%
  group_by(category_level_1) %>%
  summarise(article_count = n(), .groups = "drop") %>%
  arrange(desc(article_count))
