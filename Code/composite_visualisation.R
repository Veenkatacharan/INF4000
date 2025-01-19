# Create the bar chart: Visualizes overall category coverage
bar_chart <- ggplot(category_summary_aggregated, aes(x = reorder(category_level_1, article_count), y = article_count, fill = category_level_1)) +
  geom_bar(stat = "identity") +  # Bar chart showing total articles per category
  coord_flip() +  # Flip the axes for better readability
  scale_fill_manual(values = c(  # Define specific colors for each category
    "sport" = "#1f78b4",
    "society" = "#33a02c",
    "politics" = "#e31a1c",
    "health" = "#ff7f00",
    "environment" = "#6a3d9a",
    "science and technology" = "#b15928",
    "economy, business and emergency incident" = "#a6cee3",
    "crime, law and justice" = "#fb9a99",
    "arts, culture, entertainment and media" = "#fdbf6f",
    "disaster, accident and emergency incident" = "#cab2d6",
    "education" = "#ffff99",
    "religion and belief" = "#1f78b4",
    "conflict, war and peace" = "#b2df8a",
    "weather" = "#33a02c"
  )) +
  labs(title = "Category Coverage by News Source",  # Title for the bar chart
       x = "Category",  # Label for x-axis
       y = "Number of Articles") +  # Label for y-axis
  theme_minimal() +  # Use a minimal theme for clean visuals
  theme(legend.position = "none")  # Hide the legend (categories are labeled on the bars)

# Create the pie chart: Shows the proportional distribution of articles across categories
pie_chart <- ggplot(category_proportions, aes(x = "", y = article_count, fill = category_level_1)) +
  geom_bar(stat = "identity", width = 1) +  # Create a bar chart before converting to polar
  coord_polar("y", start = 0) +  # Convert to a circular pie chart
  labs(title = "Proportion of Articles by Category",  # Title for the pie chart
       x = NULL,  # Remove x-axis label
       y = NULL) +  # Remove y-axis label
  theme_void() +  # Use a void theme for a clean look
  scale_fill_brewer(palette = "Set3")  # Use a color palette for better category distinction

# Create the grouped bar chart: Compares most and least covered categories across selected news sources
grouped_bar_chart <- ggplot(selected_data_long, aes(x = source, fill = coverage_type)) +
  geom_bar(aes(y = ifelse(coverage_type == "most_covered", max_count, -min_count)),
           stat = "identity", position = "dodge") +  # Stack most and least covered categories
  scale_y_continuous(labels = abs) +  # Display absolute values for negative counts
  labs(title = "Most and Least Covered Categories by Selected News Sources",  # Title
       x = "News Source",  # x-axis label
       y = "Number of Articles",  # y-axis label
       fill = "Coverage Type") +  # Legend title
  theme_minimal() +  # Minimal theme for clean visuals
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create the time series plot: Visualizes how category coverage changes over time
time_series_plot <- ggplot(time_series_data, aes(x = as.Date(paste0(month, "-01")), y = article_count, color = category_level_1)) +
  geom_line(linewidth = 1) +  # Line plot showing trends over time
  labs(title = "Time Series Trend of Article Counts by Category",  # Title
       x = "Date",  # x-axis label
       y = "Number of Articles",  # y-axis label
       color = "Category") +  # Legend title
  theme_minimal() +  # Minimal theme for clean visuals
  theme(legend.position = "right")  # Position the legend on the right

# Combine the four saved graphs into a composite visualisation
composite_visualisation <- (
  (bar_chart | pie_chart) /        # Row 1: Bar chart and Pie chart side by side
    (grouped_bar_chart | time_series_plot)  # Row 2: Grouped bar chart and Time series side by side
)

# Add a title and subtitle for the composite visualisation
composite_visualisation <- composite_visualisation +
  plot_annotation(
    title = "Composite Visualisation of News Coverage Trends",  # Main title
    subtitle = "Insights into category distribution, source-level analysis, and temporal trends",  # Subtitle
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),  # Title styling
      plot.subtitle = element_text(size = 12, margin = margin(b = 10))  # Subtitle styling
    )
  )

# Display the composite visualisation
print(composite_visualisation)

# Save the composite visualisation as a PNG file
ggsave("composite_visualisation.png", composite_visualisation, width = 14, height = 12)