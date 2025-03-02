###########################################################################
# Modern Super Bowl Analysis
#
# Title: Modern Super Bowl Analysis: Trends in TV Ratings, Ad Costs, and Halftime 
#        Performances (Super Bowls from 1987)
#
# Description:
#   This project analyzes Super Bowl data from modern times (Super Bowl from 1987)
#   to understand trends in game competitiveness, TV viewership, advertisement
#   costs, and halftime performances. The analysis examines:
#     1. The relationship between point differences and viewership.
#     2. Trends in average US viewers, household ratings, and ad costs.
#     3. How halftime show performance (number of songs, solo vs. multiple
#        artists) correlates with TV ratings.
#
# Data Sources:
#   - super_bowls.csv: Super Bowl game data (including point differences)
#   - tv.csv: TV ratings, viewership, and advertisement costs
#   - halftime_musicians.csv: Halftime show data (musician and number of songs)
#
# Note: We focus on modern Super Bowls (super_bowl from 1987) to exclude earlier, 
# less relevant games.
###########################################################################

# ---------------------------
# Load Required Packages
# ---------------------------
library(tidyverse)  # for data manipulation and visualization

# ---------------------------
# Load the Data
# ---------------------------
# Set full paths to the datasets (adjust the paths if necessary)
super_bowls <- read_csv("C:/Users/olayi/Desktop/Data/R/R Projects/Superbowl/datasets/super_bowls.csv")
tv <- read_csv("C:/Users/olayi/Desktop/Data/R/R Projects/Superbowl/datasets/tv.csv")
halftime_musicians <- read_csv("C:/Users/olayi/Desktop/Data/R/R Projects/Superbowl/datasets/halftime_musicians.csv")

# Inspect the loaded data
print(super_bowls)
print(tv)
print(halftime_musicians)

# ---------------------------
# Filter to Modern Super Bowls
# ---------------------------
# We focus on Super Bowls greater than 20 (i.e., modern era from 1987)
modern_superbowl <- super_bowls %>% filter(super_bowl > 20)
modern_halftime <- halftime_musicians %>% filter(super_bowl > 20)
modern_tv <- tv %>% filter(super_bowl > 20)

# ---------------------------
# Impute Missing share_household Data in TV Dataset
# ---------------------------
# Calculate an imputation value from modern_tv
impute_value <- mean(modern_tv$share_household, na.rm = TRUE)
modern_tv <- modern_tv %>% 
  mutate(share_household = if_else(super_bowl == 57 & is.na(share_household), 
                                   impute_value, 
                                   share_household))
# View the updated TV data
view(modern_tv)

# ---------------------------
# Q1: Do large point differences result in lost viewers?
# ---------------------------

# Plot a histogram of point differences in modern Super Bowls
ggplot(modern_superbowl, aes(difference_pts)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Point Difference", y = "Number of Super Bowls")

# Display the games with the closest and largest point differences
modern_superbowl %>% 
  filter(difference_pts == min(difference_pts) | difference_pts == max(difference_pts))

# Merge the modern TV and Super Bowl game data
modern_games_tv <- modern_tv %>% inner_join(modern_superbowl, by = "super_bowl")

# Create a scatter plot to examine the relationship between point difference and household share
ggplot(modern_games_tv, aes(difference_pts, share_household)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Point Difference", y = "Viewership (household share)")

# Based on the visualization, we interpret the relationship as weak.
score_impact = "weak"

# ---------------------------
# Q2: Trends in Viewership, Ratings, and Ad Costs
# ---------------------------

# Transform data for plotting using pivot_longer to combine multiple metrics
games_tv_plot <- modern_games_tv %>%
  pivot_longer(cols = c(avg_us_viewers, rating_household, ad_cost), 
               names_to = "category", 
               values_to = "value") %>%
  mutate(cat_name = case_when(
    category == "avg_us_viewers" ~ "Average number of US viewers",
    category == "rating_household" ~ "Household rating",
    category == "ad_cost" ~ "Advertisement cost (USD)",
    TRUE ~ as.character(category)
  ))

# Scale the values within each category for plotting
games_tv_plot_scaled <- games_tv_plot %>%
  group_by(category) %>%
  mutate(scaled_value = value / max(value)) %>%
  ungroup()

# Plot the scaled trends over time by Super Bowl
ggplot(games_tv_plot_scaled, aes(x = super_bowl, y = scaled_value, color = category)) +
  geom_line() +
  labs(x = "Super Bowl", y = "Scaled Value (0–1)", color = "Category")

# Interpretation: Ratings increased before advertisement costs did.
first_to_increase = "ratings"

# ---------------------------
# Q3: Does the Number of Songs Performed Affect Viewership?
# ---------------------------
# Merge halftime musician data with modern TV data
modern_halftime_data <- modern_halftime %>% inner_join(modern_tv, by = "super_bowl")

# Scatter plot: Number of songs vs. average US viewers
ggplot(modern_halftime_data, aes(x = num_songs, y = avg_us_viewers)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Super Bowl Viewership vs. Number of Halftime Songs",
       x = "Number of Songs Performed",
       y = "Average US Viewers (millions)") +
  theme_minimal()

# Calculate correlation between number of songs and average US viewers
correlation <- cor(modern_halftime_data$num_songs, modern_halftime_data$avg_us_viewers, use = "complete.obs")
print(paste("Correlation between number of songs and TV rating:", round(correlation, 3)))

# ---------------------------
# Q4: Which Musicians Performed the Most Songs?
# ---------------------------
# Display the top 5 musicians based on number of songs performed
modern_halftime %>% 
  arrange(desc(num_songs)) %>%
  head(5)

# ---------------------------
# Q5: Did the Number of Songs Increase Over Time?
# ---------------------------
# Plot the trend of number of songs over Super Bowls
ggplot(modern_halftime, aes(x = super_bowl, y = num_songs)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Songs Performed in Super Bowl Halftime Shows",
       x = "Super Bowl Number",
       y = "Number of Songs") +
  theme_minimal()

# ---------------------------
# Q6: Compare Performances: Solo vs. Multiple Artists
# ---------------------------
# Aggregate performance data: count the number of musicians per Super Bowl
performance_data <- modern_halftime %>%
  group_by(super_bowl) %>%
  summarize(
    num_musicians = n(),
    total_songs = sum(num_songs, na.rm = TRUE),
    primary_musician = first(musician)
  ) %>%
  ungroup() %>%
  mutate(
    performance_type = if_else(num_musicians > 1, "Multiple Artists", "Solo")
  )

# View the aggregated performance data
print(performance_data)

# Merge performance data with modern TV data
halftime_tv <- performance_data %>%
  inner_join(modern_tv, by = "super_bowl")
head(halftime_tv)

# Boxplot: Compare TV ratings for solo vs. multiple artist performances
ggplot(halftime_tv, aes(x = performance_type, y = rating_household, fill = performance_type)) +
  geom_boxplot() +
  labs(title = "TV Ratings by Halftime Performance Type",
       x = "Performance Type",
       y = "Household TV Rating") +
  theme_minimal()

# ---------------------------
# Q7: Association Between Total Songs and TV Ratings
# ---------------------------
# Scatter plot: Total number of songs vs. household TV rating
ggplot(halftime_tv, aes(x = total_songs, y = rating_household)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Association Between Total Songs and TV Ratings",
       x = "Total Number of Songs Performed",
       y = "Household TV Rating") +
  theme_minimal()

# Calculate correlation between total songs and TV ratings
correlation <- cor(halftime_tv$total_songs, halftime_tv$rating_household, use = "complete.obs")
print(paste("Correlation between total songs and TV rating:", round(correlation, 3)))

# ---------------------------
# Q8: Identify the Highest Rated Halftime Show
# ---------------------------
# Find the Super Bowl with the highest household TV rating
highest_rated <- halftime_tv %>%
  arrange(desc(rating_household)) %>%
  select(super_bowl, performance_type, primary_musician, rating_household, total_songs) %>%
  head(1)
print(highest_rated)
