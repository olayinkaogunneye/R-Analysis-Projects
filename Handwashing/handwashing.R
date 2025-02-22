# Load required libraries
library(tidyverse)

# Load and inspect the data
yearly <- read_csv("C:/Users/olayi/Desktop/Data/R/R Projects/Handwashing/datasets/yearly_deaths_by_clinic.csv")
print(yearly)

monthly <- read_csv("C:/Users/olayi/Desktop/Data/R/R Projects/Handwashing/datasets/monthly_deaths.csv")
print(monthly)

# Add proportion_deaths to both data frames
yearly <- yearly %>% 
  mutate(proportion_deaths = deaths / births)

monthly <- monthly %>% 
  mutate(proportion_deaths = deaths / births)

# Plot yearly proportion of deaths by clinic
ggplot(yearly, aes(x = year, y = proportion_deaths, color = clinic)) +
  geom_line() +
  labs(title = "Yearly Proportion of Deaths by Clinic", 
       x = "Year", 
       y = "Proportion of Deaths")

# Plot monthly proportion of deaths
ggplot(monthly, aes(date, proportion_deaths)) +
  geom_line() +
  labs(title = "Monthly Proportion of Deaths", 
       x = "Year", 
       y = "Proportion of Deaths")

# Add a flag for when handwashing started
handwashing_start = as.Date('1847-06-01')

monthly <- monthly %>%
  mutate(handwashing_started = date >= handwashing_start)

# Plot monthly deaths with handwashing status
ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_started)) +
  geom_line() +
  labs(title = "Effect of Handwashing on Monthly Deaths", 
       x = "Year", 
       y = "Proportion of Deaths")

# Calculate mean proportion of deaths before and after handwashing
monthly_summary <- monthly %>% 
  group_by(handwashing_started) %>%
  summarize(mean_proportion_deaths = mean(proportion_deaths, na.rm = TRUE))

# Print summary
print(monthly_summary)
