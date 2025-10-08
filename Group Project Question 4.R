# Question 4

# Load packages
library(ggplot2)
library(dplyr)

listings <- read.csv("listings.csv")
View(listings)

str(listings)
colnames(listings)
sum(is.na(listings$neighbourhood_group))


#Clean data - remove listings with no price
sum(is.na(listings$price))


listings_clean <- listings %>%
  filter(!is.na(price)) %>%
  mutate(
    host_category = case_when(
      calculated_host_listings_count == 1 ~ "Single Host",
      calculated_host_listings_count <= 5 ~ "Small Host (2–5)",
      calculated_host_listings_count <= 10 ~ "Medium Host (6–10)",
      TRUE ~ "Large Host (10+)"
    )
  )

ggplot(listings_clean, aes(x = host_category, y = price)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  scale_y_log10(labels = scales::dollar_format()) +
  labs(
    title = "Airbnb Price Distribution by Host Size (log scale)",
    y = "Listing Price (USD, log10 scale)",
    x = "Host Category"
  ) +
  theme_minimal()


#EDA
summary_stats <- listings_clean %>%
  group_by(host_category) %>%
  summarise(
    count = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    q1 = quantile(price, 0.25, na.rm = TRUE),
    q3 = quantile(price, 0.75, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE)
  )

summary_stats


