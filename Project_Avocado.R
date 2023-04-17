library(dplyr)
library(ggplot2)
library(ggtext)
library(lubridate)
library(tidyr)

rm(list=ls())

# 0: read in the dataset
avocado <- read.csv("avocado.csv", header = TRUE)

# 1: Average price based on type: Conventional vs. Organic

# 1.1 draw histograms for each type
ggplot(avocado, aes(x = AveragePrice, fill = type)) +
  geom_histogram(bins = 30, alpha = 0.8, position = 'identity', color = "black") +
  facet_wrap(~ type) +
  labs(title = "Histogram of Average Price by Type",
       x = "Average Price",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Conventional" = "#56B4E9", "Organic" = "#D55E00"))


# 1.2 mean price for each type
mean_price_by_type <- aggregate(AveragePrice ~ type, data = avocado, FUN = mean)

print(mean_price_by_type)

summary_by_type <- by(avocado$AveragePrice, avocado$type, summary)

print(summary_by_type)


# 1.3 box plot
ggplot(avocado, aes(x = type, y = AveragePrice)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  ggtitle("Average Avocado Prices by Type")



# 2: check seasonal fluctuations

# 2.1 Extract the month from the date column
avocado$month <- month(ymd(avocado$Date))

# 2.2 Summarize the average price for each month and type
monthly_summary <- avocado %>%
  group_by(type, month) %>%
  summarise(avg_price = mean(AveragePrice, na.rm = TRUE)) %>%
  arrange(type, month)

print(monthly_summary)

# 2.3 Create a line graph for monthly price
ggplot(monthly_summary, aes(x = month, y = avg_price, group = type, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  labs(title = "Average Price of Avocados by Type",
       x = "Month",
       y = "Average Price",
       color = "Type") +
  theme_minimal()



# 2.4 Draw monthly price table
monthly_summary_table <- monthly_summary %>%
  pivot_wider(names_from = month, values_from = avg_price)

monthly_summary_long <- monthly_summary_table %>%
  mutate(type = as.factor(type)) %>%
  pivot_longer(-type, names_to = "month", values_to = "avg_price") %>%
  mutate(month = as.numeric(month))

ggplot(monthly_summary_long, aes(x = month, y = type, fill = avg_price, label = round(avg_price, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "white", size = 4) +
  scale_x_continuous(breaks = 1:12) +
  scale_fill_gradient2(low = "lightblue", mid = "blue", high = "darkblue", midpoint = median(monthly_summary_long$avg_price)) +
  labs(x = "Month", y = "Type", fill = "Average Price", title = "Average Price by Month and Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))

# 3: check geographical variations

# 3.1 Average Price by Region, conventional and organic
conventional_data <- avocado %>% filter(type == "conventional")
organic_data <- avocado %>% filter(type == "organic")

# 3.2 Plot, order cities by price from high to low
ggplot(avocado, aes(x = reorder(region, AveragePrice, median), y = AveragePrice, fill = type)) +
  geom_boxplot(show.legend = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = "top") +
  labs(x = "Region", y = "Average Price", title = "Average Price by Region for Conventional and Organic Avocados") +
  coord_flip()
