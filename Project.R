# Install packages if needed
#install.packages(c('tidyverse', 'lubridate', 'GGally', 'emmeans', 'ggplot2','Hmisc', 'FactoMineR', 'factoextra'))
# Load the necessary packages
library(tidyverse)
library(lubridate)
library(GGally)
library (emmeans)
library(ggplot2)
library(Hmisc)
library(FactoMineR)
library(factoextra)

# Read in the dataset
avocado_data <- read.csv("avocado.csv", header = TRUE)

# Making copy of original data
df <- avocado_data

# avocado_data <- df
attach(avocado_data)

# View the first few rows of the dataset
head(avocado_data)

# Check the structure of the dataset
str(avocado_data)

# Summary statistics
summary(avocado_data[, c("AveragePrice", "Total.Volume", "X4046", "X4225", "X4770",
                         "Total.Bags", "Small.Bags","Large.Bags", "XLarge.Bags",
                         "type", "year", "region")])

# Converting date column from char to posixct
avocado_data$Date <- parse_date_time(avocado_data$Date, orders = c("%m/%d/%Y", "%Y-%m-%d"))


# Check for null values in all columns
colSums(is.na(avocado_data))


# Histogram of avocado prices
ggplot(avocado_data, aes(x = AveragePrice)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  ggtitle("Distribution of Average Avocado Prices")


# Box plot of avocado prices by type
ggplot(avocado_data, aes(x = type, y = AveragePrice)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  ggtitle("Average Avocado Prices by Type")


# Time series plot of avocado prices by year
avocado_data %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = factor(year))) +
  geom_line(size = 1) +
  labs(title = "Average Avocado Prices Over Time", x = "Date", y = "Average Price")


# Scatterplot of avocado prices and volume
ggplot(avocado_data, aes(x = Total.Volume, y = AveragePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Avocado Prices vs. Volume", x = "Total Volume", y = "Average Price")

# Price trends by type and region
# I uploaded this graph in the group chat in case it's not displaying properly on your screen
avocado_data %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = type)) +
  geom_line() +
  facet_wrap(~region, ncol = 6, scales = "free_x") +
  labs(title = "Avocado Prices by Region and Type", x = "Date", y = "Average Price")

# Check size difference by price and or region

# These graph don't look that nice so I'll leave them here if you want to look

# avocado_data %>%
#   ggplot(aes(x = 4046, y = AveragePrice, color = region)) +
#   geom_point() +
#   labs(title = "Scatterplot of 4046 vs. Average Price by Region")
# 
# avocado_data %>%
#   ggplot(aes(x = 4225, y = AveragePrice, color = region)) +
#   geom_point() +
#   labs(title = "Scatterplot of 4225 vs. Average Price by Region")
# 
# avocado_data %>%
#   ggplot(aes(x = 4770, y = AveragePrice, color = region)) +
#   geom_point() +
#   labs(title = "Scatterplot of 4770 vs. Average Price by Region")


# If you want to compare specific regions with different sized avocados and avg price
ggpairs(avocado_data[avocado_data$region %in% c("GreatLakes", "NorthernNewEngland", "SouthCentral",
                                                 "Midsouth", "Southeast", "West", "Northeast"), 
                     c("X4046", "X4225", "X4770", "AveragePrice", "region")],
        lower = list(continuous = "points"), 
        upper = list(combo = "box"), 
        axisLabels = "show",
        title = "Scatterplot Matrix of Avocado Data")

# T-test for the average price of organic and conventional avocados
t.test(AveragePrice ~ type, data = avocado_data, subset = type %in% c("organic", "conventional"))

# ANOVA to test for differences in average price among regions
anova <- aov(AveragePrice ~ region, data = avocado_data)
summary(anova)

# ANOVA to test for differences in average price among years
avo_anova <- aov(AveragePrice ~ year, data = avocado_data)
summary(avo_anova)

# Chi-square test for association between region and type
table <- table(avocado_data$type, avocado_data$region)
chisq.test(table)

# Select relevant columns
vol_cols <- c("X4046", "X4225", "X4770")
price_col <- "AveragePrice"
vol_price_data <- avocado_data[, c(vol_cols, price_col)]

# Calculate Pearson correlation coefficients
correlations <- cor(vol_price_data, method = "pearson")

# View correlation matrix
correlations

ggpairs(avocado_data[, c(vol_cols, price_col)],
        lower = list(continuous = "points"), 
        upper = list(combo = "box"), 
        axisLabels = "show",
        title = "Scatterplot Matrix of Avocado Volume and Price Data")

#Linear model to compare avg price and total bags sold
lm_model <- lm(AveragePrice ~ Total.Bags, data = avocado_data)
summary(lm_model)

ggplot(avocado_data, aes(x = Total.Bags, y = AveragePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Average Price and Total Bags Sold",
       x = "Total Bags Sold", y = "Average Price")


#Are there significant differences in average prices between different regions, 
#after controlling for the type of avocado (conventional or organic) and the year?

# Fit multiple linear regression model
model <- lm(AveragePrice ~ type*region*year, data = avocado_data)

# Test for overall significance of the model
summary(model)

# Test for significance of individual coefficients
summary(model)$coefficients

# Test for interaction effect between type and region
summary(model)$coefficients["typeorganic:regionWest", c("Estimate", "Pr(>|t|)")]

# Get predicted average prices for each region, holding type and year constant
emmeans(model, ~ region, at = list(type = c("conventional", "organic"), year = mean(avocado_data$year)))

# Plot predicted average prices by region
# This graphs X-axis is a little cluttered if anyone can fix it that would be appreciated 
ggplot(avocado_data, aes(x = region, y = AveragePrice, color = type)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") +
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 2) +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Average Prices by Region, Type, and Year",
    x = "Region", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
# I tried doing PCA here but on this data it's pointless. All the data is important further reduction is unnecessary

# # Subset the data to include only numerical variables
# avocado_num <- avocado_data[, c("X4046", "X4225", "X4770", "Total.Volume", "Total.Bags", "Small.Bags", "Large.Bags", "XLarge.Bags", "AveragePrice")]
# 
# # Run PCA on the numerical variables
# pca_avocado <- PCA(avocado_num, scale.unit = TRUE, ncp = 5, graph = FALSE)
# 
# # Print summary of the results
# summary(pca_avocado)
# 
# # Plot the scree plot to visualize the variance explained by each component
# plot(pca_avocado, choix = "var")
# 
# # Plot the correlation circle to visualize the contribution of each variable to each component
# plot(pca_avocado, choix = "varcor")
# 
# # Converting type from char to factor to use in PCA
# avocado_data$type <- factor(avocado_data$type)
# 
# # Plot the individuals on the first two principal components to visualize any clusters or patterns in the data
# plot(pca_avocado, choix = "ind", habillage = avocado_data$type)
# 
# # Type 1 is conventional Type 2 Organic
# avocado_data$type <- as.numeric(avocado_data$type)
# 
# pca_avocado2 <- prcomp(avocado_data[,3:12], scale. = TRUE)
# plot(pca_avocado2, choix = "ind", habillage = avocado_data$type)
# 
# summary(pca_avocado2)
# 
