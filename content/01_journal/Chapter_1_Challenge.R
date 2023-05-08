# install.packages("plotly")

library(tidyverse)
library(tidyquant)
library(broom)
library(ggplot2)
library(umap)

# STOCK PRICES
sp_500_prices_tbl <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/sp_500_prices_tbl.rds")
sp_500_prices_tbl

# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/sp_500_index_tbl.rds")
sp_500_index_tbl

sp_500_prices_tbl %>% glimpse()

# Select symbol, date, and adjusted columns
sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
  select(symbol, date, adjusted) %>%
  
  # Filter to dates beginning in the year 2018 and beyond
  filter(date >= as.Date("2018-01-01")) %>%
  
  # Compute a Lag of 1 day on the adjusted stock price, grouped by symbol
  group_by(symbol) %>%
  mutate(lag_adjusted = lag(adjusted)) %>%
  
  # Remove NA values from the lag operation
  filter(!is.na(lag_adjusted)) %>%
  
  # Compute the difference between adjusted and the lag
  mutate(diff_adjusted = adjusted - lag_adjusted) %>%
  
  # Compute the percentage difference by dividing the difference by the lag
  mutate(pct_return = diff_adjusted / lag_adjusted) %>%
  
  # Return only the symbol, date, and pct_return columns
  select(symbol, date, pct_return)

# Save as a variable named sp_500_daily_returns_tbl
sp_500_daily_returns_tbl <- as_tibble(sp_500_daily_returns_tbl)

sp_500_daily_returns_tbl <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/sp_500_daily_returns_tbl.rds")
sp_500_daily_returns_tbl

# Spread the date column to get the values as percentage returns, filling NAs with zeros
stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%
  spread(key = date, value = pct_return, fill = 0)

# Save the result as stock_date_matrix_tbl
stock_date_matrix_tbl <- as_tibble(stock_date_matrix_tbl)
  
stock_date_matrix_tbl <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/stock_date_matrix_tbl.rds")


# Drop the non-numeric column, symbol
stock_date_matrix_tbl <- stock_date_matrix_tbl %>%
  select(-symbol)

# Perform kmeans() with centers = 4 and nstart = 20
kmeans_obj <- kmeans(stock_date_matrix_tbl, centers = 4, nstart = 20)

# Use glance() to get the tot.withinss
tot_withinss <- glance(kmeans_obj)$tot.withinss

kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    kmeans(centers = center, nstart = 20)
}

# Print the tot.withinss value
cat("Total within-cluster sum of squares (tot.withinss):", tot_withinss, "\n")

# Create a tibble with the 'centers' column ranging from 1 to 30
centers_tbl <- tibble(centers = 1:30)

# Add a column named 'k_means' with the kmeans_mapper() output using mutate() and map()
k_means_mapped_tbl <- centers_tbl %>%
  mutate(k_means = map(centers, ~kmeans_mapper(center = .x)))

# Add a column named 'glance' with the glance() output using mutate() and map()
k_means_mapped_tbl <- k_means_mapped_tbl %>%
  mutate(glance = map(k_means, ~glance(.x)))

# Print the resulting tibble
k_means_mapped_tbl

# Unnest the glance column
k_means_mapped_tbl <- unnest(k_means_mapped_tbl, glance)

# Plot the Scree Plot
ggplot(k_means_mapped_tbl, aes(x = centers, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  labs(title = "Scree Plot") +
  theme_bw()

k_means_mapped_tbl <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/k_means_mapped_tbl.rds")

# Apply umap() function to stock_date_matrix_tbl
umap_results <- stock_date_matrix_tbl %>%
  umap()

# Convert layout from umap_results to a tibble
umap_results_tbl <- as_tibble(umap_results$layout)

# Bind the columns of umap_results_tbl with the symbol column from stock_date_matrix_tbl
umap_results_tbl <- bind_cols(stock_date_matrix_tbl$symbol, umap_results_tbl)

# Rename the symbol column
colnames(umap_results_tbl)[1] <- "symbol"

# Save the results as umap_results_tbl
umap_results_tbl

# Create the visualization
ggplot(umap_results_tbl, aes(x = V1, y = V2)) +
  geom_point(alpha = 0.5) +
  labs(title = "UMAP Projection") +
  theme_tq()

k_means_mapped_tbl <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/k_means_mapped_tbl.rds")
umap_results_tbl   <- read_rds("~/GitHub/ss23-bdml-shubh11am/machine_learning_fundamentals/umap_results_tbl.rds")

# Get the k_means_obj from the 10th center
kmeans_obj <- k_means_mapped_tbl$k_means[[10]]

# Augment k_means_obj with stock_date_matrix_tbl and select symbol and .cluster columns
k_means_augmented <- augment(kmeans_obj, stock_date_matrix_numeric) %>%
  select(symbol, .cluster)

# Left join k_means_augmented with umap_results_tbl by symbol
umap_kmeans_results_tbl <- left_join(umap_results_tbl, k_means_augmented, by = "symbol")

# Left join umap_kmeans_results_tbl with sp_500_index_tbl by symbol
umap_kmeans_results_tbl <- left_join(umap_kmeans_results_tbl, sp_500_index_tbl %>% select(symbol, company, sector), by = "symbol")

# Store the output as umap_kmeans_results_tbl
umap_kmeans_results_tbl

