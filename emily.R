# Load necessary libraries
library(dplyr)
library(readr)

# Load the dataset
airbnbData <- read_csv('airbnbSource.csv')

# Perform initial exploration
summary(airbnbData)
str(airbnbData)
head(airbnbData)


install.packages("geosphere")
library(geosphere)


# Assuming we have the coordinates for JFK Airport
jfk_coords <- c(lat = 40.6413, long = -73.7781)

# Calculate distances from JFK and filter listings within a certain radius
airbnbData <- airbnbData %>%
  mutate(distance_from_jfk = distm(
    matrix(c(longitude, latitude), ncol = 2),
    matrix(c(jfk_coords["long"], jfk_coords["lat"]), ncol = 2),
    fun = distHaversine
  )) %>%
  filter(distance_from_jfk <= 10 * 1609.34) # within 10 miles from JFK


# Filter listings based on the capacity
suitable_listings <- airbnbData %>%
  group_by(`host [id]`) %>% # Replace with the correct column name
  filter(sum(`room and type` == 'Entire home/apt') * 2 + sum(`room and type` == 'Private room') >= 7)


# Define a ranking function
rank_listings <- function(df) {
  
  # Normalize the distance and noise values
  df$normalized_distance <- (df$distance_from_jfk - min(df$distance_from_jfk)) / (max(df$distance_from_jfk) - min(df$distance_from_jfk))
  df$normalized_noise <- (df$`noise(dB)` - min(df$`noise(dB)`)) / (max(df$`noise(dB)`) - min(df$`noise(dB)`))
  
  # Calculate the score
  df$score <- with(df, (1 - normalized_distance) + (1 - normalized_noise) + `floor` + (1 / `Price($)`))
  
  # Return the dataframe with rankings
  df %>%
    arrange(desc(score)) %>%
    select(ID, name, neighbourhood, score)
}

# Apply the ranking function and get the top listings
ranked_listings <- rank_listings(suitable_listings)
top_listings <- head(ranked_listings, 5)
print(top_listings)





# Hypothesis 1: Comparison of prices for top 20% housing market neighborhoods
price_75th_percentile <- quantile(airbnbData$`Price($)`, 0.75)

# Create a new column 'expensive' based on the 75th percentile
airbnbData$expensive <- airbnbData$`Price($)` > price_75th_percentile

# Permutation test
set.seed(123) 
perm_diffs <- replicate(10000, {
  shuffled_expensive <- sample(airbnbData$expensive)
  mean(airbnbData$`Price($)`[shuffled_expensive]) - mean(airbnbData$`Price($)`[!shuffled_expensive])
})

# Calculate the observed difference in means
observed_diff <- mean(airbnbData$`Price($)`[airbnbData$expensive]) - mean(airbnbData$`Price($)`[!airbnbData$expensive])

# Calculate the p-value
p_value <- mean(abs(perm_diffs) >= abs(observed_diff))

# Print the p-value
p_value









#Bayesian Odds Task Implementation
# Calculate the 75th percentile of price for all Airbnb listings
price_75th_percentile <- quantile(airbnbData$`Price($)`, 0.75)

# Calculate the prior probability of a listing being expensive (above the 75th percentile)
p_expensive <- mean(airbnbData$`Price($)` > price_75th_percentile)

# Calculate the prior odds of a listing being expensive
prior_odds_expensive <- p_expensive / (1 - p_expensive)

# Calculate the probability of a listing being in Manhattan given it's expensive
p_manhattan_given_expensive <- mean(airbnbData$`neighbourhood group` == 'Manhattan' & airbnbData$`Price($)` > price_75th_percentile) / p_expensive

# Calculate the probability of a listing being in Manhattan given it's not expensive
p_manhattan_given_not_expensive <- mean(airbnbData$`neighbourhood group` == 'Manhattan' & airbnbData$`Price($)` <= price_75th_percentile) / (1 - p_expensive)

# Calculate the likelihood ratio
likelihood_ratio <- p_manhattan_given_expensive / p_manhattan_given_not_expensive

# Calculate the posterior odds
posterior_odds_expensive_manhattan <- prior_odds_expensive * likelihood_ratio

# Output the posterior odds
posterior_odds_expensive_manhattan

