# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the datasets
airbnb_df <- read.csv("NY_airBNB.csv")
ny_housing_df <- read.csv("NY-House-Dataset.csv")


###EDA 

# Viewing the structure
str(airbnb_df)
str(ny_housing_df)

# Summary statistics
summary(airbnb_df)
summary(ny_housing_df)


# Checking for missing values in Airbnb dataset
colSums(is.na(airbnb_df))

# Checking for missing values in NY Housing dataset
colSums(is.na(ny_housing_df))




# Log transformation of Airbnb prices
ggplot(airbnb_df, aes(x = log1p(price))) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("Log-transformed Distribution of Airbnb Prices")

# Log transformation of NY Housing Market prices
ggplot(ny_housing_df, aes(x = log1p(PRICE))) + 
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_minimal() +
  ggtitle("Log-transformed Distribution of Housing Market Prices")



# Selecting the top 20 neighborhoods by average price
top_neighborhoods <- airbnb_avg_by_neighborhood %>%
  top_n(20, avg_price)

# Plotting the average price for these top neighborhoods
ggplot(top_neighborhoods, aes(x = reorder(neighbourhood, avg_price), y = avg_price)) + 
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  coord_flip() +
  ggtitle("Top 20 Neighborhoods by Average Airbnb Price")


# Correlation matrix for Airbnb dataset
cor(airbnb_df %>% select(price, minimum_nights, number_of_reviews, reviews_per_month))

# Correlation matrix for NY Housing dataset
# Adjust the column selection based on the actual columns in the dataset
cor(ny_housing_df %>% select(PRICE, BEDS, BATH, PROPERTYSQFT))




#Hypothesis: "There is no significant difference in the average price of Airbnb listings 
#between neighborhoods that are in the top 20% of the housing market prices and those that are not."


# Assuming you have a variable 'top_20_percent_housing' that indicates whether a neighborhood is in the top 20% of housing market prices.
# Let's create a sample variable for illustration purposes.
airbnb_df$top_20_percent_housing <- sample(c(TRUE, FALSE), nrow(airbnb_df), replace = TRUE)

# Calculate the observed difference in means
observed_mean_diff <- with(airbnb_df, mean(price[top_20_percent_housing]) - mean(price[!top_20_percent_housing]))

# Initialize permutation distribution
perm_diffs <- replicate(10000, {
  # Shuffle the top_20_percent_housing labels
  shuffled_top_20 <- sample(airbnb_df$top_20_percent_housing)
  
  # Compute the mean difference for the permuted data
  mean(airbnb_df$price[shuffled_top_20]) - mean(airbnb_df$price[!shuffled_top_20])
})

# Calculate the p-value
p_value <- mean(abs(perm_diffs) >= abs(observed_mean_diff))
p_value




#Hypothesis 2:
#"Entire home/apt" listings are more expensive on average than "Private room" listings.

# Filter the data for each room type
entire_home_prices <- airbnb_df[airbnb_df$room_type == 'Entire home/apt',]$price
private_room_prices <- airbnb_df[airbnb_df$room_type == 'Private room',]$price

# Calculate the sample mean and standard deviation for each room type
entire_home_mean <- mean(entire_home_prices)
private_room_mean <- mean(private_room_prices)
entire_home_sd <- sd(entire_home_prices)
private_room_sd <- sd(private_room_prices)

# Calculate the sample sizes
n_entire_home <- length(entire_home_prices)
n_private_room <- length(private_room_prices)

# Calculate the z-score
z_score_room_type <- (entire_home_mean - private_room_mean) / sqrt((entire_home_sd^2 / n_entire_home) + (private_room_sd^2 / n_private_room))

# Calculate the p-value
p_value_room_type <- 2 * pnorm(-abs(z_score_room_type))

p_value_room_type




#Hypothesis 3:
#"Airbnb listings in Manhattan have a higher average price than listings in Brooklyn."

# Filter the data for Manhattan and Brooklyn listings
manhattan_prices <- airbnb_df[airbnb_df$neighbourhood_group == 'Manhattan',]$price
brooklyn_prices <- airbnb_df[airbnb_df$neighbourhood_group == 'Brooklyn',]$price

# Calculate the sample mean and standard deviation for each group
manhattan_mean <- mean(manhattan_prices)
brooklyn_mean <- mean(brooklyn_prices)
manhattan_sd <- sd(manhattan_prices)
brooklyn_sd <- sd(brooklyn_prices)

# Calculate the sample sizes
n_manhattan <- length(manhattan_prices)
n_brooklyn <- length(brooklyn_prices)

# Calculate the z-score
z_score <- (manhattan_mean - brooklyn_mean) / sqrt((manhattan_sd^2 / n_manhattan) + (brooklyn_sd^2 / n_brooklyn))

# Calculate the p-value
p_value <- 2 * pnorm(-abs(z_score))

p_value









#Task (b): Bayesian Odds Task - 1

#Hypothesis: "An Airbnb listing is more likely to be priced above the 75th percentile (expensive) if it is located in Manhattan compared to Brooklyn."
# Calculate the 75th percentile of price for all Airbnb listings
price_75th_percentile <- quantile(airbnb_df$price, 0.75)

# Calculate the prior probability of a listing being expensive
p_expensive <- mean(airbnb_df$price > price_75th_percentile)

# Calculate the prior odds of a listing being expensive
prior_odds_expensive <- p_expensive / (1 - p_expensive)

# Calculate the probability of a listing being in Manhattan given it's expensive
p_manhattan_given_expensive <- mean(airbnb_df$neighbourhood_group == 'Manhattan' & airbnb_df$price > price_75th_percentile) / p_expensive

# Calculate the probability of a listing being in Manhattan given it's not expensive
p_manhattan_given_not_expensive <- mean(airbnb_df$neighbourhood_group == 'Manhattan' & airbnb_df$price <= price_75th_percentile) / (1 - p_expensive)

# Calculate the likelihood ratio
likelihood_ratio <- p_manhattan_given_expensive / p_manhattan_given_not_expensive


# Calculate the posterior odds
posterior_odds_expensive_manhattan <- prior_odds_expensive * likelihood_ratio

posterior_odds_expensive_manhattan









#d) Employ multiple plots to visually represent relationships within the data.
ggplot(airbnb_df, aes(x = number_of_reviews, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Number of Reviews", y = "Price", title = "Relationship between Number of Reviews and Price") +
  theme_minimal()





ggplot(airbnb_df, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot() +
  labs(x = "Neighborhood Group", y = "Price", title = "Price Distribution by Neighborhood Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(airbnb_df, aes(x = price, fill = room_type)) +
  geom_density(alpha = 0.7) +
  labs(x = "Price", y = "Density", title = "Price Density by Room Type") +
  theme_minimal() +
  xlim(0, 1000) # Limit x-axis to remove extreme values for better visualization


library(corrplot)

# Calculating the correlation matrix
cor_matrix <- cor(airbnb_df %>% select(price, minimum_nights, number_of_reviews, reviews_per_month), use = "complete.obs")

# Plotting the heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")









