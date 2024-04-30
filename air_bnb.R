air_bnb <- read.csv("C:\\Users\\areeb\\OneDrive - Asia Pacific University\\Desktop\\Project\\Airbnb_Open_Data.csv")
View(air_bnb)
summary(air_bnb)
install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("dplyr")
#Checking Column names
colnames(air_bnb)

#Unique values
unique(air_bnb$id)
unique(air_bnb$NAME)
unique(air_bnb$host.id)
unique(air_bnb$host_identity_verified)
unique(air_bnb$host.name)
unique(air_bnb$neighbourhood.group)
unique(air_bnb$neighbourhood)
unique(air_bnb$lat)
unique(air_bnb$long)
unique(air_bnb$country)
unique(air_bnb$country.code)
unique(air_bnb$instant_bookable)
unique(air_bnb$cancellation_policy)
unique(air_bnb$room.type)
unique(air_bnb$Construction.year)
unique(air_bnb$price)
unique(air_bnb$service.fee)
unique(air_bnb$minimum.nights)
unique(air_bnb$number.of.reviews)
unique(air_bnb$last.review)
unique(air_bnb$reviews.per.month)
unique(air_bnb$review.rate.number)
unique(air_bnb$calculated.host.listings.count)
unique(air_bnb$availability.365)
unique(air_bnb$house_rules)
unique(air_bnb$license)

#Tranformation to Quantitative Values
air_bnb$price <- as.numeric(gsub("[^0-9.]", "", air_bnb$price))
#Checking class
class(air_bnb$price)
# Check the summary
summary(air_bnb$price)

air_bnb$service.fee <- as.numeric(gsub("[^0-9.]", "", air_bnb$service.fee))
#Checking class
class(air_bnb$service.fee)
# Check the summary
summary(air_bnb$service.fee)

air_bnb$minimum.nights <- as.numeric(gsub("[^0-9.]", "", air_bnb$minimum.nights))
#Checking class
class(air_bnb$minimum.nights)
# Check the summary
summary(air_bnb$minimum.nights)

air_bnb$number.of.reviews <- as.numeric(gsub("[^0-9.]", "", air_bnb$number.of.reviews))
#Checking class
class(air_bnb$number.of.reviews)
# Check the summary 
summary(air_bnb$number.of.reviews)

air_bnb$reviews.per.month <- as.numeric(gsub("[^0-9.]", "", air_bnb$reviews.per.month))
#Checking class
class(air_bnb$reviews.per.month)
# Check the summary
summary(air_bnb$reviews.per.month)

air_bnb$review.rate.number <- as.numeric(gsub("[^0-9.]", "", air_bnb$review.rate.number))
#Checking class
class(air_bnb$review.rate.number)
# Check the summary
summary(air_bnb$review.rate.number)

air_bnb$calculated.host.listings.count <- as.numeric(gsub("[^0-9.]", "", air_bnb$calculated.host.listings.count))
#Checking class
class(air_bnb$calculated.host.listings.count)
# Check the summary 
summary(air_bnb$calculated.host.listings.count)

air_bnb$availability.365 <- as.numeric(gsub("[^0-9.]", "", air_bnb$availability.365))
#Checking class
class(air_bnb$availability.365)
# Check the summary 
summary(air_bnb$availability.365)

#Find missing values
is.na(air_bnb)

#Count missing values
sum(is.na(air_bnb$room.type))

#Data Cleaning
air_bnb$host_identity_verified <- ifelse(air_bnb$host_identity_verified == "", "unconfirmed", air_bnb$host_identity_verified)

# Count blank spaces in host.name column
blank_count <- sum(air_bnb$host.name == "")
print(blank_count)
#Removing the rows which blank spaces
air_bnb <- air_bnb[air_bnb$host.name != "", ]

#Count Blank Spaces in neighbourhood.group column
blank_count1 <- sum(air_bnb$neighbourhood.group == "")
print(blank_count1)
#Replace with most frequent group
most_frequent_neighbourhood <- names(sort(table(air_bnb$neighbourhood.group), decreasing = TRUE))[1]
air_bnb$neighbourhood.group[air_bnb$neighbourhood.group == ""] <- most_frequent_neighbourhood

#Count Blank Spaces in neighbourhood column
blank_count2 <- sum(air_bnb$neighbourhood == "")
print(blank_count2)
#Replace with most frequent 
most_frequent_neighbourhood_1 <- names(sort(table(air_bnb$neighbourhood), decreasing = TRUE))[1]
air_bnb$neighbourhood[air_bnb$neighbourhood == ""] <- most_frequent_neighbourhood_1

#Count Blank Spaces in country column
blank_count3 <- sum(air_bnb$country == "")
print(blank_count3)
#Replace with most frequent 
most_frequent_country <- names(sort(table(air_bnb$country), decreasing = TRUE))[1]
air_bnb$country[air_bnb$country == ""] <- most_frequent_country

#Count Blank Spaces in country.code column
blank_count4 <- sum(air_bnb$country.code == "")
print(blank_count4)
#Replace with most frequent 
most_frequent_country.code <- names(sort(table(air_bnb$country.code), decreasing = TRUE))[1]
air_bnb$country.code[air_bnb$country.code == ""] <- most_frequent_country.code


#Counting missing values of instant_bookable
sum(is.na(air_bnb$instant_bookable))

# Calculate the mode of instant_bookable
mode_instant_bookable <- names(sort(table(air_bnb$instant_bookable), decreasing = TRUE))[1]

# Replace NA values with the mode
air_bnb$instant_bookable[is.na(air_bnb$instant_bookable)] <- mode_instant_bookable

# Replace blank spaces in 'NAME' column with 'Unknown"
air_bnb$NAME[air_bnb$NAME == ""] <- "Unknown"

#Count Blank Spaces in cancellation_policy column
blank_count5 <- sum(air_bnb$cancellation_policy == "")
print(blank_count5)
#Replace with most frequent 
most_frequent_cancellation_policy <- names(sort(table(air_bnb$cancellation_policy), decreasing = TRUE))[1]
air_bnb$cancellation_policy[air_bnb$cancellation_policy == ""] <- most_frequent_cancellation_policy

#Counting missing values of construction_year
sum(is.na(air_bnb$Construction.year))

# Calculate the mode of construction_year
mode_construction <- names(sort(table(air_bnb$Construction.year), decreasing = TRUE))[1]

# Replace NA values with the mode
air_bnb$Construction.year[is.na(air_bnb$Construction.year)] <- mode_construction

#Counting missing values of price
sum(is.na(air_bnb$price))

# Calculate the mean of price
mean_price <- mean(air_bnb$price, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$price[is.na(air_bnb$price)] <- mean_price
print(mean_price)

#Counting missing values of service_fee
sum(is.na(air_bnb$service.fee))

# Calculate the mean of service_fee
mean_service_fee <- mean(air_bnb$service.fee, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$service.fee[is.na(air_bnb$service.fee)] <- mean_service_fee
print(mean_service_fee)

#Counting missing values of minimum nights
sum(is.na(air_bnb$minimum.nights))

# Calculate the mean of minimum nights
mean_minimum_nights <- mean(air_bnb$minimum.nights, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$minimum.nights[is.na(air_bnb$minimum.nights)] <- mean_minimum_nights
print(mean_minimum_nights)


#Counting missing values of number of reviews
sum(is.na(air_bnb$number.of.reviews))

# Calculate the mean of number of reviews
mean_number_reviews <- mean(air_bnb$number.of.reviews, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$number.of.reviews[is.na(air_bnb$number.of.reviews)] <- mean_number_reviews
print(mean_number_reviews)

# Replace blank spaces in Last.review column with 'Unknown"
air_bnb$last.review[air_bnb$last.review == ""] <- "Unknown"

#Counting missing values of reviews.per.month
sum(is.na(air_bnb$reviews.per.month))

# Calculate the mean
mean_reviews_per_month <- mean(air_bnb$reviews.per.month, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$reviews.per.month[is.na(air_bnb$reviews.per.month)] <- mean_reviews_per_month
print(mean_reviews_per_month)

#Counting missing values of reviews rate
sum(is.na(air_bnb$review.rate.number))

# Calculate the mean 
mean_reviews_rate <- mean(air_bnb$review.rate.number, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$review.rate.number[is.na(air_bnb$review.rate.number)] <- mean_reviews_rate
print(mean_reviews_rate)

#Counting missing values of host listings
sum(is.na(air_bnb$calculated.host.listings.count))

# Calculate the mean 
mean_host_listings <- mean(air_bnb$calculated.host.listings.count, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$calculated.host.listings.count[is.na(air_bnb$calculated.host.listings.count)] <- mean_host_listings
print(mean_host_listings)

#Counting missing values of availability
sum(is.na(air_bnb$availability.365))

# Calculate the mean 
mean_availability <- mean(air_bnb$availability.365, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$availability.365[is.na(air_bnb$availability.365)] <- mean_availability
print(mean_availability)


#Count Blank Spaces in house rules column
blank_count7 <- sum(air_bnb$house_rules == "")
print(blank_count7)
# Replace blank spaces in 'house_rules' column with 'Not stated'
air_bnb$house_rules[air_bnb$house_rules == ""] <- "Not stated"

# Replace blank spaces in 'license' column with 'Unknown"
air_bnb$license[air_bnb$license == ""] <- "Unknown"

#Counting missing values of lat
sum(is.na(air_bnb$lat))

# Calculate the mean 
mean_lat <- mean(air_bnb$lat, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$lat[is.na(air_bnb$lat)] <- mean_lat
print(mean_lat)

#Counting missing values of long
sum(is.na(air_bnb$long))

# Calculate the mean 
mean_long <- mean(air_bnb$long, na.rm = TRUE)

# Replace NA values with the mean
air_bnb$long[is.na(air_bnb$long)] <- mean_long
print(mean_long)


#Data Visualization for Price Analysis

#Comparing average prices across different neighborhoods, room types
# Load the ggplot2 library for visualization
library(ggplot2)

# Compute average prices across different neighborhoods and room types
average_prices <- aggregate(price ~ neighbourhood.group + room.type, data = air_bnb, FUN = mean)

# Create a heatmap
heatmap_plot <- ggplot(average_prices, aes(x = neighbourhood.group, y = room.type, fill = price)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Average Prices Across Neighborhood Groups and Room Types",
       x = "Neighborhood Group",
       y = "Room Type",
       fill = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the heatmap
print(heatmap_plot)


# Load the ggplot2 library for visualization
library(ggplot2)

# Scatter plot for minimum nights vs. price
scatter_min_nights <- ggplot(air_bnb, aes(x = minimum.nights, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot: Minimum Nights vs. Price",
       x = "Minimum Nights",
       y = "Price")
print(scatter_min_nights)
# Line plot with smoothed lines for availability vs. price
line_availability <- ggplot(air_bnb, aes(x = availability.365, y = price)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Smoothed Line Plot: Availability vs. Price",
       x = "Availability (in days)",
       y = "Price")
print(line_availability)

 #Box plot for cancellation policy vs. price
box_cancellation_policy <- ggplot(air_bnb, aes(x = cancellation_policy, y = price)) +
  geom_boxplot() +  # Using boxplot for categorical variable
  labs(title = "Box Plot: Cancellation Policy vs. Price",
       x = "Cancellation Policy",
       y = "Price")

print(box_cancellation_policy)

#Data Visualization for Host Analysis
# Load required libraries
library(dplyr)
library(ggplot2)
library(magrittr)
# Characteristics of hosts
# Number of listings per host
listings_per_host <- air_bnb %>%
  group_by(host.id) %>%
  summarize(num_listings = n())

# Host identity verification status
verification_status <- air_bnb %>%
  group_by(host_identity_verified) %>%
  summarize(count = n())

# Host activity (number of reviews per host)
reviews_per_host <- air_bnb %>%
  group_by(host.id) %>%
  summarize(total_reviews = sum(number.of.reviews, na.rm = TRUE))

# Visualization
# Bar plot for number of listings per host
bar_plot_listings <- ggplot(listings_per_host, aes(x = num_listings)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Listings per Host",
       x = "Number of Listings",
       y = "Number of Hosts")
print(bar_plot_listings)

# Bar plot for host identity verification status
bar_plot_verification <- ggplot(verification_status, aes(x = host_identity_verified, y = count)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Host Identity Verification Status",
       x = "Verification Status",
       y = "Count") +
  scale_x_discrete(labels = c("Not Verified" = "False", "Verified" = "True"))

print(bar_plot_verification)

# Histogram for host activity (number of reviews per host)
histogram_reviews <- ggplot(reviews_per_host, aes(x = total_reviews)) +
  geom_histogram(fill = "salmon", color = "black", bins = 20) +
  labs(title = "Host Activity: Number of Reviews per Host",
       x = "Number of Reviews",
       y = "Frequency")

print(histogram_reviews)

# Load required libraries
library(ggplot2)

# A box plot comparing hosts and their review rates
boxplot_review_rate <- ggplot(air_bnb, aes(x = host.id, y = review.rate.number)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Comparison of Hosts and Their Review Rates",
       x = "Host ID",
       y = "Review Rate")

# Rotate x-axis labels for better readability
boxplot_review_rate <- boxplot_review_rate + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the box plot
print(boxplot_review_rate)

#Spatial Analysis
# Scatter plot for geographical distribution of listings
scatter_geo_distribution <- ggplot(air_bnb, aes(x = long, y = lat)) +
  geom_point(alpha = 0.5, color = "blue") +  # Adjust alpha for transparency and color for point color
  labs(title = "Geographical Distribution of Listings",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()  # Use minimal theme for better visualization

# Print the scatter plot
print(scatter_geo_distribution)


# Calculate the total number of host listings for each neighborhood group
listings_per_neighborhood <- air_bnb %>%
  group_by(neighbourhood.group) %>%
  summarize(total_listings = sum(calculated.host.listings.count, na.rm = TRUE))

# Create a bar plot
bar_plot_listings_per_neighborhood <- ggplot(listings_per_neighborhood, aes(x = neighbourhood.group, y = total_listings)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Host Listings per Neighborhood Group",
       x = "Neighborhood Group",
       y = "Total Host Listings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Print the bar plot
print(bar_plot_listings_per_neighborhood)

#Text Analysis
library(tm)
library(SnowballC)  # For word stemming
library(wordcloud)  # For word cloud visualization

# Create a corpus from the 'house_rules' column
corpus <- Corpus(VectorSource(air_bnb$house_rules))

# Preprocessing: Convert to lowercase, remove punctuation, and numbers
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Preprocessing: Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Preprocessing: Perform word stemming
corpus <- tm_map(corpus, stemDocument)

# Create a document term matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Convert DTM to a matrix
dtm_matrix <- as.matrix(dtm)

# Calculate word frequencies
word_freq <- colSums(dtm_matrix)

# Create a word cloud
wordcloud(names(word_freq), word_freq, min.freq = 10, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# View top frequent words
top_words <- sort(word_freq, decreasing = TRUE)[1:20]
print(top_words)


#Correlation Matrix
library(dplyr)

# Select numeric columns for correlation analysis
numeric_columns <- air_bnb %>%
  select(lat, long, price, service.fee, minimum.nights, number.of.reviews, reviews.per.month, review.rate.number, calculated.host.listings.count, availability.365)

# Compute correlation matrix
correlation_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")

# Print correlation matrix
print(correlation_matrix)




