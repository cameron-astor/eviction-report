# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)

# Compute some values of interest and store them in variables for the report

# How many evictions were there?
num_evictions <- nrow(evictions)
num_features <- ncol(evictions)

# Create a table (data frame) of evictions by zip code (sort descending)
evictions_by_zipcode <- evictions %>% 
  group_by(Eviction.Notice.Source.Zipcode) %>% 
  count() %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  top_n(10, wt = n) #top ten results

# Create a plot of the number of evictions each month in the dataset
by_month <- evictions %>% 
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>% 
  mutate(month = floor_date(date, unit = "month")) %>% 
  group_by(month) %>% 
  count()

# Store plot in a variable
evictions_sf_month <- ggplot(data = by_month) + 
  geom_line(mapping = aes(x = month, y = n)) +
  labs(x = "Date", y = "Number of Evictions", title = "Evictions over time in San Francisco")

# Map evictions in 2017 


# Format the lat/long variables, filter to 2017

# Create a maptile background

# Add a layer of points on top of the map tiles
