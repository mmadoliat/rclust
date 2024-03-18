load("gokidgoweb.Rda")
library(readr)

z_score_standardization <- function(x) {
  return ((x - mean(x)) / sd(x))
}

normalized_distance <- z_score_standardization(route_data$distance)
normalized_time <- z_score_standardization(route_data$time)

combined_metric <- data.frame(normalized_distance, normalized_time)

hc <- hclust(dist(normalized_time), method = "complete")

plot(hc)
plot(hc$height, type = "b")

# Read the geolocation data
geolocations <- read.csv("Geolocations.csv")

# Filter out points not in Wisconsin
geolocations_filtered <- geolocations %>%
  filter(latitude > 42, latitude < 47, longitude > -93, longitude < -86)

route_data$cluster <- cutree(hc, k = 7)

#add long and lat data to route_data
route_data$latitude <- geolocations_filtered$latitude
route_data$longitude <- geolocations_filtered$longitude

#finding cluster centroids(mean of lat and long for each cluster)
final_data <- aggregate(route_data, by=list(cluster=route_data$cluster), mean)
