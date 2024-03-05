library(httr)
library(jsonlite)

# Function to geocode address using Google Maps API
geocode_address <- function(address, api_key) {
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  latitudes <- numeric(length = length(address))
  longitudes <- numeric(length = length(address))
  for (i in 1:length(address)) {
    response <- GET(url = base_url, query = list(address = address[i], key = api_key))
    parsed_content <- fromJSON(content(response, "text"))
    lat <- parsed_content$results$geometry$location$lat
    lng <- parsed_content$results$geometry$location$lng
    latitudes[i] <- lat
    longitudes[i] <- lng
    
  }
  geo_data_df <- data.frame(address = address, latitude = latitudes, longitude = longitudes)
  return (geo_data_df)
}

address <- route_data$destinations
api_key <- ("Apikey")
geo_data <- geocode_address(address, api_key)

write.csv(geo_data,"Geolocations.csv")

