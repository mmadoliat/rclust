library(readr)
library(ggplot2)
library(ggmap)
library(ggforce) 
library(dplyr)
library(googleway)
library(httr)
library(jsonlite)

load("pdist.Rda")
load("ptime.Rda")
load("data.Rda")

hc <- test::rhclust(pdist, ptime, 0.5, data = data)

plot(hc)

set_key("AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw")
api_key <- "AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw"

findLocation <- function(loc) {
  first_char <- substr(loc, 1, 1)
  num <- as.numeric(substr(loc, 2, nchar(loc)))
  if (first_char == "p") { # If it starts with 'p', use 'num' to index 'formatted_home'
    location <- data$formatted_home[num]
  } else if (first_char == "d") { # If it starts with 'd', use 'num' to index 'formatted_destination'
    location <- data$formatted_destination[num]
  }
  return (location)
}

getDetails <- function(numClusters) {
  if (numClusters == 5) {
    origins <- list()
    destinations <- list()
    
    # Loop over each sublist to extract origins and destinations
    for (i in 1:5) {
      # Extract the origin from the first element if it exists
      if (length(hc$merge.route[[i]]) >= 1) {
        origins[[i]] <- findLocation(hc$merge.route[[i]][[1]])
      } else {
        origins[[i]] <- NULL  # Placeholder in case there's no first element
      }
      
      # Extract the destination from the fourth element if it exists
      if (length(hc$merge.route[[i]]) >= 4) {
        destinations[[i]] <- findLocation(hc$merge.route[[i]][[4]])
      } else {
        destinations[[i]] <- NULL  # Placeholder in case there's no fourth element
      }
    }
   
    waypoint_vectors <- list()
    
    # Loop over each of the first five sublists
    for (i in 1:5) {
      # Check if the sublist has at least three elements to avoid indexing errors
      if (length(hc$merge.route[[i]]) >= 3) {
        # Create a vector of the second and third elements using findLocation
        waypoints <- list(findLocation(hc$merge.route[[i]][[2]]), findLocation(hc$merge.route[[i]][[3]]))
        # Add this vector to the list
        waypoint_vectors[[i]] <- waypoints
      } else {
        # If there are not enough elements in the sublist, add a NULL or suitable placeholder
        waypoint_vectors[[i]] <- NULL
      }
    }
    return(list(origins = origins, destinations = destinations, waypoints = waypoint_vectors))
  }
}
locations <- getDetails(5)
directions2 <- google_directions(origin = locations$origins[[1]], 
                                destination = locations$destinations[[1]],
                                mode = "driving",
                                waypoints = locations$waypoints[[1]],
                                alternatives = TRUE,
                                key = api_key)

get_all_directions <- function(locations, api_key) {
  all_directions <- list()
  
  for (i in seq_along(locations$origins)) {
    
    # Get directions
    directions <- google_directions(
      origin = locations$origins[[i]],
      destination = locations$destinations[[i]],
      waypoints = locations$waypoints[[i]],
      mode = "driving",
      alternatives = TRUE,
      key = api_key
    )
    
    all_directions[[i]] <- directions
  }
  
  return(all_directions)
}

all_routes_directions <- get_all_directions(locations, api_key)

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

flat_waypoints <- unlist(locations$waypoints, recursive = FALSE)
all_addresses <- c(locations$origins, locations$destinations, flat_waypoints)
all_addresses <- all_addresses[!sapply(all_addresses, is.null)]
geo_data_df <- geocode_address(all_addresses, api_key)

# Initialize a map using the central location from the geocoded data
central_location <- geo_data_df[1, c("latitude", "longitude")]
map <- google_map(location = central_location, key = api_key, zoom = 10)
colors <- c("red", "blue", "green", "purple", "orange", "brown", "black", "grey", "pink")
hex_colors <- rgb(col2rgb(colors)/255, maxColorValue=1)
# Loop through each route's directions and add to the map
for (i in seq_along(all_routes_directions)) {
  directions <- all_routes_directions[[i]]
  if (!is.null(directions) && !is.null(directions$routes)) {
    # Decode and add the polyline for each route
    polyline_data <- decode_pl(directions$routes$overview_polyline$points)
    polyline_df <- as.data.frame(polyline_data)
    names(polyline_df) <- c("lat", "lng")
    
    # We generate a unique ID for each polyline
    polyline_df$id <- paste("route", i, sep = "_")
    route_color <- hex_colors[i %% length(hex_colors) + 1]
    map <- add_polylines(map, data = polyline_df, lat = "lat", lon = "lng", 
                         id = "id", stroke_colour = route_color, stroke_weight = 4)
  }
}

# Add markers for the geocoded locations, assuming the geo_data_df has columns 'latitude' and 'longitude'
map <- add_markers(map, data = geo_data_df, lat = "latitude", lon = "longitude")

# Print the map to display it
print(map)
