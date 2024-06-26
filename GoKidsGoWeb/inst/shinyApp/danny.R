library(readr)
library(ggplot2)
library(ggmap)
library(ggforce) 
library(dplyr)
library(googleway)
library(httr)
library(jsonlite)

displayRoutes <- function(weight,numClusters,selectedRoutes) { #main function (used in shiny)
  load("pdist.Rda")
  load("ptime.Rda")
  load("data.Rda")
  
  hc <- test::rhclust(pdist, ptime, weight, data = data)
  api_key <- "AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw"
  locations <- getDetails(hc,numClusters,data) #returns list of 3 types of location
  all_routes_directions <- get_all_directions(locations, api_key)
  
  flat_waypoints <- unlist(locations$waypoints, recursive = FALSE)
  waypoint_types <- rep("waypoint", length(flat_waypoints))
  origin_types <- rep("origin", length(locations$origins))
  destination_types <- rep("destination", length(locations$destinations))
  all_addresses <- c(locations$origins, locations$destinations, flat_waypoints)
  
  
  all_types <- c(origin_types, destination_types, waypoint_types)
  geo_data_df <- suppressWarnings(geocode_address(all_addresses, all_types, api_key))
  
  # Initialize a map using the central location from the geocoded data
  central_location <- suppressWarnings(geo_data_df[1, c("latitude", "longitude")])
  map <- google_map(location = central_location, key = api_key, zoom = 10)
  colors <- c("red", "blue", "green", "purple", "orange", "brown", "black", "grey", "pink")
  hex_colors <- rgb(col2rgb(colors)/255, maxColorValue=1)
  # Loop through each route's directions and add to the map
  for (i in selectedRoutes) { #selected routes is chosen by user
    if (i <= length(all_routes_directions)) {
      directions <- all_routes_directions[[i]]
      #formating maps api object
      polyline_data <- decode_pl(directions$routes$overview_polyline$points)
      polyline_df <- as.data.frame(polyline_data)
      names(polyline_df) <- c("lat", "lng")
      polyline_df$id <- paste("route", i, sep = "_")
      
      route_color <- hex_colors[i %% length(hex_colors) + 1] #different color per route
      map <- add_polylines(map, data = polyline_df, lat = "lat", lon = "lng", #Create routes
                           id = "id", stroke_colour = route_color, stroke_weight = 4)
    }
  }
  blue_icon <- list(
    url = "http://maps.google.com/mapfiles/ms/icons/blue-dot.png"  # URL to a custom marker icon
  )
  
  green_icon <- list(
    url = "http://maps.google.com/mapfiles/ms/icons/green-dot.png"
  )
  
  red_icon <- list(
    url = "http://maps.google.com/mapfiles/ms/icons/red-dot.png"
  )
  
  
  # Add markers for the geocoded locations
  for (i in 1:nrow(geo_data_df)) {
    icon_url <- switch(geo_data_df$type[i],
                       "origin" = green_icon$url,
                       "waypoint" = blue_icon$url,
                       "destination" = red_icon$url,
                       red_icon$url) # default to red if no type matches
    
    map <- add_markers(
      map,
      data = geo_data_df[i, , drop = FALSE],
      lat = "latitude",
      lon = "longitude",
      marker_icon = list(url = icon_url)
    )
  }
  return (map)
}

findLocation <- function(loc, data) {
  first_char <- substr(loc, 1, 1)
  num <- as.numeric(substr(loc, 2, nchar(loc)))
  if (first_char == "p") { # If it starts with 'p', use 'num' to index 'formatted_home'
    location <- data$formatted_home[num] #order of data is same as route num like p1 & d1 is first data point
  } else if (first_char == "d") { # If it starts with 'd', use 'num' to index 'formatted_destination'
    location <- data$formatted_destination[num]
  }
  return (location)
}

getDetails <- function(hc, numClusters, data) {
  origins <- list()
  destinations <- list()
  waypoint_vectors <- list()
  if (numClusters == 3) {
    routeNum = c(1,6,7)  # cluster #'s 1, 6, and 7
    for (i in 1:3) {
      origins[[i]] <- findLocation(hc$merge.route[[routeNum[i]]][[1]], data)
      if (i == 1) {
        destinations[[i]] <- findLocation(hc$merge.route[[routeNum[i]]][[4]], data)
        waypoint_vectors[[i]] <- lapply(hc$merge.route[[routeNum[i]]][2:3], findLocation, data)
      } else {
        destinations[[i]] <- findLocation(hc$merge.route[[routeNum[i]]][[8]], data)
        waypoint_vectors[[i]] <- lapply(hc$merge.route[[routeNum[i]]][2:7], findLocation, data)
      }
    }
  } else if (numClusters == 2) {
    routeNum = c(1,8)
    for (i in 1:2) {
      origins[[i]] <- findLocation(hc$merge.route[[routeNum[i]]][[1]], data)
      if (i == 1) {
        destinations[[i]] <- findLocation(hc$merge.route[[routeNum[i]]][[4]], data)
        waypoint_vectors[[i]] <- lapply(hc$merge.route[[routeNum[i]]][2:3], findLocation, data)
      } else {
        destinations[[i]] <- findLocation(hc$merge.route[[routeNum[i]]][[16]], data)
        waypoint_vectors[[i]] <- lapply(hc$merge.route[[routeNum[i]]][2:15], findLocation, data)
      }
    }
  } else if (numClusters == 1) {
    origins[[1]] <- findLocation(hc$merge.route[[9]][[1]], data)
    destinations[[1]] <- findLocation(hc$merge.route[[9]][[20]], data)
    waypoint_vectors[[1]] <- lapply(hc$merge.route[[9]][2:19], findLocation, data)
  } else {
    for (i in 1:5) {
      origins[[i]] <- findLocation(hc$merge.route[[i]][[1]], data)
      destinations[[i]] <- findLocation(hc$merge.route[[i]][[4]], data)
      waypoint_vectors[[i]] <- lapply(hc$merge.route[[i]][2:3], findLocation, data)
    }
  }
  return(list(origins = origins, destinations = destinations, waypoints = waypoint_vectors))
}


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

geocode_address <- function(address, types, api_key) {
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  latitudes <- numeric(length = length(address))
  longitudes <- numeric(length = length(address))
  geo_data_df <- data.frame(address = address, latitude = numeric(length(address)), longitude = numeric(length(address)), type = types)
  
  for (i in 1:length(address)) {
    response <- GET(url = base_url, query = list(address = address[i], key = api_key))
    parsed_content <- fromJSON(content(response, "text"))
    geo_data_df$latitude[i] <- parsed_content$results$geometry$location$lat
    geo_data_df$longitude[i] <- parsed_content$results$geometry$location$lng
  }
  return (geo_data_df)
}






