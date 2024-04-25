library(shiny)
library("gmapsdistance")
library(readr)
library(ggplot2)
library(ggmap)
library(ggforce) 
library(dplyr)
library(googleway)
library(httr)
library(jsonlite)

source("GoogleApi.R")
source("danny.R")

# map <- displayRoutes(.5, 5)
# 
# ui <- fluidPage(
#   titlePanel("Shiny Map App"),
#   mainPanel(
#     google_mapOutput("map")
#   )
# )
# 
# server <- function(input, output) {
#   output$map <- renderGoogle_map({
#     map <- add_markers(
#       map,
#       data = geo_data_df[i, , drop = FALSE],
#       lat = "latitude",
#       lon = "longitude",
#       marker_icon = list(url = icon_url)
#     )
#     map
#   })
# }

ui <- fluidPage(
  titlePanel("Dynamic Route Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("weight", "Weight:", 
                  min = 0, max = 1, value = 0.5),
      numericInput("numClusters", "Number of Clusters:", 
                   value = 1, min = 1, max = 5, step = 1),
      actionButton("updateMap", "Update Map")
    ),
    mainPanel(
      google_mapOutput("mapDisplay", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  # Reactive function to trigger map update
  mapData <- eventReactive(input$updateMap, {
    displayRoutes(input$weight, input$numClusters)
  })
  # Render the map
  output$mapDisplay <- renderGoogle_map({
    mapData()
  })
}

# Ensure the 'displayRoutes' function and its dependencies are loaded or defined in your script.


shinyApp(ui = ui, server = server)
