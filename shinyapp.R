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
library(shinythemes)

source("GoogleApi.R")
source("danny.R")


ui <- fluidPage(
  titlePanel("GoKidsGoWeb: Route Cluster Map"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("weight", "Weight:", 
                  min = 0, max = 1, value = 0.5),
      numericInput("numClusters", "Number of Clusters:", 
                   value = 5, min = 1, max = 5, step = 1),
      checkboxGroupInput("selectedRoutes", "Select Routes:", 
                         choices = NULL),  # Choices will be updated dynamically
      actionButton("updateMap", "Update Map")
    ),
    mainPanel(
      google_mapOutput("mapDisplay", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  # Automatically update choices for selectedRoutes based on numClusters
  observe({
    route_choices <- seq_len(input$numClusters)
    labels <- paste("Route", route_choices)  # Create labels such as Route 1, Route 2, etc.
    
    updateCheckboxGroupInput(session, "selectedRoutes", 
                             choices = setNames(route_choices, labels),  # Assign names to choices
                             selected = route_choices)
  })
  
  # Update map display when updateMap button is clicked
  observeEvent(input$updateMap, {
    output$mapDisplay <- renderGoogle_map({
      # Call displayRoutes with all routes selected by default
      if (!is.null(input$selectedRoutes) && length(input$selectedRoutes) > 0) {
        displayRoutes(input$weight, input$numClusters, as.numeric(input$selectedRoutes))
      } else {
        google_map()  # Return an empty map if no routes are selected
      }
    })
  })
}


shinyApp(ui = ui, server = server)
