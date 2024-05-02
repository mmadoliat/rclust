# Load necessary libraries
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

# Optionally, you could source other scripts if needed for organization.
# Ensure these paths are correct relative to the installed package's structure.
source(system.file("scripts", "GoogleApi.R", package = "GoKidsGoWeb"))
source(system.file("scripts", "danny.R", package = "GoKidsGoWeb"))

# Define UI
ui <- fluidPage(
  titlePanel("GoKidsGoWeb: Route Cluster Map"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("weight", "Weight:", min = 0, max = 1, value = 0.5),
      numericInput("numClusters", "Number of Clusters:", value = 5, min = 1, max = 5, step = 1),
      checkboxGroupInput("selectedRoutes", "Select Routes:", choices = NULL),
      actionButton("updateMap", "Update Map")
    ),
    mainPanel(
      google_mapOutput("mapDisplay", height = "800px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observe({
    route_choices <- seq_len(input$numClusters)
    labels <- paste("Route", route_choices)
    updateCheckboxGroupInput(session, "selectedRoutes", choices = setNames(route_choices, labels), selected = route_choices)
  })
  
  observeEvent(input$updateMap, {
    output$mapDisplay <- renderGoogle_map({
      if (!is.null(input$selectedRoutes) && length(input$selectedRoutes) > 0) {
        displayRoutes(input$weight, input$numClusters, as.numeric(input$selectedRoutes))
      } else {
        google_map()
      }
    })
  })
}

# .onLoad function to launch the app when the package is loaded
.onLoad <- function(libname, pkgname) {
  shinyApp(ui = ui, server = server)
}
