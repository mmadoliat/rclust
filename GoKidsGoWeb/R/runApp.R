#' Run the GoKidsGoWeb Shiny Application
#'
#' This function starts the Shiny application contained within the GoKidsGoWeb package.
#' It assumes that the application is stored within the 'shinyApp' directory in the 'inst' folder,
#' under the name 'app.R', which includes both the UI and server components of the Shiny app.
#'
#' @importFrom shiny runApp
#' @export

runAppGoKidsGo <- function() {
  # Use system.file to construct the path to the 'app.R' file within the installed package
  appDir <- system.file("shinyApp", package = "GoKidsGoWeb")
  
  # Check if the directory path is valid
  if (appDir == "") {
    stop("Shiny application directory not found in the package.")
  }
  
  # Run the Shiny app from the specified directory
  shiny::runApp(appDir)
}