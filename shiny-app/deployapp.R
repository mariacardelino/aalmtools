# Deployment version - wrapper with error handling
options(shiny.port = 3838)
options(shiny.host = '0.0.0.0')

# Ensure package is available
if (!require(aalmtools)) {
  stop("aalmtools package is required. Please install it first.")
}

# Load the app from package
shiny::runApp(
  system.file("shiny", package = "aalmtools"),
  launch.browser = FALSE
)