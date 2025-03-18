library(shiny)
library(shinydashboard)
library(DT)
library(aalmtools)
library(rjags)
library(coda)

options(shiny.error = function() {
    stop("An error occurred. Please check your inputs and try again.")
})

# Load UI modules
source("ui/params_ui.R")
source("ui/media_ui.R")
source("ui/intake_ui.R")
source("ui/results_ui.R")
source("ui/bayesian_ui.R")
# Load server modules
source("server/params_server.R")
source("server/media_server.R")
source("server/intake_server.R")
source("server/results_server.R")
source("server/bayesian_server.R")

# Main UI
ui <- dashboardPage(
    dashboardHeader(title = "AALM Model Interface"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Model Parameters", tabName = "params", icon = icon("sliders")),
            menuItem("Media Concentrations", tabName = "media", icon = icon("flask")),
            menuItem("Intake Rates", tabName = "intake", icon = icon("arrow-down")),
            menuItem("Results", tabName = "results", icon = icon("chart-line")),
            menuItem("Bayesian Analysis", tabName = "bayes", icon = icon("chart-bar"))
        )
    ),
    
    dashboardBody(
        tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
        tabItems(
            tabItem(tabName = "params", paramsUI("params")),
            tabItem(tabName = "media", mediaUI("media")),
            tabItem(tabName = "intake", intakeUI("intake")),
            tabItem(tabName = "results", resultsUI("results")),
            tabItem(tabName = "bayes", bayesianUI("bayes"))
        )
    )
)

# Main server
server <- function(input, output, session) {
    # Initialize reactive values for global state
    model_state <- reactiveValues(
        running = FALSE,
        error = NULL
    )
    # Initialize modules
    params_data <- tryCatch({
        paramsServer("params")
        }, error = function(e) {
        model_state$error <- e$message
        NULL
    })
    media_data <- tryCatch({mediaServer("media")
    }, error = function(e) {
        model_state$error <- e$message
        NULL
        })
    intake_data <-  tryCatch({intakeServer("intake")
    }, error = function(e) {
        model_state$error <- e$message
        NULL
        })
    results_data <-  tryCatch({resultsServer("results", 
                                params_data = params_data,
                                media_data = media_data,
                                intake_data = intake_data)
                                }, error = function(e) {
                                    model_state$error <- e$message
                                    NULL
                                })
    # Check for errors in module initialization
    bayes_data <- tryCatch({bayesianServer("bayes", 
                  params_data = params_data,
                  media_data = media_data)
                  }, error = function(e) {
                                    model_state$error <- e$message
                                    NULL
                                })
}

shinyApp(ui = ui, server = server)