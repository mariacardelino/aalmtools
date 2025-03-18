#' UI Module for AALM model parameters
#' @param id Module ID for namespacing
inputUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            box(
                width = 6,
                title = "Basic Parameters",
                status = "primary",
                solidHeader = TRUE,
                
                selectInput(
                    ns("gender"),
                    "Gender", 
                    choices = c("Male", "Female")
                ),
                
                numericInput(
                    ns("age_min"),
                    "Starting Age (years)", 
                    value = 20,
                    min = 0,
                    max = 100
                ),
                
                numericInput(
                    ns("age_max"),
                    "Simulation Duration (years)", 
                    value = 1,
                    min = 0.1,
                    max = 80
                ),
                
                numericInput(
                    ns("steps_day"),
                    "Time Steps per Day", 
                    value = 24,
                    min = 1,
                    max = 48
                )
            ),
            
            box(
                width = 6,
                title = "Physiological Parameters",
                status = "primary",
                solidHeader = TRUE,
                
                numericInput(
                    ns("body_weight"),
                    "Body Weight (kg)", 
                    value = 70,
                    min = 20,
                    max = 150
                ),
                
                numericInput(
                    ns("gi_absorption"),
                    "GI Absorption Factor", 
                    value = 0.12,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                
                numericInput(
                    ns("hematocrit"),
                    "Hematocrit (%)", 
                    value = 45,
                    min = 30,
                    max = 60
                ),
                
                actionButton(
                    ns("use_default"),
                    "Reset to Defaults",
                    icon = icon("refresh")
                )
            )
        ),
        
        fluidRow(
            box(
                width = 12,
                title = "Parameter Information",
                status = "info",
                collapsible = TRUE,
                collapsed = TRUE,
                
                tags$ul(
                    tags$li(strong("Time Steps:"), "Number of calculations per day"),
                    tags$li(strong("GI Absorption:"), "Gastrointestinal absorption fraction"),
                    tags$li(strong("Hematocrit:"), "Percentage of blood volume as red blood cells")
                )
            )
        )
    )
}