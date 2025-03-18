#' UI Module for AALM Model Parameters
#' @param id Module ID for namespacing

paramsUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            # Basic Parameters Box
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
                ),
                
                helpText("Note: Age inputs will be converted to days for AALM model")
            ),
            
            # Physiological Parameters Box
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
                    "Use Default Values",
                    icon = icon("refresh")
                )
            )
        ),
        
        # Advanced Settings Panel
        fluidRow(
            box(
                width = 12,
                title = "Advanced Settings",
                status = "info",
                collapsible = TRUE,
                collapsed = TRUE,
                
                fluidRow(
                    column(6,
                        selectInput(
                            ns("rbc_model"),
                            "RBC Saturation Model",
                            choices = c(
                                "None" = 0,
                                "Saturable" = 1
                            )
                        )
                    ),
                    column(6,
                        numericInput(
                            ns("rbc_power"),
                            "RBC Power Parameter",
                            value = 1.5,
                            min = 0,
                            max = 5
                        )
                    )
                )
            )
        ),
        
        # Parameter Documentation
        fluidRow(
            box(
                width = 12,
                title = "Parameter Information",
                status = "primary",
                collapsible = TRUE,
                
                tags$div(
                    class = "param-info",
                    tags$h4("Model Parameters"),
                    tags$ul(
                        tags$li(strong("Starting Age:"), "Age at beginning of simulation"),
                        tags$li(strong("Duration:"), "Length of simulation period"),
                        tags$li(strong("Time Steps:"), "Computational resolution per day"),
                        tags$li(strong("GI Absorption:"), "Gastrointestinal absorption fraction"),
                        tags$li(strong("Hematocrit:"), "Percentage of blood volume occupied by red blood cells"),
                        tags$li(strong("RBC Model:"), "Type of red blood cell saturation model to use")
                    )
                )
            )
        )
    )
}