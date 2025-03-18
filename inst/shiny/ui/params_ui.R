#' UI Module for AALM Model Parameters
#' @param id Module ID for namespacing
paramsUI <- function(id) {
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
                    ns("blood_volume"),
                    "Blood Volume (L)",
                    value = 5.2,
                    min = 2,
                    max = 8
                ),
                
                selectInput(
                    ns("activity_level"),
                    "Activity Level",
                    choices = c(
                        "Sedentary" = "low",
                        "Moderate" = "medium",
                        "Active" = "high"
                    )
                )
            )
        ),
        
        fluidRow(
            box(
                width = 12,
                title = "Additional Settings",
                status = "info",
                collapsible = TRUE,
                collapsed = TRUE,
                
                checkboxGroupInput(
                    ns("outputs"),
                    "Output Options",
                    choices = c(
                        "Blood Lead" = "blood",
                        "Bone Lead" = "bone",
                        "Soft Tissue Lead" = "tissue",
                        "Excretion" = "excretion"
                    ),
                    selected = c("blood", "bone")
                ),
                
                selectInput(
                    ns("output_freq"),
                    "Output Frequency",
                    choices = c(
                        "Daily" = 1,
                        "Weekly" = 7,
                        "Monthly" = 30,
                        "Yearly" = 365
                    )
                )
            )
        ),
        
        # Help text with parameter descriptions
        tags$div(
            class = "help-text",
            tags$h4("Parameter Information"),
            tags$ul(
                tags$li("Starting Age: Age at beginning of simulation"),
                tags$li("Duration: Length of simulation period"),
                tags$li("Time Steps: Computational resolution per day"),
                tags$li("GI Absorption: Gastrointestinal absorption fraction")
            )
        )
    )
}