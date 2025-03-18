#' UI Module for AALM intake rates
#' @param id Module ID for namespacing
intakeUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            box(
                width = 12,
                title = "Daily Intake Rates",
                status = "primary",
                solidHeader = TRUE,
                
                fluidRow(
                    column(6,
                        # Soil and Dust
                        numericInput(
                            ns("soil_ir"),
                            "Soil Intake (g/day)",
                            value = 0.05,
                            min = 0,
                            max = 0.2,
                            step = 0.01
                        ),
                        numericInput(
                            ns("dust_ir"),
                            "Dust Intake (g/day)",
                            value = 0.05,
                            min = 0,
                            max = 0.2,
                            step = 0.01
                        )
                    ),
                    column(6,
                        # Water and Air
                        numericInput(
                            ns("water_ir"),
                            "Water Intake (L/day)",
                            value = 2.0,
                            min = 0,
                            max = 5,
                            step = 0.1
                        ),
                        numericInput(
                            ns("air_ir"),
                            "Air Intake (m³/day)",
                            value = 20.0,
                            min = 0,
                            max = 50,
                            step = 1.0
                        )
                    )
                ),
                
                fluidRow(
                    column(6,
                        # Food
                        numericInput(
                            ns("food_ir"),
                            "Food Intake (g/day)",
                            value = 2000.0,
                            min = 0,
                            max = 5000,
                            step = 100
                        )
                    ),
                    column(6,
                        # Reset button
                        br(),
                        actionButton(
                            ns("reset_intake"),
                            "Reset to Defaults",
                            icon = icon("refresh")
                        )
                    )
                )
            )
        ),
        
        # Help text
        fluidRow(
            box(
                width = 12,
                status = "info",
                title = "Intake Rate Information",
                collapsible = TRUE,
                collapsed = TRUE,
                
                tags$ul(
                    tags$li("Soil and dust intakes should total less than 0.2 g/day"),
                    tags$li("Water intake based on standard consumption of 2 L/day"),
                    tags$li("Air intake assumes moderate activity level"),
                    tags$li("Food intake varies by body weight and activity level")
                )
            )
        )
    )
}