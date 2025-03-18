#' UI Module for AALM media concentrations
#' @param id Module ID for namespacing
mediaUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            box(
                width = 12,
                title = "Media Lead Concentrations",
                status = "primary",
                solidHeader = TRUE,
                
                fluidRow(
                    column(4,
                        # Soil
                        numericInput(
                            ns("soil_conc"),
                            "Soil Lead (µg/g)",
                            value = 200,
                            min = 0
                        ),
                        numericInput(
                            ns("soil_rba"),
                            "Soil RBA",
                            value = 0.6,
                            min = 0,
                            max = 1,
                            step = 0.1
                        )
                    ),
                    column(4,
                        # Dust
                        numericInput(
                            ns("dust_conc"),
                            "Dust Lead (µg/g)",
                            value = 100,
                            min = 0
                        ),
                        numericInput(
                            ns("dust_rba"),
                            "Dust RBA",
                            value = 0.6,
                            min = 0,
                            max = 1,
                            step = 0.1
                        )
                    ),
                    column(4,
                        # Water and Air
                        numericInput(
                            ns("water_conc"),
                            "Water Lead (µg/L)",
                            value = 5,
                            min = 0
                        ),
                        numericInput(
                            ns("air_conc"),
                            "Air Lead (µg/m³)",
                            value = 0.1,
                            min = 0,
                            step = 0.01
                        )
                    )
                ),
                
                fluidRow(
                    column(4,
                        # Food
                        numericInput(
                            ns("food_conc"),
                            "Food Lead (µg/day)",
                            value = 1,
                            min = 0
                        )
                    ),
                    column(4,
                        # Reset button
                        br(),
                        actionButton(
                            ns("reset_media"),
                            "Reset to Defaults",
                            icon = icon("refresh")
                        )
                    )
                )
            )
        ),
        
        # Tooltips and help
        fluidRow(
            box(
                width = 12,
                status = "info",
                title = "Media Information",
                collapsible = TRUE,
                collapsed = TRUE,
                
                tags$ul(
                    tags$li("RBA = Relative Bioavailability"),
                    tags$li("Water and food RBA are fixed at 1.0"),
                    tags$li("Default values based on typical exposure scenarios"),
                    tags$li("Air concentration assumes PM10 fraction")
                )
            )
        )
    )
}