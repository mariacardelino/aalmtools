#' Server logic for intake rates
#' @param id Module ID for namespacing
intakeServer <- function(id) {
    const <- AALM_constants
    
    moduleServer(id, function(input, output, session) {
        # Default intake rates
        default_intake <- list(
            soil = 0.05,    # g/day
            dust = 0.05,    # g/day
            water = 2.0,    # L/day
            air = 20.0,     # m³/day
            food = 2000.0   # g/day
        )
        
        # Reactive values for intake rates
        intake_values <- reactiveVal(default_intake)
        
        # Reset to defaults
        observeEvent(input$reset_intake, {
            intake_values(default_intake)
            
            # Update UI
            updateNumericInput(session, "soil_ir", value = default_intake$soil)
            updateNumericInput(session, "dust_ir", value = default_intake$dust)
            updateNumericInput(session, "water_ir", value = default_intake$water)
            updateNumericInput(session, "air_ir", value = default_intake$air)
            updateNumericInput(session, "food_ir", value = default_intake$food)
        })
        
        # Validate intake rates
        observe({
            validate(
                need(input$soil_ir >= 0, "Soil intake must be non-negative"),
                need(input$dust_ir >= 0, "Dust intake must be non-negative"),
                need(input$water_ir >= 0, "Water intake must be non-negative"),
                need(input$air_ir >= 0, "Air intake must be non-negative"),
                need(input$food_ir >= 0, "Food intake must be non-negative")
            )
            
            # Validate total soil+dust intake
            total_soil_dust <- input$soil_ir + input$dust_ir
            if(total_soil_dust > 0.2) {
                showNotification(
                    "Warning: Total soil and dust intake exceeds 0.2 g/day",
                    type = "warning"
                )
            }
        })
        
        # Return reactive expression with validated intake rates
        return(reactive({
            list(
                soil = input$soil_ir,
                dust = input$dust_ir,
                water = input$water_ir,
                air = input$air_ir,
                food = input$food_ir
            )
        }))
    })
}