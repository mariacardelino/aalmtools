#' Server logic for media concentrations
#' @param id Module ID for namespacing
mediaServer <- function(id) {
    const <- AALM_constants
    
    moduleServer(id, function(input, output, session) {
        # Default media values
        default_media <- list(
            soil = list(conc = 200, rba = 0.6),
            dust = list(conc = 100, rba = 0.6),
            water = list(conc = 5, rba = 1.0),
            air = list(conc = 0.1),
            food = list(conc = 1, rba = 1.0)
        )
        
        # Reactive values for media parameters
        media_values <- reactiveVal(default_media)
        
        # Reset to defaults
        observeEvent(input$reset_media, {
            media_values(default_media)
            
            # Update UI
            updateNumericInput(session, "soil_conc", value = default_media$soil$conc)
            updateNumericInput(session, "soil_rba", value = default_media$soil$rba)
            updateNumericInput(session, "dust_conc", value = default_media$dust$conc)
            updateNumericInput(session, "dust_rba", value = default_media$dust$rba)
            updateNumericInput(session, "water_conc", value = default_media$water$conc)
            updateNumericInput(session, "air_conc", value = default_media$air$conc)
            updateNumericInput(session, "food_conc", value = default_media$food$conc)
        })
        
        # Validate media inputs
        observe({
            validate(
                need(input$soil_conc >= 0, "Soil concentration must be non-negative"),
                need(input$dust_conc >= 0, "Dust concentration must be non-negative"),
                need(input$water_conc >= 0, "Water concentration must be non-negative"),
                need(input$air_conc >= 0, "Air concentration must be non-negative"),
                need(input$food_conc >= 0, "Food concentration must be non-negative"),
                need(input$soil_rba >= 0 && input$soil_rba <= 1, "Soil RBA must be between 0 and 1"),
                need(input$dust_rba >= 0 && input$dust_rba <= 1, "Dust RBA must be between 0 and 1")
            )
        })
        
        # Return reactive expression with validated media parameters
        return(reactive({
            list(
                soil = list(
                    conc = input$soil_conc,  # µg/g
                    rba = input$soil_rba     # fraction
                ),
                dust = list(
                    conc = input$dust_conc,  # µg/g
                    rba = input$dust_rba     # fraction
                ),
                water = list(
                    conc = input$water_conc, # µg/L
                    rba = 1.0                # fraction
                ),
                air = list(
                    conc = input$air_conc    # µg/m³
                ),
                food = list(
                    conc = input$food_conc,  # µg/day
                    rba = 1.0                # fraction
                )
            )
        }))
    })
}