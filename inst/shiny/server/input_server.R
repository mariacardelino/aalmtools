#' Server logic for AALM parameter inputs
#' @param id Module ID for namespacing
paramsServer <- function(id) {
     # Import AALM constants
    const <- AALM_constants

    moduleServer(id, function(input, output, session) {
        # Default values
        default_params <- list(
            gender = "Male",
            age_min = 20,
            age_max = 1,
            steps_day = 24,
            body_weight = 70,
            gi_absorption = 0.12,
            hematocrit = 45,
            rbc_model = 1,
            rbc_power = 1.5
        )
        
        # Reactive values to store parameter state
        params <- reactiveVal(default_params)
        
        # Reset to defaults when button clicked
        observeEvent(input$use_default, {
            params(default_params)
            
            # Update UI inputs
            updateSelectInput(session, "gender", selected = default_params$gender)
            updateNumericInput(session, "age_min", value = default_params$age_min)
            updateNumericInput(session, "age_max", value = default_params$age_max)
            updateNumericInput(session, "steps_day", value = default_params$steps_day)
            updateNumericInput(session, "body_weight", value = default_params$body_weight)
            updateNumericInput(session, "gi_absorption", value = default_params$gi_absorption)
            updateNumericInput(session, "hematocrit", value = default_params$hematocrit)
            updateSelectInput(session, "rbc_model", selected = default_params$rbc_model)
            updateNumericInput(session, "rbc_power", value = default_params$rbc_power)
        })
        
        # Validate inputs
        observe({
            # Age validation
            if (input$age_min + input$age_max > 100) {
                showNotification(
                    "Total simulation age cannot exceed 100 years",
                    type = "warning"
                )
            }
            
            # Physiological parameter validation
            validate(
                need(input$body_weight >= 20, "Body weight must be at least 20 kg"),
                need(input$gi_absorption <= 1, "GI absorption must be <= 1"),
                need(input$hematocrit >= 30 && input$hematocrit <= 60,
                     "Hematocrit must be between 30-60%")
            )
        })
        
        # Return reactive expression with validated parameters
        reactive({
            list(
                # Basic parameters
                gender = input$gender,
                age_min = input$age_min * 365, # Convert to days
                age_max = (input$age_min + input$age_max) * 365,
                steps_day = input$steps_day,
                
                # Physiological parameters
                body_weight = input$body_weight,
                gi_absorption = input$gi_absorption,
                hematocrit = input$hematocrit/100, # Convert to fraction
                
                # Advanced settings
                rbc_model = as.numeric(input$rbc_model),
                rbc_power = input$rbc_power
            )
        })

         # Add validation for steps_day based on AALM constants
        observe({
            validate(
                need(input$steps_day <= const$max_steps_day, 
                     paste("Steps per day cannot exceed", const$max_steps_day))
            )
        })
    })
}