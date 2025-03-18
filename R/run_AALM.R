#' Main entry point for AALM model
#' Direct port of Fortran PROGRAM Leg16
#' @param parFile Path to parameter file
#' @param outputDir Output directory
#' @param verbose Print debug info
#' 
#' #' @return List containing model results:
#' \itemize{
#'   \item Y - Compartment amounts
#'   \item flows - Flow rates between compartments
#'   \item TS - Time series results
#'   \item MB - Mass balance information
#' }
#'  
#' @examples
#' \dontrun{
#' results <- run_AALM("input/params.txt", "output", verbose=TRUE)
#' }
#' 
#' @export
run_AALM <- function(parFile = "LeggettInput.txt", outputDir = NULL, verbose = FALSE) {
  # Initialize global model state
  state <- list(
    sim = list(),           # Simulation parameters
    PC = NULL,              # Physiological constants
    PV = NULL,              # Time-varying parameters
    Y = NULL,               # Compartment amounts
    flows = NULL,           # Flow rates
    TS = NULL,             # Time series results
    Intakes = NULL,        # Intake tracking
    Uptakes = NULL,        # Uptake tracking
    Excrete = NULL,        # Excretion tracking
    MB = NULL,             # Mass balance
    debug = verbose
  )
  
  # Initialize lung parameters for 3 source types
  state$sim$lung <- vector("list", 3)
  for(i in 1:3) {
    state$sim$lung[[i]] <- list(
      RLTBplas = 0, RLTBLET = 0,
      RLalvPlas = 0, RLalvLTB = 0, 
      RLalvLint = 0, RLintPlas = 0,
      DepFracLET = 0, DepFracLTB = 0,
      DepFracLalv = 0
    )
  }
  
  # Load constants
  const <- AALM_constants
  
  # Set up output directory
  if(is.null(outputDir)) {
    outputDir <- getwd()
  }
  
  # Debug output
  if(state$debug) {
    cat("AALM Model Version", const$version, "\n")
    cat("Parameter file:", parFile, "\n")
    cat("Output directory:", outputDir, "\n")
  }
  
  # Read and process input file sections
  tryCatch({
    # Check parameter file exists
    if(!file.exists(parFile)) {
      stop("Parameter file not found: ", parFile)
    }
    
    # Read parameter file lines
    lines <- readLines(parFile)
    
    # Process each line based on type
    for(line in lines) {
      type <- substr(trimws(line), 1, 3)
      
      switch(type,
             "RUN" = { state$sim$runname <- parse_run_name(line) },
             "SIM" = { state$sim <- parse_sim_params(line, state$sim) },
             "GRO" = { state$sim <- parse_growth_params()(line, state$sim) },
             "LUN" = { state$sim <- parse_lung_params()(line, state$sim) },
             "PHY" = { state$PC <- parse_phys_params()(line, state$PC) },
             "MED" = { state$sim <- parse_media_params()(line, state$sim) }
      )
    }
    
    # Initialize arrays after parameters are read
    state <- initialize_arrays(state)
    
    if(state$debug) {
      cat("Parameter file processed successfully\n")
      cat("Running simulation for ages", 
          state$sim$age_min, "to", state$sim$age_max, "days\n")
    }
    
    # Run main simulation
    state <- run_simulation(state)
    
    # Calculate mass balance
    state <- calculate_mass_balance(state)
    
    # Write outputs
    write_outputs(state, outputDir)
    
    if(state$debug) {
      cat("Simulation completed successfully\n")
    }
    
    return(state)
    
  }, error = function(e) {
    # Error handling
    cat("ERROR:", e$message, "\n")
    if(state$debug) {
      cat("Stack trace:\n")
      print(e)
    }
    stop(e$message)
  })
}

#' Initialize model arrays after parameters are read
#' @param state Model state
initialize_arrays <- function(state) {
  const <- AALM_constants
  
  # Calculate number of timesteps
  state$sim$NTS <- (state$sim$age_max - state$sim$age_min) * 
    state$sim$steps_per_day
  
  # Initialize arrays
  state$Y <- array(0, dim=c(const$Num_Comp))
  state$flows <- array(0, dim=c(const$Num_Flow))
  state$TS <- array(0, dim=c(const$Num_TS, state$sim$NTS))
  state$PV <- array(0, dim=c(const$Num_PV, state$sim$NTS))
  
  # Initialize tracking arrays  
  state$Intakes <- array(0, dim=8)
  state$Uptakes <- array(0, dim=11)
  state$Excrete <- array(0, dim=5)
  
  # Initialize mass balance
  state$MB <- list(
    Yin = array(0, dim=const$Num_MB),
    Ycomp = array(0, dim=const$Num_MB),
    Yflow = array(0, dim=const$Num_MB),
    Ybody = array(0, dim=const$Num_MB),
    Yout = array(0, dim=const$Num_MB)
  )
  
  return(state)
}