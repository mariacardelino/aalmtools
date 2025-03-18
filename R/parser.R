#' Parameter file parsing functions for AALM model
#'    match read_sim_data(), read_growth_data(), etc in original Fortran model

#' Read and parse parameter file
#' @param filename Path to parameter file
#' @param debug Print debug info
#' @return List containing parsed parameters
read_parameters <- function(filename, debug = FALSE) {
  const <- AALM_constants
  
  # Initialize parameter lists
  sim <- list()
  PC <- rep(0, const$Num_PC)
  PV <- array(0, dim = c(const$Num_PV, sim$NTS))
  
  # Read file lines
  lines <- readLines(filename)
  
  # Process each line
  for (line in lines) {
    if (startsWith(line, "RUN")) {
      sim$runname <- parse_run_name(line)
    }
    else if (startsWith(line, "SIM")) {
      parse_sim_params(line, sim)  
    }
    else if (startsWith(line, "GRO")) {
      parse_growth_params(line, sim)
    }
    else if (startsWith(line, "LUN")) {
      parse_lung_params(line, sim)
    }
    else if (startsWith(line, "PHY")) {
      parse_phys_params(line, PC)
    }
    else if (startsWith(line, "MED")) {
      parse_media_params(line, sim)
    }
  }
  
  return(list(sim = sim, PC = PC, PV = PV))
}

#' Parse run name from input line
#' @param line Character string containing the run name#' 
#' @return Parsed run name
#' @export
parse_run_name <- function(line) {
  parts <- strsplit(line, "\\s+")[[1]]
  return(parts[2])
}

#' Parse simulation parameters from input file
#' 
#' @description
#' Parses and validates simulation parameters from an input configuration line.
#' 
#' @param line Character string containing the parameter configuration
#' @param sim List object containing simulation settings to be updated
#' @return Updated simulation settings list
#' @examples
#' \dontrun{
#' sim <- list()
#' line <- "OUTWRITE 1 0"
#' sim <- parse_sim_params(line, sim)
#' }
#' @export
parse_sim_params <- function(line, sim) {
  parts <- strsplit(line, "\\s+")[[1]]
  param <- parts[2]
  
  if (param == "OUTWRITE") {
    sim$outwrite <- as.numeric(parts[3:4])
  }
  else if (param == "DEBUG") {
    sim$debug <- as.logical(as.numeric(parts[3]))
  }
  else if (param == "AGE_RANGE") {
    sim$age_min <- as.numeric(parts[4])
    sim$age_max <- as.numeric(parts[5])
  }
  else if (param == "STEPS_PER_DAY") {
    sim$steps_per_day <- as.numeric(parts[3])
  }
}

#' Parse growth parameters
#' @param line Character string containing growth parameters
#' @param sim Simulation parameters list
#' @export
parse_growth_params <- function(line, sim) {
    parts <- strsplit(line, "\\s+")[[1]]
    param <- parts[2]
    
    if (param == "BODY_WEIGHT") {
        sim$body_weight <- as.numeric(parts[3])
    }
    else if (param == "GENDER") {
        sim$gender <- parts[3]
    }
}

#' Parse lung parameters
#' @param line Character string containing lung parameters
#' @param sim Simulation parameters list
#' @export
parse_lung_params <- function(line, sim) {
    parts <- strsplit(line, "\\s+")[[1]]
    param <- parts[2]
    
    if (param == "DEPOSITION") {
        sim$lung_deposition <- list(
            ET = as.numeric(parts[3]),
            TB = as.numeric(parts[4]),
            AL = as.numeric(parts[5])
        )
    }
    else if (param == "CLEARANCE") {
        sim$lung_clearance <- list(
            ET = as.numeric(parts[3]),
            TB = as.numeric(parts[4]),
            AL = as.numeric(parts[5]),
            INT = as.numeric(parts[6])
        )
    }
}

#' Parse physiological parameters
#' @param line Character string containing physiological parameters
#' @param PC Physiological constants vector
#' @export
parse_phys_params <- function(line, PC) {
    parts <- strsplit(line, "\\s+")[[1]]
    param <- parts[2]
    const <- AALM_constants
    
    if (param == "GI_ABSORPTION") {
        PC[const$PC$GI_absorption] <- as.numeric(parts[3])
    }
    else if (param == "HEMATOCRIT") {
        PC[const$PC$Hematocrit] <- as.numeric(parts[3])/100  # Convert to fraction
    }
    else if (param == "RBC_BINDING") {
        PC[const$PC$RBC_binding] <- as.numeric(parts[3])
    }
}

#' Parse media parameters
#' @param line Character string containing media parameters
#' @param sim Simulation parameters list
#' @export
parse_media_params <- function(line, sim) {
    parts <- strsplit(line, "\\s+")[[1]]
    param <- parts[2]
    
    if (param == "SOIL") {
        sim$media$soil <- list(
            conc = as.numeric(parts[3]),
            intake = as.numeric(parts[4]),
            rba = as.numeric(parts[5])
        )
    }
    else if (param == "DUST") {
        sim$media$dust <- list(
            conc = as.numeric(parts[3]),
            intake = as.numeric(parts[4]),
            rba = as.numeric(parts[5])
        )
    }
    else if (param == "WATER") {
        sim$media$water <- list(
            conc = as.numeric(parts[3]),
            intake = as.numeric(parts[4])
        )
    }
    else if (param == "AIR") {
        sim$media$air <- list(
            conc = as.numeric(parts[3]),
            inhalation = as.numeric(parts[4])
        )
    }
    else if (param == "DIET") {
        sim$media$diet <- list(
            intake = as.numeric(parts[3])
        )
    }
}

#' Validate parsed parameters
#' @param params List of parsed parameters
#' @return TRUE if valid, throws error if invalid
#' @export
validate_parameters <- function(params) {
    # Check required parameters
    required <- c("runname", "age_min", "age_max", "steps_per_day", "body_weight")
    missing <- required[!required %in% names(params$sim)]
    if (length(missing) > 0) {
        stop("Missing required parameters: ", paste(missing, collapse=", "))
    }
    
    # Validate ranges
    if (params$sim$age_min < 0) stop("age_min must be non-negative")
    if (params$sim$age_max <= params$sim$age_min) stop("age_max must be greater than age_min")
    if (params$sim$steps_per_day < 1) stop("steps_per_day must be at least 1")
    if (params$sim$body_weight <= 0) stop("body_weight must be positive")
    
    # Validate media parameters if present
    if (!is.null(params$sim$media)) {
        if (!is.null(params$sim$media$soil$rba) && 
            (params$sim$media$soil$rba < 0 || params$sim$media$soil$rba > 1)) {
            stop("Soil RBA must be between 0 and 1")
        }
        if (!is.null(params$sim$media$dust$rba) && 
            (params$sim$media$dust$rba < 0 || params$sim$media$dust$rba > 1)) {
            stop("Dust RBA must be between 0 and 1")
        }
    }
    
    return(TRUE)
}