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

#' Parse individual parameter line types
parse_run_name <- function(line) {
  parts <- strsplit(line, "\\s+")[[1]]
  return(parts[2])
}

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

# Additional parsing functions...