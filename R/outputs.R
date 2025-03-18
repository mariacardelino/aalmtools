#' Output generation functions for AALM model
#' Direct port of Fortran output code
#' @param state Final model state
#' @param sim Simulation parameters  
#' @param runname Name of model run
#' @param outputDir Output directory
write_outputs <- function(state, sim, runname, outputDir) {
  const <- AALM_constants
  
  # Create run directory if it doesn't exist
  dir.create(file.path(outputDir, runname), recursive = TRUE, showWarnings = FALSE)
  
  # Write RunInfo file
  runinfo_file <- file.path(outputDir, runname, paste0("RunInfo_", runname, ".txt"))
  write("AALM R code RunInfo file", runinfo_file)
  write(paste("Run name =", runname), runinfo_file, append = TRUE)
  call_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write(paste("Run at time =", call_time), runinfo_file, append = TRUE)
  
  # Write Log file
  log_file <- file.path(outputDir, runname, paste0("Log_", runname, ".csv"))
  write_log_file(state, sim, log_file)
  
  # Write source tracking file 
  src_file <- file.path(outputDir, runname, paste0("Src_", runname, ".csv"))
  write_source_file(state, sim, src_file)
  
  # Write Out file
  out_file <- file.path(outputDir, runname, paste0("Out_", runname, ".csv"))
  write_concentrations_file(state, sim, out_file)
  
  # Write Day file
  day_file <- file.path(outputDir, runname, paste0("Day_", runname, ".csv"))
  write_daily_file(state, sim, day_file)
  
  # Write Rates file
  rates_file <- file.path(outputDir, runname, paste0("Rates_", runname, ".csv"))
  write_rates_file(state, sim, rates_file)
  
  # Close RunInfo file with completion message
  write("Run successfully completed", runinfo_file, append = TRUE)
}

#' Write source tracking file
#' @param state Model state
#' @param sim Simulation parameters
#' @param filename Output file path
write_source_file <- function(state, sim, filename) {
  header <- paste(
    "Timestep,Days,Years,Air1,Air2,Air3,Dust1,Dust2,Dust3,Soil1,Soil2,Soil3,",
    "Water1,Water2,Water3,Food1,Food2,Food3,Other1,Other2,Other3,Tintake,Tbody,Telim,Diff"
  )
  write(header, filename)
  
  # Write source data for each output timestep
  for(j in seq(sim$outwrite, sim$NTS, by=sim$outwrite)) {
    data <- sprintf(
      "%d,%.6f,%.6f,%s,%s,%s,%s,%.6f,%.6f,%.6f", 
      j, 
      state$age_ts[j],
      state$age_ts[j]/365,
      paste(sprintf("%.6f", state$source[1:3,j] * state$adjust[1:3]), collapse=","),
      paste(sprintf("%.6f", state$source[4:18,j] * state$adjust[4:18]), collapse=","),
      state$MB$Yin[j],
      state$MB$Ybody[j], 
      state$MB$Yout[j],
      state$MB$Ycomp[j] + state$MB$Yflow[j] - state$MB$Yin[j]
    )
    write(data, filename, append=TRUE)
  }
}

#' Write concentration outputs file
#' @param state Model state
#' @param sim Simulation parameters
#' @param filename Output file path 
write_concentrations_file <- function(state, sim, filename) {
  header <- paste(
    "Timestep,Days,Years,Cblood,Cplas,Ckidney,Cliver,Ccort,Ctrab,Cbone,",
    "Ablood,Aplas,ARBC,Akidney,Aliver,Acort,Atrab,Abone,Asoft,Abrain,",
    "ART,Astom,AGI,Aprot,AEVF,Ablad,Aflow,Tbody,Aurine,Afecal,Asweat,Ahair"
  )
  write(header, filename)
  
  for(j in seq(0, sim$NTS, by=sim$outwrite)) {
    data <- sprintf(
      "%d,%.6f,%.6f,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s",
      j,
      state$age_ts[j],
      state$age_ts[j]/365,
      state$Cout[state$Cblood,j],
      state$Cout[state$Cplas,j],
      state$Cout[state$Ckidney,j],
      state$Cout[state$Cliver,j],
      state$Cout[state$Ccort,j],
      state$Cout[state$Ctrab,j],
      state$Cout[state$Cbone,j],
      state$Y[state$Plas,j] + state$Y[state$RBC,j],
      state$Y[state$Plas,j],
      state$Y[state$RBC,j],
      state$Y[state$Kdn1,j] + state$Y[state$Kdn2,j],
      state$Y[state$Lvr1,j] + state$Y[state$Lvr2,j],
      state$Y[state$Csur,j] + state$Y[state$Cdif,j] + state$Y[state$Cvol,j],
      state$Y[state$Tsur,j] + state$Y[state$Tdif,j] + state$Y[state$Tvol,j],
      sum(state$Y[c(state$Csur:state$Tvol),j]),
      sum(state$Y[c(state$Sof0:state$Sof2),j]),
      state$Y[state$Bran,j],
      sum(state$Y[c(state$LET:state$Lint),j]),
      state$Y[state$Stom,j],
      sum(state$Y[c(state$SI:state$LLI),j]),
      state$Y[state$Prot,j],
      state$Y[state$EVF,j],
      state$Y[state$Blad,j],
      state$MB$Yflow[j],
      state$MB$Ybody[j],
      state$Y[state$Urin,j],
      state$Y[state$Fece,j],
      state$Y[state$Swet,j],
      state$Y[state$Hair,j]
    )
    write(data, filename, append=TRUE)
  }
}

#' Write daily inputs/outputs file
#' @param state Model state
#' @param sim Simulation parameters
#' @param filename Output file path
write_daily_file <- function(state, sim, filename) {
  header <- paste(
    "Day,InAirToT,InAirDep,InIngest,InDust,InSoil,InWater,InFood,InOther,",
    "UpTotal,UpAir,UpLung,UpGIAir,UpGITotal,UpIngest,UpGIDust,UpGISoil,",
    "UpGIWater,UpGIFood,UpGIOther,ExAir,ExUrine,ExFeces,ExSweat,ExHair",
    sep=""
  )
  write(header, filename)
  
  for(t in seq(sim$Nperday, sim$NTS, by=sim$Nperday)) {
    data <- sprintf(
      "%d,%s,%s,%s,%s,%s",
      t/sim$Nperday,
      paste(sprintf("%.6f", state$Intakes[1:8]), collapse=","),
      paste(sprintf("%.6f", state$Uptakes[1:11]), collapse=","),
      paste(sprintf("%.6f", state$Excrete[1:5]), collapse=",")
    )
    write(data, filename, append=TRUE)
  }
}
#' Write comprehensive log file
#' @param state Model state
#' @param sim Simulation parameters
#' @param filename Output file path
write_log_file <- function(state, sim, filename) {
  # Write header info
  write(paste("AALM Log File for run:", sim$runname), filename)
  write(paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), filename, append=TRUE)
  write("\nModel Parameters:", filename, append=TRUE)
  
  # Write simulation parameters
  write(sprintf("Age range: %d to %d days", sim$age_min, sim$age_max), filename, append=TRUE)
  write(sprintf("Steps per day: %d", sim$Nperday), filename, append=TRUE)
  
  # Write physiological parameters
  write("\nPhysiological Parameters:", filename, append=TRUE)
  for(i in 1:const$Num_PC) {
    write(sprintf("PC[%d] = %.6f", i, sim$PC[i]), filename, append=TRUE)
  }
  
  # Write time series data
  write("\nTime Series Data:", filename, append=TRUE)
  write("Day,Age(yr),Body_wt,Blood_vol,RBC_vol,Plas_vol,Hematocrit", filename, append=TRUE)
  
  for(t in seq(0, sim$NTS, by=sim$outwrite)) {
    data <- sprintf("%.1f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f",
                    t/sim$Nperday,
                    t/(365*sim$Nperday),
                    state$TS[const$Body_wt,t],
                    state$TS[const$Blood_vol,t],
                    state$TS[const$RBC_vol,t],
                    state$TS[const$Plas_vol,t],
                    state$TS[const$Hematocrit,t]
    )
    write(data, filename, append=TRUE)
  }
}

#' Write rates file for model simulation
#' 
#' Writes transfer rates and flows between compartments to a CSV file.
#' 
#' @param state List containing the model state and rates
#' @param sim List containing simulation parameters
#' @param filename Character string specifying the output file path
#' @return Logical indicating whether file was written successfully
#' @examples
#' \dontrun{
#' state <- list(rates = matrix())
#' sim <- list(parameters = list())
#' write_rates_file(state, sim, "rates.csv")
#' }
#' @export
write_rates_file <- function(state, sim, filename) {
  # Write header
  write("AALM Transfer Rates", filename)
  write(paste("Run:", sim$runname), filename, append=TRUE)
  
  # Calculate rates at specific ages (as in Fortran)
  set_ages <- c(100, 365, 1825, 3650, 5475, 9125) * sim$Nperday
  
  for(t in set_ages) {
    write(sprintf("\nTimestep=%d  Year=%.3f", t, t/(365*sim$Nperday)), 
          filename, append=TRUE)
    
    # Transfer rates from plasma
    write(sprintf("  Plasma-D to EVF        = %.6f", 
                  state$TS[const$R_Plas,t] * state$TS[const$T_EVF,t] / 
                    state$TS[const$T_Sum,t]), filename, append=TRUE)
    write(sprintf("  Plasma-D to RBC        = %.6f",
                  state$TS[const$R_Plas,t] * state$TS[const$T_RBC,t] / 
                    state$TS[const$T_Sum,t]), filename, append=TRUE)
    write(sprintf("  Plasma-D to ST0        = %.6f",
                  state$TS[const$R_Plas,t] * state$TS[const$T_Sof0,t] / 
                    state$TS[const$T_Sum,t]), filename, append=TRUE)
    
    # Bone transfer rates
        write(sprintf("  Cort surface-->plasma  = %.6f", state$TS[const$R_CsurPlas,t]),
              filename, append=TRUE)
        write(sprintf("  Cort surface-->diffuse = %.6f", state$TS[const$R_CsurCdif,t]),
              filename, append=TRUE)
        write(sprintf("  Cort diffuse-->volume  = %.6f", state$TS[const$R_CdifCvol,t]),
              filename, append=TRUE)
        write(sprintf("  Trab surface-->plasma  = %.6f", state$TS[const$R_TsurPlas,t]),
              filename, append=TRUE)
        write(sprintf("  Trab surface-->diffuse = %.6f", state$TS[const$R_TsurTdif,t]),
              filename, append=TRUE)
        write(sprintf("  Trab diffuse-->volume  = %.6f", state$TS[const$R_TdifTvol,t]),
              filename, append=TRUE)
        
        # Soft tissue transfer rates
        write(sprintf("  ST0-->plasma          = %.6f", state$TS[const$R_Sof0Plas,t]),
              filename, append=TRUE)
        write(sprintf("  ST1-->plasma          = %.6f", state$TS[const$R_Sof1Plas,t]),
              filename, append=TRUE)
        write(sprintf("  ST2-->plasma          = %.6f", state$TS[const$R_Sof2Plas,t]),
              filename, append=TRUE)
        
        # RBC and protein transfer rates
        write(sprintf("  RBC-->plasma          = %.6f", state$TS[const$R_RBCPlas,t]),
              filename, append=TRUE)
        write(sprintf("  Protein-->plasma      = %.6f", state$TS[const$R_ProtPlas,t]),
              filename, append=TRUE)
        
        # Liver and kidney transfer rates
        write(sprintf("  Liver1-->plasma       = %.6f", state$TS[const$R_Lvr1Plas,t]),
              filename, append=TRUE)
        write(sprintf("  Liver2-->plasma       = %.6f", state$TS[const$R_Lvr2Plas,t]),
              filename, append=TRUE)
        write(sprintf("  Kidney1-->plasma      = %.6f", state$TS[const$R_Kdn1Plas,t]),
              filename, append=TRUE)
        write(sprintf("  Kidney2-->plasma      = %.6f", state$TS[const$R_Kdn2Plas,t]),
              filename, append=TRUE)
        
        # Lung transfer rates
        write(sprintf("  LET-->plasma          = %.6f", state$TS[const$R_LETPlas,t]),
              filename, append=TRUE)
        write(sprintf("  LTB-->plasma          = %.6f", state$TS[const$R_LTBPlas,t]),
              filename, append=TRUE)
        write(sprintf("  Lalv-->plasma         = %.6f", state$TS[const$R_LalvPlas,t]),
              filename, append=TRUE)
        write(sprintf("  Lint-->plasma         = %.6f", state$TS[const$R_LintPlas,t]),
              filename, append=TRUE)
        
        # GI tract transfer rates
        write(sprintf("  SI-->plasma           = %.6f", state$TS[const$R_SIPlas,t]),
              filename, append=TRUE)
        write(sprintf("  SI-->ULI              = %.6f", state$TS[const$R_SIULI,t]),
              filename, append=TRUE)
        write(sprintf("  ULI-->LLI             = %.6f", state$TS[const$R_ULILLI,t]),
              filename, append=TRUE)
        
        # Excretion rates
        write(sprintf("  Kidney-->urine        = %.6f", state$TS[const$R_KdnUrin,t]),
              filename, append=TRUE)
        write(sprintf("  LLI-->feces           = %.6f", state$TS[const$R_LLIFece,t]),
              filename, append=TRUE)
        write(sprintf("  Plasma-->sweat        = %.6f", state$TS[const$R_PlasSwet,t]),
              filename, append=TRUE)
        write(sprintf("  ST1-->hair            = %.6f", state$TS[const$R_Sof1Hair,t]),
              filename, append=TRUE)
    }
    
    return(TRUE)
}

#' Write model outputs with error handling
#' 
#' Safely writes AALM model outputs to files with error checking and validation.
#' 
#' @param state List containing the model state including compartment amounts and flows
#' @param sim List containing simulation parameters and settings
#' @param runname Character string specifying the name of the simulation run
#' @param outputDir Character string specifying the directory path for output files
#' @return Logical indicating whether writing was successful
#' @examples
#' \dontrun{
#' state <- list(compartments = list(), flows = list())
#' sim <- list(parameters = list())
#' write_outputs_safe(state, sim, "test_run", "outputs")
#' }
#' @export
write_outputs_safe <- function(state, sim, runname, outputDir) {
  tryCatch({
    write_outputs(state, sim, runname, outputDir)
  }, error = function(e) {
    # Write error to log
    err_file <- file.path(outputDir, runname, "error.log")
    write(sprintf("Error writing outputs: %s", e$message), err_file)
    stop(sprintf("Failed to write outputs: %s", e$message))
  })
}