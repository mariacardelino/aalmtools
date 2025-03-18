#' Core simulation functions for AALM model
#'    State intitialization, mass balance calculations, 
#'    intake/update calculations (subroutine), results storage

#' Initialize model state
#' @param sim Simulation parameters
#' @return List containing model state vectors
initialize_model <- function(sim) {
  # Create vectors for all compartments
  state <- list(
    Y = rep(0, length(AALM_constants$Compartments)),
    flows = rep(0, length(AALM_constants$Flows)),
    PC = rep(0, AALM_constants$Num_PC),
    PV = rep(0, AALM_constants$Num_PV),
    TS = rep(0, AALM_constants$Num_TS)
  )
  return(state)
}

#' Run the AALM simulation
#' 
#' @description
#' Executes the main AALM simulation with the provided parameters and initial state.
#' 
#' @param state List containing the current model state including compartment amounts
#' @param sim List containing simulation parameters, settings and time points
#' @return List containing updated model state and simulation results
#' @examples
#' \dontrun{
#' state <- initialize_state()
#' sim <- list(timesteps = 100, dt = 0.1)
#' results <- run_simulation(state, sim)
#' }
#' @export
run_simulation <- function(state, sim) {
  # Main time stepping loop
  for (t in 1:sim$NTS) {
    # Update all compartments
    state <- update_compartments(state, sim, t)
    
    # Calculate flows between compartments
    state <- calculate_flows(state, sim, t)
    
    # Update physiological variables
    state <- update_physiology(state, sim, t)
    
    # Store timestep results
    state <- store_results(state, sim, t)
  }
  
  return(state)
}

#' Calculate flows between compartments
#' @param state Current model state
#' @param sim Simulation parameters
#' @param t Current timestep
#' @return Updated flow rates
#' Calculate flows between compartments

calculate_flows <- function(state, sim, t) {
  const <- AALM_constants
  
  # Calculate weight-based parameters
  wgtsum <- sim$gr$wbirth + sim$gr$wchild + sim$gr$wadult
  
  # Body mass scaling
  state$TS[const$Body_wt, t] <- sim$gr$wbirth + 
    (t/365) * sim$gr$wchild/(sim$gr$half + t/365) +
    sim$gr$wadult/(1.0 + sim$gr$kappa * exp(-sim$gr$lambda * sim$gr$wadult * t/365))
  
  # Hematocrit calculation
  state$TS[const$Hematocrit, t] <- sim$PC[const$HCTA] + 
    (sim$PC[const$HCTB] - sim$PC[const$HCTA]) * exp(-13.9 * t/365)
  
  # Blood volume calculations (factor of 10 to obtain deciliters)
  state$TS[const$Blood_vol, t] <- sim$PC[const$VBLC] * 10 * state$TS[const$Body_wt, t]
  state$TS[const$RBC_vol, t] <- state$TS[const$Blood_vol, t] * state$TS[const$Hematocrit, t]
  state$TS[const$Plas_vol, t] <- state$TS[const$Blood_vol, t] * (1 - state$TS[const$Hematocrit, t])
  
  # Tissue weights
  state$TS[const$Bone_wt, t] <- 1000 * 0.0290 * state$TS[const$Body_wt, t]^1.21
  state$TS[const$Bone_vol, t] <- 1000 * 0.0168 * state$TS[const$Body_wt, t]^1.188
  state$TS[const$Cort_wt, t] <- state$TS[const$Bone_wt, t] * 0.80
  state$TS[const$Kid_wt, t] <- 1050 * sim$PC[const$VKC] * wgtsum * 
    (state$TS[const$Body_wt, t]/wgtsum)^0.84
  state$TS[const$Liv_wt, t] <- 1050 * sim$PC[const$VLC] * wgtsum * 
    (state$TS[const$Body_wt, t]/wgtsum)^0.85
  state$TS[const$Skel_wt, t] <- 1000 * 0.058 * state$TS[const$Body_wt, t]^1.21
  state$TS[const$Trab_wt, t] <- state$TS[const$Bone_wt, t] * 0.20
  
  # Calculate tissue fractional transfer rates
  agescl <- (1 - sim$PC[const$TEVF] - state$PV[const$TBone, t]) / 
    (1 - sim$PC[const$TEVF] - state$PV[const$TBone, sim$NTS])
  
  state$TS[const$T_Bone, t] <- state$PV[const$TBone, t]
  state$TS[const$T_EVF, t] <- sim$PC[const$TEVF]
  state$TS[const$T_Frac, t] <- state$PV[const$TFrac, t]
  state$TS[const$T_Bran, t] <- state$PV[const$ToBran, t] * agescl
  state$TS[const$T_SI, t] <- sim$PC[const$ToFece] * agescl
  state$TS[const$T_Kdn1, t] <- sim$PC[const$ToKdn1] * agescl
  state$TS[const$T_Kdn2, t] <- sim$PC[const$ToKdn2] * agescl
  state$TS[const$T_Lvr1, t] <- sim$PC[const$ToLvr1] * agescl
  state$TS[const$T_Prot, t] <- sim$PC[const$ToProt] * agescl
  state$TS[const$T_RBC, t] <- sim$PC[const$ToRBC] * agescl
  state$TS[const$T_Sof0, t] <- state$PV[const$ToSof0, t] * agescl
  state$TS[const$T_Sof1, t] <- state$PV[const$ToSof1, t] * agescl
  state$TS[const$T_Sof2, t] <- state$PV[const$ToSof2, t] * agescl
  state$TS[const$T_Swet, t] <- sim$PC[const$ToSwet] * agescl
  state$TS[const$T_Urin, t] <- sim$PC[const$ToUrin] * agescl
  
  # Sum of all tissue fractions
  state$TS[const$T_Sum, t] <- state$TS[const$T_Bone, t] + 
    state$TS[const$T_Bran, t] + 
    state$TS[const$T_EVF, t] + 
    state$TS[const$T_SI, t] +
    state$TS[const$T_Kdn1, t] + 
    state$TS[const$T_Kdn2, t] + 
    state$TS[const$T_Lvr1, t] + 
    state$TS[const$T_Prot, t] +
    state$TS[const$T_RBC, t] + 
    state$TS[const$T_Sof0, t] + 
    state$TS[const$T_Sof1, t] + 
    state$TS[const$T_Sof2, t] +
    state$TS[const$T_Swet, t] + 
    state$TS[const$T_Urin, t]
  
  # Calculate flow rates
  state$TS[const$R_Plas, t] <- sim$PC[const$RPlas] * state$TS[const$T_Sum, t]
  state$TS[const$R_EVF, t] <- state$TS[const$T_EVF, t] * state$TS[const$R_Plas, t] / 
    sim$PC[const$SizeVF]
  
  # Set rates for bone compartments
  state$TS[const$R_CdifCsur, t] <- (1 - state$PV[const$FLong, t]) * state$PV[const$RDiff, t]
  state$TS[const$R_CdifCvol, t] <- state$PV[const$FLong, t] * state$PV[const$RDiff, t]
  state$TS[const$R_Cort, t] <- state$PV[const$RCort, t]
  state$TS[const$R_CsurCdif, t] <- state$PV[const$Rcs2df, t]
  state$TS[const$R_CsurPlas, t] <- state$PV[const$Rcs2b, t]
  
  # Set rates for other compartments
  state$TS[const$R_Blad, t] <- state$PV[const$RBlad, t]
  state$TS[const$R_Bran, t] <- state$PV[const$RBran, t]
  state$TS[const$R_Kdn2, t] <- state$PV[const$RKdn2, t]
  state$TS[const$R_Lvr2, t] <- state$PV[const$RLvr2, t]
  state$TS[const$R_RBC, t] <- state$PV[const$RRBC, t]
  
  # Set rates for trabecular bone
  state$TS[const$R_TdifTsur, t] <- (1 - state$PV[const$FLong, t]) * state$PV[const$RDiff, t]
  state$TS[const$R_TdifTvol, t] <- state$PV[const$FLong, t] * state$PV[const$RDiff, t]
  state$TS[const$R_Trab, t] <- state$PV[const$RTrab, t]
  state$TS[const$R_TsurTdif, t] <- state$PV[const$Rts2df, t]
  state$TS[const$R_TsurPlas, t] <- state$PV[const$Rts2b, t]
  
  return(state)
}

#' Calculate intake and uptake values
#' @param state Current model state 
#' @param sim Simulation parameters
#' @param t Current timestep
calculate_intake_uptake <- function(state, sim, t) {
  const <- AALM_constants
  
  # Reset daily counters if first timestep of day
  if (t %% sim$Nperday == 1 || sim$Nperday == 1) {
    state$Intakes <- rep(0, 8)
    state$Uptakes <- rep(0, 11) 
    state$Excrete <- rep(0, 5)
  }
  
  # Calculate intakes
  state$Intakes[const$InAirTot] <- state$Intakes[const$InAirTot] + 
    sum(state$source[1:3, t] * state$adjust[1:3])
  
  # Calculate lung deposition
  lung_dep <- sim$lung[1:3]$DepFracLET + 
    sim$lung[1:3]$DepFracLTB + 
    sim$lung[1:3]$DepFracLalv
  
  state$Intakes[const$InAirDep] <- state$Intakes[const$InAirDep] + 
    sum(state$source[1:3, t] * state$adjust[1:3] * lung_dep)
  
  state$Intakes[const$InIngest] <- state$Intakes[const$InIngest] + 
    sum(state$source[4:18, t] * state$adjust[4:18])
  
  # Calculate source-specific intakes and uptakes
  for (j in 4:sim$Nsource) {
    media <- substr(sim$Allsource[[j]]$name, 1, 4)
    
    switch(media,
           "DUST" = {
             state$Intakes[const$InDust] <- state$Intakes[const$InDust] + 
               state$source[j, t] * state$adjust[j]
             state$Uptakes[const$UpGIDust] <- state$Uptakes[const$UpGIDust] + 
               state$SrcT[const$F18SIPlas, j]
           },
           "SOIL" = {
             state$Intakes[const$InSoil] <- state$Intakes[const$InSoil] + 
               state$source[j, t] * state$adjust[j]
             state$Uptakes[const$UpGISoil] <- state$Uptakes[const$UpGISoil] + 
               state$SrcT[const$F18SIPlas, j]  
           },
           "WATE" = {
             state$Intakes[const$InWater] <- state$Intakes[const$InWater] + 
               state$source[j, t] * state$adjust[j]
             state$Uptakes[const$UpGIWater] <- state$Uptakes[const$UpGIWater] + 
               state$SrcT[const$F18SIPlas, j]
           },
           "FOOD" = {
             state$Intakes[const$InFood] <- state$Intakes[const$InFood] + 
               state$source[j, t] * state$adjust[j]
             state$Uptakes[const$UpGIFood] <- state$Uptakes[const$UpGIFood] + 
               state$SrcT[const$F18SIPlas, j]
           },
           "OTHE" = {
             state$Intakes[const$InOther] <- state$Intakes[const$InOther] + 
               state$source[j, t] * state$adjust[j]
             state$Uptakes[const$UpGIOther] <- state$Uptakes[const$UpGIOther] + 
               state$SrcT[const$F18SIPlas, j]
           }
    )
  }
  
  # Calculate total uptakes
  state$Uptakes[const$UpLung] <- state$Uptakes[const$UpLung] + 
    state$flows[const$LET_Plas, t] + 
    state$flows[const$LTB_Plas, t] + 
    state$flows[const$Lalv_Plas, t] + 
    state$flows[const$Lint_Plas, t]
  
  state$Uptakes[const$UpGIAir] <- state$Uptakes[const$UpGIAir] + 
    sum(state$SrcT[const$F18SIPlas, 1:3])
  
  state$Uptakes[const$UpAir] <- state$Uptakes[const$UpLung] + 
    state$Uptakes[const$UpGIAir]
  
  state$Uptakes[const$UpIngest] <- state$Uptakes[const$UpIngest] + 
    sum(state$SrcT[const$F18SIPlas, 4:18])
  
  state$Uptakes[const$UpGITotal] <- state$Uptakes[const$UpGIAir] + 
    state$Uptakes[const$UpIngest]
  
  state$Uptakes[const$UpTotal] <- state$Uptakes[const$UpAir] + 
    state$Uptakes[const$UpIngest]
  
  # Calculate excretion
  state$Excrete[const$ExAir] <- state$Excrete[const$ExAir] + 
    state$Intakes[const$InAirTot] - state$Intakes[const$InAirDep]
  
  state$Excrete[const$ExUrine] <- state$Excrete[const$ExUrine] + 
    state$flows[const$Blad_Urin, t]
  
  state$Excrete[const$ExFeces] <- state$Excrete[const$ExFeces] + 
    state$flows[const$LLI_Fece, t]
  
  state$Excrete[const$ExSweat] <- state$Excrete[const$ExSweat] + 
    state$flows[const$Plas_Swet, t]
  
  state$Excrete[const$ExHair] <- state$Excrete[const$ExHair] + 
    state$flows[const$Sof1_Hair, t]
  
  return(state)
}

#' Calculate mass balance for all compartments
#' @param state Current model state
#' @param sim Simulation parameters
#' @param t Current timestep
calculate_mass_balance <- function(state, sim, t) {
  const <- AALM_constants
  
  # Initialize mass balance arrays
  mb <- list(
    Yin = rep(0, const$Num_MB),
    Yeat = rep(0, const$Num_MB),
    Yinhal = rep(0, const$Num_MB),
    Ycomp = rep(0, const$Num_MB),
    Yflow = rep(0, const$Num_MB),
    Ybody = rep(0, const$Num_MB),
    Yout = rep(0, const$Num_MB)
  )
  
  # Calculate mass in each compartment
  mb$Ycomp[1] <- sum(state$Y[const$Compartments[c("Stom", "SI", "ULI", "LLI")]])
  mb$Ycomp[2] <- sum(state$Y[const$Compartments[c("Csur", "Cdif", "Cvol", "Tsur", "Tdif", "Tvol")]])
  mb$Ycomp[3] <- sum(state$Y[const$Compartments[c("Plas", "Prot", "RBC")]])
  mb$Ycomp[4] <- sum(state$Y[const$Compartments[c("Lvr1", "Lvr2")]])
  mb$Ycomp[5] <- sum(state$Y[const$Compartments[c("Kdn1", "Kdn2")]])
  mb$Ycomp[6] <- sum(state$Y[const$Compartments[c("LET", "LTB", "Lalv", "Lint")]])
  mb$Ycomp[7] <- sum(state$Y[const$Compartments[c("Blad", "Bran", "EVF", "Sof0", "Sof1", "Sof2")]])
  
  # Calculate total body burden
  state$TS[const$T_Body, t] <- sum(mb$Ycomp)
  state$TS[const$T_Blood, t] <- mb$Ycomp[3]
  
  # Calculate flows between compartments
  for (i in 1:const$Num_Flow) {
    state$TS[const$T_Flow + i - 1, t] <- state$flows[i]
  }
  
  # Calculate excretion totals
  state$TS[const$T_Excr, t] <- sum(state$Excrete)
  state$TS[const$T_Urin, t] <- state$Excrete[const$ExUrine]
  state$TS[const$T_Fece, t] <- state$Excrete[const$ExFeces]
  state$TS[const$T_Swet, t] <- state$Excrete[const$ExSweat]
  state$TS[const$T_Hair, t] <- state$Excrete[const$ExHair]
  
  return(state)
}