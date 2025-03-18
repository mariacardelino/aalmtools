#' Compartment update functions for AALM model with 
#'    Actvty() fuction

# ALL COMPARTMENTS
# 1. Stomach (Stom)
# 2. Small intestine (SI)
# 3. upper large intestine (ULI)
# 4. lower large intestine (LLI)
# 5. Feces (Fece)
# 6. Plasma (Plas)
# 7. Plasma protein-bound (Prot)
# 8. Red blood cells (RBC)
# 9. Extra-vascular fluid (EVF)
# 10. Fast turnover soft tissue (Sof0)
# 11. Intermediate turnover soft tissue (Sof1)
# 12. Slow turnover soft tissue (Sof2)
# 13. Brain (Bran)
# 14. Cortical surface (Csur)
# 15. Exchangeable cortical volume (Cdif)
# 16. Nonexchangeable cortical volume (Cvol)
# 17. Trabecular surface (Tsur)
# 18. Exchangeable trabecular volume (Tdif)
# 19. Nonexchangeable trabecular volume (Tvol)
# 20. Liver 1 (Lvr1)
# 21. Liver 2 (Lvr2)
# 22. Kidneys1 (Kdn1)
# 23. Kidneys2 (Kdn2)
# 24. Bladder (Blad)
# 25. Urine (Urin)
# 26. Sweat (Swet)
# 27. Hair (Hair)
# 28. Lungs Extrathoracic (LET or Ylung(1))
# 29. Lungs Tracheo-broncular (LTB or Ylung(2))
# 30. Lungs Alveolar (Lalv or Ylung(3))
# 31. Lungs Intertitial (Lint)


#' Update all compartments for current timestep
#' @param state Current model state
#' @param t Current timestep
update_compartments <- function(state, t) {
  # Update compartments in same order as Fortran model
  state <- update_stomach(state, t)
  state <- update_small_intestine(state, t)
  state <- update_upper_large_intestine(state, t)
  state <- update_lower_large_intestine(state, t)
  state <- update_plasma(state, t)
  state <- update_RBC(state, t)
  state <- update_protein(state, t)
  state <- update_cortical_surface(state, t)
  state <- update_cortical_diffuse(state, t)
  state <- update_cortical_volume(state, t)
  state <- update_trabecular_surface(state, t)
  state <- update_trabecular_diffuse(state, t)
  state <- update_trabecular_volume(state, t)
  state <- update_liver_1(state, t)
  state <- update_liver_2(state, t)
  state <- update_kidney_1(state, t)
  state <- update_kidney_2(state, t)
  state <- update_lung_ET(state, t)
  state <- update_lung_TB(state, t)
  state <- update_lung_alveolar(state, t)
  state <- update_lung_interstitial(state, t)
  state <- update_soft_tissue_0(state, t)
  state <- update_soft_tissue_1(state, t)
  state <- update_soft_tissue_2(state, t)
  state <- update_urine(state, t)
  state <- update_feces(state, t)
  state <- update_sweat(state, t)
  state <- update_hair(state, t)
  
  return(state)
}

#' Compartment updates ################
#'  1. Calculate outflows based on current compartment amount and transfer coefficients
#'  2. Update amount using Actvty()
#'  3. Store results in state vectors

#' @param state Current model state
#' @param t Current timestep

#' Update stomach compartment
update_stomach <- function(state, t) {
  const <- AALM_constants
  
  # Get current amount in stomach
  Y_stom <- state$Y[const$Compartments["Stom"]]
  
  # Calculate source tracking for up to 18 sources
  # First 3 are from lung mucociliary clearance
  state$SrcT[const$Ingest18, 1:3] <- state$YlungTM1[const$F3LETStom, 1:3]
  
  # Sources 4-18 are ingestion sources
  state$SrcT[const$Ingest18, 4:state$Nsource] <- 
    state$source[4:state$Nsource, t] * state$adjust[4:state$Nsource]
  
  # Calculate flows for each source
  for(j in 1:state$Nsource) {
    state$SrcT[const$Y18Stom, j] <- Actvty(
      state$SrcTM1[const$Y18Stom, j],
      state$SrcT[const$Ingest18, j]/state$delt,
      state$PC[const$StomRate],
      state$delt
    )
    
    state$SrcT[const$F18StomSI, j] <- 
      state$SrcTM1[const$Y18Stom, j] + 
      state$SrcT[const$Ingest18, j] - 
      state$SrcT[const$Y18Stom, j]
  }
  
  # Calculate total stomach amount and flow to SI
  state$Y[const$Stom, t] <- sum(state$SrcT[const$Y18Stom, 1:state$Nsource])
  state$flows[const$Stom_SI, t] <- sum(state$SrcT[const$F18StomSI, 1:state$Nsource])
  
  return(state)
}

#' Update small intestine compartment
update_small_intestine <- function(state, t) {
  const <- AALM_constants
  
  # Source tracking from liver and plasma (index 0)
  state$SrcTM1[const$F18StomSI, 0] <- 
    state$flows[const$Lvr1_SI, t-1] + state$flows[const$Plas_SI, t-1]
  
  # Calculate SI amounts and flows for each source (0-18)
  for(j in 0:state$Nsource) {
    state$SrcT[const$Y18SI, j] <- Actvty(
      state$SrcTM1[const$Y18SI, j],
      state$SrcTM1[const$F18StomSI, j]/state$delt,
      state$PC[const$SIRate],
      state$delt
    )
    
    # Calculate outflow and split between plasma and ULI
    outmass <- state$SrcTM1[const$Y18SI, j] + 
      state$SrcTM1[const$F18StomSI, j] - 
      state$SrcT[const$Y18SI, j]
    
    GIabs <- state$TS[const$F1_abs, t] * state$AllSource[[j]]$RBA
    
    state$SrcT[const$F18SIPlas, j] <- outmass * GIabs
    state$SrcT[const$F18SIULI, j] <- outmass * (1 - GIabs)
  }
  
  # Calculate total SI amount and flows
  state$Y[const$SI, t] <- sum(state$SrcT[const$Y18SI, 0:state$Nsource])
  state$flows[const$SI_Plas, t] <- sum(state$SrcT[const$F18SIPlas, 0:state$Nsource])
  state$flows[const$SI_ULI, t] <- sum(state$SrcT[const$F18SIULI, 0:state$Nsource])
  
  return(state)
}

#' Update upper large intestine compartment
update_upper_large_intestine <- function(state, t) {
  const <- AALM_constants
  
  # Calculate ULI amounts and flows for each source
  for(j in 0:state$Nsource) {
    state$SrcT[const$Y18ULI, j] <- Actvty(
      state$SrcTM1[const$Y18ULI, j],
      state$SrcT[const$F18SIULI, j]/state$delt,
      state$PC[const$ULIRate],
      state$delt
    )
    
    state$SrcT[const$F18ULILLI, j] <- 
      state$SrcTM1[const$Y18ULI, j] + 
      state$SrcT[const$F18SIULI, j] - 
      state$SrcT[const$Y18ULI, j]
  }
  
  # Calculate total ULI amount and flow to LLI
  state$Y[const$ULI, t] <- sum(state$SrcT[const$Y18ULI, 0:state$Nsource])
  state$flows[const$ULI_LLI, t] <- sum(state$SrcT[const$F18ULILLI, 0:state$Nsource])
  
  return(state)
}

#' Update lower large intestine compartment
update_lower_large_intestine <- function(state, t) {
  const <- AALM_constants
  
  # Calculate LLI amounts and flows for each source  
  for(j in 0:state$Nsource) {
    state$SrcT[const$Y18LLI, j] <- Actvty(
      state$SrcTM1[const$Y18LLI, j],
      state$SrcT[const$F18ULILLI, j]/state$delt,
      state$PC[const$LLIRate],
      state$delt
    )
    
    state$SrcT[const$F18LLIFece, j] <- 
      state$SrcTM1[const$Y18LLI, j] + 
      state$SrcT[const$F18ULILLI, j] - 
      state$SrcT[const$Y18LLI, j]
  }
  
  # Calculate total LLI amount and flow to feces
  state$Y[const$LLI, t] <- sum(state$SrcT[const$Y18LLI, 0:state$Nsource])
  state$flows[const$LLI_Fece, t] <- sum(state$SrcT[const$F18LLIFece, 0:state$Nsource])
  
  return(state)
}

#' Update plasma compartment
update_plasma <- function(state, t) {
  const <- AALM_constants
  
  # Calculate RBC saturation effects
  cf <- 1.0
  if(state$sim$irbc == 1 && 
     state$TS[const$RBC_conc, t-1] > state$PC[const$RBCnl]) {
    
    suppress <- (1 - ((state$TS[const$RBC_conc, t-1] - state$PC[const$RBCnl]) /
                        (state$PC[const$SatRat] - state$PC[const$RBCnl])))^state$PC[const$Power]
    if(suppress < 0) suppress <- 0
    
    cf <- (state$TS[const$T_sum, t] - suppress * state$TS[const$T_RBC, t]) /
      (state$TS[const$T_sum, t] - state$TS[const$T_RBC, t])
    
    state$TS[const$T_RBC, t] <- state$TS[const$T_RBC, t] * suppress
  }
  
  # Calculate plasma flows
  Y_plas <- state$Y[const$Plas, t-1]
  outplas <- Y_plas * state$PC[const$PlasRate]
  
  state$flows[const$Plas_RBC, t] <- outplas * state$TS[const$T_RBC, t]
  state$flows[const$Plas_Prot, t] <- outplas * cf * state$TS[const$T_Prot, t]
  state$flows[const$Plas_EVF, t] <- outplas * cf * state$TS[const$T_EVF, t]
  state$flows[const$Plas_Csur, t] <- outplas * cf * state$TS[const$T_Bone, t]
  state$flows[const$Plas_Tsur, t] <- outplas * cf * state$TS[const$T_Bone, t]
  state$flows[const$Plas_Sof0, t] <- outplas * cf * state$TS[const$T_Sof0, t]
  state$flows[const$Plas_Swet, t] <- outplas * cf * state$TS[const$T_Swet, t]
  
  # Calculate new plasma amount
  state$Y[const$Plas, t] <- Actvty(
    Y_plas,
    sum(state$flows[c(const$LET_Plas, const$LTB_Plas, const$Lalv_Plas,
                      const$Lint_Plas, const$SI_Plas, const$Csur_Plas,
                      const$Tsur_Plas, const$RBC_Plas, const$Prot_Plas,
                      const$EVF_Plas), t-1])/state$delt,
    outplas,
    state$delt
  )
  
  return(state)
}

#' Update RBC compartment
update_RBC <- function(state, t) {
  const <- AALM_constants
  
  Y_rbc <- state$Y[const$RBC, t-1]
  
  # Calculate RBC flows
  state$flows[const$RBC_Plas, t] <- Y_rbc * state$PC[const$RBCRate]
  
  # Update RBC amount
  state$Y[const$RBC, t] <- Actvty(
    Y_rbc,
    state$flows[const$Plas_RBC, t]/state$delt,
    state$flows[const$RBC_Plas, t],
    state$delt
  )
  
  return(state)
}

#' Update protein compartment
update_protein <- function(state, t) {
  const <- AALM_constants
  
  Y_prot <- state$Y[const$Prot, t-1]
  
  # Calculate protein flows
  state$flows[const$Prot_Plas, t] <- Y_prot * state$PC[const$ProtRate]
  
  # Update protein amount
  state$Y[const$Prot, t] <- Actvty(
    Y_prot,
    state$flows[const$Plas_Prot, t]/state$delt,
    state$flows[const$Prot_Plas, t],
    state$delt
  )
  
  return(state)
}

#' Update cortical bone surface
update_cortical_surface <- function(state, t) {
  const <- AALM_constants
  
  Y_csur <- state$Y[const$Csur, t-1]
  
  # Calculate cortical surface flows
  state$flows[const$Csur_Plas, t] <- Y_csur * state$PC[const$CsurPlas]
  state$flows[const$Csur_Cdif, t] <- Y_csur * state$PC[const$CsurCdif]
  
  # Update cortical surface amount
  state$Y[const$Csur, t] <- Actvty(
    Y_csur,
    (state$flows[const$Plas_Csur, t] + 
       state$flows[const$Cdif_Csur, t])/state$delt,
    state$flows[const$Csur_Plas, t] + 
      state$flows[const$Csur_Cdif, t],
    state$delt
  )
  
  return(state)
}

#' Update cortical bone diffuse
update_cortical_diffuse <- function(state, t) {
  const <- AALM_constants
  
  Y_cdif <- state$Y[const$Cdif, t-1]
  
  # Calculate cortical diffuse flows
  state$flows[const$Cdif_Csur, t] <- Y_cdif * state$PC[const$CdifCsur] 
  state$flows[const$Cdif_Cvol, t] <- Y_cdif * state$PC[const$CdifCvol]
  
  # Update cortical diffuse amount
  state$Y[const$Cdif, t] <- Actvty(
    Y_cdif,
    state$flows[const$Csur_Cdif, t]/state$delt,
    state$flows[const$Cdif_Csur, t] + 
      state$flows[const$Cdif_Cvol, t],
    state$delt
  )
  
  return(state)
}

#' Update cortical volume compartment
update_cortical_volume <- function(state, t) {
  const <- AALM_constants
  
  Y_cvol <- state$Y[const$Cvol, t-1]
  
  # No outflows from volume compartment
  state$Y[const$Cvol, t] <- Actvty(
    Y_cvol,
    state$flows[const$Cdif_Cvol, t]/state$delt,
    0,
    state$delt
  )
  
  return(state)
}

#' Update trabecular compartments
update_trabecular_surface <- function(state, t) {
  const <- AALM_constants
  
  Y_tsur <- state$Y[const$Tsur, t-1]
  
  # Calculate trabecular surface flows
  state$flows[const$Tsur_Plas, t] <- Y_tsur * state$PC[const$TsurPlas]
  state$flows[const$Tsur_Tdif, t] <- Y_tsur * state$PC[const$TsurTdif]
  
  state$Y[const$Tsur, t] <- Actvty(
    Y_tsur,
    (state$flows[const$Plas_Tsur, t] + 
       state$flows[const$Tdif_Tsur, t])/state$delt,
    state$flows[const$Tsur_Plas, t] + 
      state$flows[const$Tsur_Tdif, t],
    state$delt
  )
  
  return(state)
}

update_trabecular_diffuse <- function(state, t) {
  const <- AALM_constants
  
  Y_tdif <- state$Y[const$Tdif, t-1]
  
  # Calculate trabecular diffuse flows
  state$flows[const$Tdif_Tsur, t] <- Y_tdif * state$PC[const$TdifTsur]
  state$flows[const$Tdif_Tvol, t] <- Y_tdif * state$PC[const$TdifTvol]
  
  state$Y[const$Tdif, t] <- Actvty(
    Y_tdif,
    state$flows[const$Tsur_Tdif, t]/state$delt,
    state$flows[const$Tdif_Tsur, t] + 
      state$flows[const$Tdif_Tvol, t],
    state$delt
  )
  
  return(state)
}

update_trabecular_volume <- function(state, t) {
  const <- AALM_constants
  
  Y_tvol <- state$Y[const$Tvol, t-1]
  
  # No outflows from volume compartment
  state$Y[const$Tvol, t] <- Actvty(
    Y_tvol,
    state$flows[const$Tdif_Tvol, t]/state$delt,
    0,
    state$delt
  )
  
  return(state)
}

#' Update liver compartments
update_liver_1 <- function(state, t) {
  const <- AALM_constants
  
  Y_lvr1 <- state$Y[const$Lvr1, t-1]
  
  # Calculate liver 1 flows
  state$flows[const$Lvr1_Plas, t] <- Y_lvr1 * state$PC[const$Lvr1Plas]
  state$flows[const$Lvr1_Lvr2, t] <- Y_lvr1 * state$PC[const$Lvr1Lvr2]
  
  state$Y[const$Lvr1, t] <- Actvty(
    Y_lvr1,
    state$flows[const$Plas_Lvr1, t]/state$delt,
    state$flows[const$Lvr1_Plas, t] + 
      state$flows[const$Lvr1_Lvr2, t],
    state$delt
  )
  
  return(state)
}


#' Update liver 2 compartment
update_liver_2 <- function(state, t) {
  const <- AALM_constants
  
  Y_lvr2 <- state$Y[const$Lvr2, t-1]
  
  # Calculate liver 2 flows
  state$flows[const$Lvr2_Plas, t] <- Y_lvr2 * state$PC[const$Lvr2Plas]
  
  state$Y[const$Lvr2, t] <- Actvty(
    Y_lvr2,
    state$flows[const$Lvr1_Lvr2, t]/state$delt,
    state$flows[const$Lvr2_Plas, t],
    state$delt
  )
  
  return(state)
}

#' Update kidney compartments
update_kidney_1 <- function(state, t) {
  const <- AALM_constants
  
  Y_kdn1 <- state$Y[const$Kdn1, t-1]
  
  # Calculate kidney 1 flows
  state$flows[const$Kdn1_Plas, t] <- Y_kdn1 * state$PC[const$Kdn1Plas]
  state$flows[const$Kdn1_Kdn2, t] <- Y_kdn1 * state$PC[const$Kdn1Kdn2]
  state$flows[const$Kdn1_Urin, t] <- Y_kdn1 * state$PC[const$Kdn1Urin]
  
  state$Y[const$Kdn1, t] <- Actvty(
    Y_kdn1,
    state$flows[const$Plas_Kdn1, t]/state$delt,
    state$flows[const$Kdn1_Plas, t] + 
      state$flows[const$Kdn1_Kdn2, t] + 
      state$flows[const$Kdn1_Urin, t],
    state$delt
  )
  
  return(state)
}

update_kidney_2 <- function(state, t) {
  const <- AALM_constants
  
  Y_kdn2 <- state$Y[const$Kdn2, t-1]
  
  # Calculate kidney 2 flows
  state$flows[const$Kdn2_Plas, t] <- Y_kdn2 * state$PC[const$Kdn2Plas]
  
  state$Y[const$Kdn2, t] <- Actvty(
    Y_kdn2,
    state$flows[const$Kdn1_Kdn2, t]/state$delt,
    state$flows[const$Kdn2_Plas, t],
    state$delt
  )
  
  return(state)
}

#' Update lung compartments
update_lung_ET <- function(state, t) {
  const <- AALM_constants
  
  # Update lung ET region amounts for each source (1-3)
  for(i in 1:3) {
    state$Ylung[i, const$F3LET] <- Actvty(
      state$YlungM1[i, const$F3LET],
      state$source[i, t] * state$adjust[i] * 
        state$lung[[i]]$DepFracLET/state$delt,
      state$lung[[i]]$RLTBplas + state$lung[[i]]$RLTBLET,
      state$delt
    )
    
    # Calculate flows from ET region
    state$flows[const$LET_Plas, t] <- state$flows[const$LET_Plas, t] +
      state$YlungM1[i, const$F3LET] * state$lung[[i]]$RLTBplas
    
    state$flows[const$LET_Stom, t] <- state$flows[const$LET_Stom, t] +
      state$YlungM1[i, const$F3LET] * state$lung[[i]]$RLTBLET
  }
  
  # Update total ET amount
  state$Y[const$LET, t] <- sum(state$Ylung[1:3, const$F3LET])
  
  return(state)
}


#' Update lung TB region
update_lung_TB <- function(state, t) {
  const <- AALM_constants
  
  # Update TB region amounts for each source (1-3)
  for(i in 1:3) {
    state$Ylung[i, const$F3LTB] <- Actvty(
      state$YlungM1[i, const$F3LTB],
      (state$source[i, t] * state$adjust[i] * state$lung[[i]]$DepFracLTB + 
         state$Ylung[i, const$F3Lalv] * state$lung[[i]]$RLalvLTB)/state$delt,
      state$lung[[i]]$RLTBplas + state$lung[[i]]$RLTBLET,
      state$delt
    )
    
    # Calculate flows from TB region
    state$flows[const$LTB_Plas, t] <- state$flows[const$LTB_Plas, t] +
      state$YlungM1[i, const$F3LTB] * state$lung[[i]]$RLTBplas
    
    state$flows[const$LTB_LET, t] <- state$flows[const$LTB_LET, t] +
      state$YlungM1[i, const$F3LTB] * state$lung[[i]]$RLTBLET
  }
  
  state$Y[const$LTB, t] <- sum(state$Ylung[1:3, const$F3LTB])
  
  return(state)
}

#' Update lung alveolar region
update_lung_alveolar <- function(state, t) {
  const <- AALM_constants
  
  # Update alveolar region amounts for each source
  for(i in 1:3) {
    state$Ylung[i, const$F3Lalv] <- Actvty(
      state$YlungM1[i, const$F3Lalv],
      state$source[i, t] * state$adjust[i] * state$lung[[i]]$DepFracLalv/state$delt,
      state$lung[[i]]$RLalvPlas + state$lung[[i]]$RLalvLTB + 
        state$lung[[i]]$RLalvLint,
      state$delt
    )
    
    # Calculate flows from alveolar region
    state$flows[const$Lalv_Plas, t] <- state$flows[const$Lalv_Plas, t] +
      state$YlungM1[i, const$F3Lalv] * state$lung[[i]]$RLalvPlas
    
    state$flows[const$Lalv_LTB, t] <- state$flows[const$Lalv_LTB, t] +
      state$YlungM1[i, const$F3Lalv] * state$lung[[i]]$RLalvLTB
    
    state$flows[const$Lalv_Lint, t] <- state$flows[const$Lalv_Lint, t] +
      state$YlungM1[i, const$F3Lalv] * state$lung[[i]]$RLalvLint
  }
  
  state$Y[const$Lalv, t] <- sum(state$Ylung[1:3, const$F3Lalv])
  
  return(state)
}

#' Update lung interstitial region
update_lung_interstitial <- function(state, t) {
  const <- AALM_constants
  
  # Update interstitial amounts for each source
  for(i in 1:3) {
    state$Ylung[i, const$F3Lint] <- Actvty(
      state$YlungM1[i, const$F3Lint],
      state$Ylung[i, const$F3Lalv] * state$lung[[i]]$RLalvLint/state$delt,
      state$lung[[i]]$RLintPlas,
      state$delt
    )
    
    # Calculate flow to plasma
    state$flows[const$Lint_Plas, t] <- state$flows[const$Lint_Plas, t] +
      state$YlungM1[i, const$F3Lint] * state$lung[[i]]$RLintPlas
  }
  
  state$Y[const$Lint, t] <- sum(state$Ylung[1:3, const$F3Lint])
  
  return(state)
}

#' Update soft tissue compartments
update_soft_tissue_0 <- function(state, t) {
  const <- AALM_constants
  
  Y_sof0 <- state$Y[const$Sof0, t-1]
  
  # Calculate soft tissue 0 flows
  state$flows[const$Sof0_Plas, t] <- Y_sof0 * state$PC[const$Sof0Plas]
  state$flows[const$Sof0_Sof1, t] <- Y_sof0 * state$PC[const$Sof0Sof1]
  
  state$Y[const$Sof0, t] <- Actvty(
    Y_sof0,
    state$flows[const$Plas_Sof0, t]/state$delt,
    state$flows[const$Sof0_Plas, t] + state$flows[const$Sof0_Sof1, t],
    state$delt
  )
  
  return(state)
}

update_soft_tissue_1 <- function(state, t) {
  const <- AALM_constants
  
  Y_sof1 <- state$Y[const$Sof1, t-1]
  
  # Calculate soft tissue 1 flows
  state$flows[const$Sof1_Sof2, t] <- Y_sof1 * state$PC[const$Sof1Sof2]
  state$flows[const$Sof1_Hair, t] <- Y_sof1 * state$PC[const$Sof1Hair]
  
  state$Y[const$Sof1, t] <- Actvty(
    Y_sof1,
    state$flows[const$Sof0_Sof1, t]/state$delt,
    state$flows[const$Sof1_Sof2, t] + state$flows[const$Sof1_Hair, t],
    state$delt
  )
  
  return(state)
}

update_soft_tissue_2 <- function(state, t) {
  const <- AALM_constants
  
  Y_sof2 <- state$Y[const$Sof2, t-1]
  
  # No outflows from soft tissue 2
  state$Y[const$Sof2, t] <- Actvty(
    Y_sof2,
    state$flows[const$Sof1_Sof2, t]/state$delt,
    0,
    state$delt
  )
  
  return(state)
}

#' Update excretion compartments
update_urine <- function(state, t) {
  const <- AALM_constants
  
  Y_urin <- state$Y[const$Urin, t-1]
  
  # Calculate cumulative urinary excretion
  state$Y[const$Urin, t] <- Actvty(
    Y_urin,
    state$flows[const$Kdn1_Urin, t]/state$delt,
    0,  # Terminal excretion - no outflows
    state$delt
  )
  
  return(state)
}

update_feces <- function(state, t) {
  const <- AALM_constants
  
  Y_fece <- state$Y[const$Fece, t-1]
  
  # Calculate cumulative fecal excretion
  state$Y[const$Fece, t] <- Actvty(
    Y_fece,
    state$flows[const$LLI_Fece, t]/state$delt,
    0,  # Terminal excretion - no outflows
    state$delt
  )
  
  return(state)
}

update_sweat <- function(state, t) {
  const <- AALM_constants
  
  Y_swet <- state$Y[const$Swet, t-1]
  
  # Calculate cumulative sweat excretion
  state$Y[const$Swet, t] <- Actvty(
    Y_swet,
    state$flows[const$Plas_Swet, t]/state$delt,
    0,  # Terminal excretion - no outflows
    state$delt
  )
  
  return(state)
}

update_hair <- function(state, t) {
  const <- AALM_constants
  
  Y_hair <- state$Y[const$Hair, t-1]
  
  # Calculate cumulative hair excretion
  state$Y[const$Hair, t] <- Actvty(
    Y_hair,
    state$flows[const$Sof1_Hair, t]/state$delt,
    0,  # Terminal excretion - no outflows
    state$delt
  )
  
  return(state)
}