#' Constants and parameters for AALM model
#' 
#' @description Defines all constants, indices, and parameter defaults

AALM_constants <- list(
  version = "Mar 17 2024",
  # Model dimensions
  N1 = 100, # Max number of input points per source
  
  #Core model parameters
  Num_PC = 50,
  Num_PV = 22, 
  Num_Comp0 = 27,
  Num_Flow = 55,
  Num_TS = 47,
  Num_MB = 7,
  
  # Source tracking indices for GI tract (up to 18 sources)
  Ingest18 = 1,
  F18LungPlas = 2,
  F18LungStom = 3,
  Y18Stom = 4,
  F18StomSI = 5,
  Y18SI = 6,
  F18SIPlas = 7,
  F18SIULI = 8,
  Y18ULI = 9,
  F18ULILLI = 10,
  Y18LLI = 11,
  F18LLIFe = 12,
  Y18Plas = 13,
  Y18Fe = 14,
  Diff18 = 15,
  
  # Lung tracking indices
  YLET = 1,
  YLTB = 2,
  YLalv = 3,
  YLint = 4,
  F3LTBLET = 5,
  F3LalvLTB = 6,
  F3LalvLint = 7,
  F3LETStom = 8,
  F3LETPlas = 9,
  F3LTBPlas = 10,
  F3LalvPlas = 11,
  F3LintPlas = 12,
  
  # Physiological constants (PC)
  Ashwt = 1,
  BldMot = 2,
  BonIn = 3,
  BranIn = 4,
  Bratio = 5,
  # ... continue with all 50 PC parameters as in Fortran
  
  # Compartments
  Compartments = c(
    "Stom" = 1, "SI" = 2, "ULI" = 3, "LLI" = 4,
    "Csur" = 5, "Cdif" = 6, "Cvol" = 7,
    "Tsur" = 8, "Tdif" = 9, "Tvol" = 10,
    "Plas" = 11, "Prot" = 12, "RBC" = 13,
    "Lvr1" = 14, "Lvr2" = 15,
    "Kdn1" = 16, "Kdn2" = 17,
    "Blad" = 18, "Bran" = 19,
    "EVF" = 20,
    "Sof0" = 21, "Sof1" = 22, "Sof2" = 23,
    "Urin" = 24, "Fece" = 25,
    "Swet" = 26, "Hair" = 27,
    "LET" = 28, "LTB" = 29,
    "Lalv" = 30, "Lint" = 31
  ),
  
  # Flows between compartments
  FLOW_INDICES <- list(
    Lung_Plas = 1, Lung_Stom = 2, 
    Stom_SI = 3, SI_Plas = 4, SI_ULI = 5,
    ULI_LLI = 6, LLI_Fece = 7,
    Csur_Plas = 8, Csur_Cdif = 9, 
    Cdif_Csur = 10, Cdif_Cvol = 11,
    Cvol_Plas = 12, Tsur_Plas = 13,
    # ... all 55 flows as in Fortran
  )
)