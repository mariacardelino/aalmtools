#' Constants and parameters for AALM 
#' @export
#' @description Defines all parameters and constants used in the AALM.

#' The following parameters in the Fortran AALM do not need initialization:
    #' PV indicies - handled dynamically during model execution since they are age-dependent.
    #' TS indicies - calculated during runtime based on physiological parameters.
    #' CompName and FlowName character arrays 
    #' Daily intakes/uptake indices - calculated during run.
    #' Variable source tracking arrays (SrcT, SrcTM1, etc) - runtime variables.
    #' Growth parameters - handled thorugh input file and parameter parsing.
    
AALM_constants <- list(
  version = "Mar 18 2025",
  
  # Model dimensions
  N1 = 100,  # Max number of input points per source
  Num_PC = 50,
  Num_PV = 22,
  Num_Comp0 = 27,
  Num_Flow = 55,
  Num_TS = 47,
  Num_MB = 7,
  
  # Mass balance indices
  Yin = 1, Yeat = 2, Yinhal = 3, Ycomp = 4, Yflow = 5, Ybody = 6, Yout = 7,
  
  # Source tracking indices for GI tract
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
  YLET = 1, YLTB = 2, YLalv = 3, YLint = 4,
  F3LTBLET = 5, F3LalvLTB = 6, F3LalvLint = 7,
  F3LETStom = 8, F3LETPlas = 9, F3LTBPlas = 10,
  F3LalvPlas = 11, F3LintPlas = 12,
  
  # Physiological constants (PC)
  Ashwt = 1, BldMot = 2, BonIn = 3, BranIn = 4, Bratio = 5,
  CrtWt = 6, H1toBl = 7, H1toH2 = 8, H1toSI = 9, HepIn = 10,
  IFetal = 11, Kwt = 12, Plsvol = 13, Power = 14, RBCIn = 15,
  RBCnl = 16, RBCVol = 17, RenIn = 18, RKdn1 = 19, RLLI = 20,
  RLvr1 = 21, RPlas = 22, RProt = 23, RSIC = 24, RSof0 = 25,
  RSof1 = 26, RSof2 = 27, RStmc = 28, RULI = 29, S2Hair = 30,
  SatRat = 31, SizeVF = 32, SofIn = 33, TBoneL = 34, TEVF = 35,
  ToFece = 36, ToKdn1 = 37, ToKdn2 = 38, ToLvr1 = 39, ToProt = 40,
  ToRBC = 41, ToSwet = 42, ToUrin = 43, TrbWt = 44, VBLC = 45,
  VKC = 46, VLC = 47, VLUC = 48, HCTA = 49, HCTB = 50,
  
  # Compartments with indices
  Compartments = c(
    Stom = 1, SI = 2, ULI = 3, LLI = 4,
    Csur = 5, Cdif = 6, Cvol = 7, 
    Tsur = 8, Tdif = 9, Tvol = 10,
    Plas = 11, Prot = 12, RBC = 13,
    Lvr1 = 14, Lvr2 = 15,
    Kdn1 = 16, Kdn2 = 17,
    Blad = 18, Bran = 19, EVF = 20,
    Sof0 = 21, Sof1 = 22, Sof2 = 23,
    Urin = 24, Fece = 25, Swet = 26, Hair = 27,
    LET = 28, LTB = 29, Lalv = 30, Lint = 31
  ),
  
  # Flows between compartments
  Flows = c(
    Lung_Plas = 1,    Lung_Stom = 2,    Stom_SI = 3,     SI_Plas = 4,     SI_ULI = 5,
    ULI_LLI = 6,      LLI_Fece = 7,     Csur_Plas = 8,   Csur_Cdif = 9,   Cdif_Csur = 10,
    Cdif_Cvol = 11,   Cvol_Plas = 12,   Tsur_Plas = 13,  Tsur_Tdif = 14,  Tdif_Tsur = 15,
    Tdif_Tvol = 16,   Tvol_Plas = 17,   Lvr1_Lvr2 = 18,  Lvr1_Plas = 19,  Lvr1_SI = 20,
    Lvr2_Plas = 21,   Kdn1_Blad = 22,   Kdn2_Plas = 23,  Blad_Urin = 24,  Bran_Plas = 25,
    EVF_Plas = 26,    Sof0_Plas = 27,   Sof1_Plas = 28,  Sof1_Hair = 29,  Sof2_Plas = 30,
    Prot_Plas = 31,   RBC_Plas = 32,    Plas_SI = 33,    Plas_Prot = 34,  Plas_RBC = 35,
    Plas_EVF = 36,    Plas_Sof0 = 37,   Plas_Sof1 = 38,  Plas_Sof2 = 39,  Plas_Bran = 40,
    Plas_Csur = 41,   Plas_Tsur = 42,   Plas_Lvr1 = 43,  Plas_Kdn1 = 44,  Plas_Kdn2 = 45,
    Plas_Blad = 46,   Plas_Swet = 47,   LET_Plas = 48,   LET_Stom = 49,   LTB_Plas = 50,
    LTB_LET = 51,     Lalv_Plas = 52,   Lalv_LTB = 53,   Lalv_Lint = 54,  Lint_Plas = 55
  )
)