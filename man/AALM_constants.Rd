% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AALM_constants.R
\docType{data}
\name{AALM_constants}
\alias{AALM_constants}
\title{Constants and parameters for AALM}
\format{
An object of class \code{list} of length 94.
}
\usage{
AALM_constants
}
\description{
Defines all parameters and constants used in the AALM.
The following parameters in the Fortran AALM do not need initialization:
PV indicies - handled dynamically during model execution since they are age-dependent.
TS indicies - calculated during runtime based on physiological parameters.
CompName and FlowName character arrays 
Daily intakes/uptake indices - calculated during run.
Variable source tracking arrays (SrcT, SrcTM1, etc) - runtime variables.
Growth parameters - handled thorugh input file and parameter parsing.
}
\keyword{datasets}
