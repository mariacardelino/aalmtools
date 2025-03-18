#' aalmtools: Age-dependent Adult Lead Model Tools
#'
#' @description
#' Implementation of the All-Ages Lead Model (AALM) in R with support for
#' parameter estimation, simulation, and visualization.
#'
#' @docType _PACKAGE
#' @name aalmtools
#'
#' @import shiny
#' @import shinydashboard
#' @import DT
#' @import plotly
#' @import ggplot2
#' @import MCMCpack
#' @import shinyBS
#' @import shinyjs
#' @import rjags
#' @import coda
#'
#' @importFrom stats update sd mean var
#' @importFrom utils write.csv read.csv write.table packageVersion
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{run_AALM}}: Run AALM simulation
#'   \item \code{\link{estimate_ingestion_rates}}: Bayesian estimation
#'   \item \code{\link{write_outputs_safe}}: Write model outputs
#' }
NULL