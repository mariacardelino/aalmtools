#' Calculate compartment activity using analytical solution
#' Direct port of Fortran Actvty function
#' @param Y Current amount in compartment
#' @param P Total input rate
#' @param X Transfer coefficient (output rate)
#' @param D Time step
#' @return Updated amount in compartment
#' @export
Actvty <- function(Y, P, X, D) {
  if(X <= 0) {
    # No removal - accumulation only
    return(Y + P * D)
  } else {
    # Analytical solution for first-order kinetics
    return((P/X) + (Y - P/X) * exp(-X * D))
  }
}