#' Analytical solution for compartment activity
#' Direct port of Fortran Actvty function
#' @param Y Current amount
#' @param P Input rate
#' @param X Transfer coefficient
#' @param D Time step
#' @export
Actvty <- function(Y, P, X, D) {
  if(X == 0) {
    return(Y + P * D)
  } else {
    return((Y - P/X) * exp(-X * D) + P/X)
  }
}

