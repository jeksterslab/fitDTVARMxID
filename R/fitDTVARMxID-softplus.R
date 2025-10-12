#' Softplus and Inverse Softplus Transformations
#'
#' The softplus transformation maps unconstrained real values to the
#' positive real line. This is useful when parameters (e.g., variances)
#' must be strictly positive. The inverse softplus transformation
#' recovers the unconstrained value from a positive input.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector or matrix.
#'   Input values to be transformed.
#'
#' @return
#' - `Softplus()`: numeric vector or matrix of strictly positive values.
#' - `InvSoftplus()`: numeric vector or matrix of unconstrained values.
#'
#' @details
#' - `Softplus(x) = log(1 + exp(x))`
#' - `InvSoftplus(x) = log(exp(x) - 1)`
#'
#' For numerical stability, these functions use `log1p()` and `expm1()`
#' internally.
#'
#' @examples
#' # Apply softplus to unconstrained values
#' x <- c(-5, 0, 5)
#' y <- Softplus(x)
#'
#' # Recover unconstrained values
#' x_recovered <- InvSoftplus(y)
#'
#' y
#' x_recovered
#'
#' @family DTVAR Functions
#' @keywords fitDTVARMxID misc
#' @name Softplus
NULL

#' @rdname Softplus
#' @export
Softplus <- function(x) {
  log1p(exp(x))
}

#' @rdname Softplus
#' @export
InvSoftplus <- function(x) {
  log(expm1(x))
}
