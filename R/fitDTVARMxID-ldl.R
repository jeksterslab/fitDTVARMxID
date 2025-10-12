#' LDL' Decomposition of a Symmetric Positive-Definite Matrix
#'
#' Performs an LDL' factorization of a symmetric positive-definite
#' matrix \eqn{X}, such that
#' \deqn{X = L D L^\top,}
#' where \eqn{L} is unit lower-triangular (ones on the diagonal)
#' and \eqn{D} is diagonal.
#'
#' @param x Numeric matrix. Must be symmetric positive-definite.
#'
#' @details
#' This function returns both the unit lower-triangular factor \eqn{L}
#' and the diagonal factor \eqn{D}.
#' The strictly lower-triangular part of \eqn{L} is also provided
#' for convenience. The function additionally computes an unconstrained
#' vector `d_uc` such that `softplus(d_uc) = d_vec`, using
#' \eqn{\mathrm{softplus}^{-1}(y) = \log(\exp(y) - 1)} for stable
#' back-transformation.
#'
#' @returns A list with components:
#' \item{l_mat_unit}{Unit lower-triangular matrix \eqn{L}.}
#' \item{l_mat_strict}{Strictly lower-triangular part of \eqn{L}.}
#' \item{d_mat}{Diagonal matrix \eqn{D}.}
#' \item{d_vec}{Vector of diagonal entries of \eqn{D}.}
#' \item{d_uc}{Unconstrained vector with
#'   \eqn{\mathrm{softplus}(d\_uc) = d\_vec}.}
#' \item{x}{Original input matrix.}
#' \item{y}{Reconstructed matrix \eqn{L D L^\top}.}
#' \item{diff}{Difference `x - y`.}
#'
#' @examples
#' set.seed(123)
#' A <- matrix(rnorm(16), 4, 4)
#' S <- crossprod(A) + diag(1e-6, 4) # SPD
#' out <- LDL(S)
#' max(abs(out$diff))
#'
#' @family DTVAR Functions
#' @keywords fitDTVARMxID misc
#' @export
LDL <- function(x) {
  k <- nrow(x)
  l_mat_unit <- diag(1, k)
  d_vec <- numeric(k)
  for (i in seq_len(k)) {
    # compute d_mat[i]
    if (i == 1) {
      d_vec[i] <- x[i, i]
    } else {
      s <- 0.0
      for (p in seq_len(i - 1)) {
        s <- s + l_mat_unit[i, p] * l_mat_unit[i, p] * d_vec[p]
      }
      d_vec[i] <- x[i, i] - s
    }
    if (i < k) {
      for (j in (i + 1):k) {
        s <- 0.0
        if (i > 1) {
          for (p in seq_len(i - 1)) {
            s <- s + l_mat_unit[j, p] * l_mat_unit[i, p] * d_vec[p]
          }
        }
        l_mat_unit[j, i] <- (x[j, i] - s) / d_vec[i]
      }
    }
  }
  d_mat <- diag(d_vec)
  l_mat_strict <- l_mat_unit - diag(1, k)
  out <- list(
    l_mat_unit = l_mat_unit,
    l_mat_strict = l_mat_strict,
    d_mat = d_mat,
    d_vec = d_vec,
    d_uc = log(expm1(d_vec)),
    x = x,
    y = l_mat_unit %*% d_mat %*% t(l_mat_unit)
  )
  out$diff <- out$x - out$y
  out
}
