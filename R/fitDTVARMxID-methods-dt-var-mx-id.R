#' Print Method for Object of Class `dtvarmxid`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `dtvarmxid`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#' @inheritParams coef.dtvarmxid
#'
#' @method print dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
print.dtvarmxid <- function(x,
                            means = FALSE,
                            alpha = TRUE,
                            beta = TRUE,
                            nu = TRUE,
                            psi = TRUE,
                            theta = TRUE,
                            converged = TRUE,
                            grad_tol = 1e-2,
                            hess_tol = 1e-8,
                            vanishing_theta = TRUE,
                            theta_tol = 0.001,
                            ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = x$output,
      FUN = coef,
      alpha = alpha,
      beta = beta,
      nu = nu,
      psi = psi,
      theta = theta,
      converged = converged,
      grad_tol = grad_tol,
      hess_tol = hess_tol,
      vanishing_theta = vanishing_theta,
      theta_tol = theta_tol
    )
  )
  if (means) {
    cat("\nMeans of the estimated paramaters per individual.\n")
    out <- colMeans(out)
  } else {
    cat("\nEstimated paramaters per individual.\n")
  }
  base::print(out)
}

#' Summary Method for Object of Class `dtvarmxid`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `dtvarmxid`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#' @inheritParams coef.dtvarmxid
#'
#' @method summary dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
summary.dtvarmxid <- function(object,
                              means = FALSE,
                              alpha = TRUE,
                              beta = TRUE,
                              nu = TRUE,
                              psi = TRUE,
                              theta = TRUE,
                              converged = TRUE,
                              grad_tol = 1e-2,
                              hess_tol = 1e-8,
                              vanishing_theta = TRUE,
                              theta_tol = 0.001,
                              ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = object$output,
      FUN = coef,
      alpha = alpha,
      beta = beta,
      nu = nu,
      psi = psi,
      theta = theta,
      converged = converged,
      grad_tol = grad_tol,
      hess_tol = hess_tol,
      vanishing_theta = vanishing_theta,
      theta_tol = theta_tol
    )
  )
  if (means) {
    # nocov start
    if (interactive()) {
      cat("\nMeans of the estimated paramaters per individual.\n")
    }
    # nocov end
    out <- colMeans(out)
  } else {
    # nocov start
    if (interactive()) {
      cat("\nEstimated paramaters per individual.\n")
    }
    # nocov end
  }
  out
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `dtvarmxid`.
#' @param alpha Logical.
#'   If `alpha = TRUE`,
#'   include estimates of the `alpha` vector, if available.
#'   If `alpha = FALSE`,
#'   exclude estimates of the `alpha` vector.
#' @param beta Logical.
#'   If `beta = TRUE`,
#'   include estimates of the `beta` matrix, if available.
#'   If `beta = FALSE`,
#'   exclude estimates of the `beta` matrix.
#' @param nu Logical.
#'   If `nu = TRUE`,
#'   include estimates of the `nu` vector, if available.
#'   If `nu = FALSE`,
#'   exclude estimates of the `nu` vector.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix, if available.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix, if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param converged Logical.
#'   Only include converged cases.
#' @param ... additional arguments.
#' @inheritParams converged.dtvarmxid
#' @return Returns a list of vectors of parameter estimates.
#'
#' @method coef dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
coef.dtvarmxid <- function(object,
                           alpha = TRUE,
                           beta = TRUE,
                           nu = TRUE,
                           psi = TRUE,
                           theta = TRUE,
                           converged = TRUE,
                           grad_tol = 1e-2,
                           hess_tol = 1e-8,
                           vanishing_theta = TRUE,
                           theta_tol = 0.001,
                           ...) {
  fit <- object$output
  fit <- fit[
    which(
      converged.dtvarmxid(
        object = object,
        grad_tol = grad_tol,
        hess_tol = hess_tol,
        vanishing_theta = vanishing_theta,
        theta_tol = theta_tol,
        prop = FALSE
      )
    )
  ]
  parnames <- names(
    coef(fit[[1]])
  )
  idx <- integer(0)
  if (alpha) {
    idx <- c(
      idx,
      grep(
        pattern = "^alpha_",
        x = parnames
      )
    )
  }
  if (beta) {
    idx <- c(
      idx,
      grep(
        pattern = "^beta_",
        x = parnames
      )
    )
  }
  if (nu) {
    idx <- c(
      idx,
      grep(
        pattern = "^nu_",
        x = parnames
      )
    )
  }
  if (psi) {
    idx <- c(
      idx,
      grep(
        pattern = "^psi_",
        x = parnames
      )
    )
  }
  if (theta) {
    idx <- c(
      idx,
      grep(
        pattern = "^theta_",
        x = parnames
      )
    )
  }
  lapply(
    X = fit,
    FUN = function(x,
                   idx) {
      coef(x)[idx]
    },
    idx = idx
  )
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `dtvarmxid`.
#' @param alpha Logical.
#'   If `alpha = TRUE`,
#'   include estimates of the `alpha` vector, if available.
#'   If `alpha = FALSE`,
#'   exclude estimates of the `alpha` vector.
#' @param beta Logical.
#'   If `beta = TRUE`,
#'   include estimates of the `beta` matrix, if available.
#'   If `beta = FALSE`,
#'   exclude estimates of the `beta` matrix.
#' @param nu Logical.
#'   If `nu = TRUE`,
#'   include estimates of the `nu` vector, if available.
#'   If `nu = FALSE`,
#'   exclude estimates of the `nu` vector.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix, if available.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix, if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param converged Logical.
#'   Only include converged cases.
#' @param ... additional arguments.
#' @inheritParams converged.dtvarmxid
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
vcov.dtvarmxid <- function(object,
                           alpha = TRUE,
                           beta = TRUE,
                           nu = TRUE,
                           psi = TRUE,
                           theta = TRUE,
                           converged = TRUE,
                           grad_tol = 1e-2,
                           hess_tol = 1e-8,
                           vanishing_theta = TRUE,
                           theta_tol = 0.001,
                           ...) {
  fit <- object$output
  fit <- fit[
    which(
      converged.dtvarmxid(
        object = object,
        grad_tol = grad_tol,
        hess_tol = hess_tol,
        vanishing_theta = vanishing_theta,
        theta_tol = theta_tol,
        prop = FALSE
      )
    )
  ]
  parnames <- names(
    coef(fit[[1]])
  )
  idx <- integer(0)
  if (alpha) {
    idx <- c(
      idx,
      grep(
        pattern = "^alpha_",
        x = parnames
      )
    )
  }
  if (beta) {
    idx <- c(
      idx,
      grep(
        pattern = "^beta_",
        x = parnames
      )
    )
  }
  if (nu) {
    idx <- c(
      idx,
      grep(
        pattern = "^nu_",
        x = parnames
      )
    )
  }
  if (psi) {
    idx <- c(
      idx,
      grep(
        pattern = "^psi_",
        x = parnames
      )
    )
  }
  if (theta) {
    idx <- c(
      idx,
      grep(
        pattern = "^theta_",
        x = parnames
      )
    )
  }
  lapply(
    X = fit,
    FUN = function(x,
                   idx) {
      vcov(x)[idx, idx, drop = FALSE]
    },
    idx = idx
  )
}

#' Check Model Convergence
#'
#' Evaluate whether OpenMx fit has converged successfully.
#'
#' @details Convergence is defined by three criteria:
#' \enumerate{
#'   \item Status code equals `0L`.
#'   \item The maximum absolute gradient is below `grad_tol`.
#'   \item The Hessian is positive definite
#'         with all eigenvalues greater than `hess_tol`.
#'   \item If \code{vanishing_theta = TRUE}, the model additionally checks
#'         that the diagonal elements of the measurement error covariance matrix
#'         (\eqn{\Theta}) are not vanishingly small,
#'         where “small” is defined by `theta_tol`.
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object A fit object.
#' @param ... Passed to and/or used by methods.
#'
#' @keywords methods
#' @import OpenMx
#' @export
converged <- function(object,
                      ...) {
  UseMethod("converged")
}

#' @rdname converged
#' @param object An object of class `dtvarmxid`.
#' @param grad_tol Numeric scalar.
#'   Tolerance for the maximum absolute gradient.
#' @param hess_tol Numeric scalar.
#'   Tolerance for Hessian eigenvalues;
#'   eigenvalues must be strictly greater than this value.
#' @param vanishing_theta Logical.
#'   Test for measurement error variance going to zero.
#' @param theta_tol Numeric.
#'   Tolerance for vanishing theta test.
#' @param prop Logical.
#'   If `prop = FALSE`, a named logical vector indicating,
#'   for each individual fit, whether the convergence criteria are met.
#'   If `prop = TRUE`, the proportion of cases that converged.
#'
#' @return For the `dtvarmxid` method:
#'   If `prop = FALSE`, a named logical vector indicating,
#'   for each individual fit, whether the convergence criteria are met.
#'   If `prop = TRUE`, the proportion of cases that converged.
#'
#' @method converged dtvarmxid
#' @import OpenMx
#' @export
converged.dtvarmxid <- function(object,
                                grad_tol = 1e-2,
                                hess_tol = 1e-8,
                                vanishing_theta = TRUE,
                                theta_tol = 0.001,
                                prop = FALSE,
                                ...) {
  out <- sapply(
    X = object$output,
    FUN = function(i,
                   grad_tol,
                   hess_tol,
                   vanishing_theta,
                   theta_tol) {
      code <- !(
        is.null(i$output) ||
          is.null(i$output$status) ||
          i$output$status$code != 0L
      )
      good_grad <- .MxHelperIsGoodFit(
        x = i,
        tol = grad_tol
      )
      pd_hessian <- .MxHelperHasPdHessian(
        x = i,
        tol = hess_tol
      )
      good_fit <- good_grad && pd_hessian
      theta_ok <- TRUE
      if (vanishing_theta) {
        parnames <- names(
          coef(i)
        )
        has_theta <- any(
          grepl(
            "^theta_",
            parnames,
            perl = TRUE
          )
        )
        if (has_theta) {
          theta_diag <- tryCatch(
            diag(
              OpenMx::mxEvalByName(
                name = "theta",
                model = i
              )
            ),
            error = function(e) {
              numeric(0)
            }
          )
          if (length(theta_diag) > 0) {
            theta_ok <- all(theta_diag > theta_tol)
          } else {
            theta_ok <- TRUE
          }
        } else {
          theta_ok <- TRUE
        }
      }
      good_fit && theta_ok && code
    },
    grad_tol = grad_tol,
    hess_tol = hess_tol,
    vanishing_theta = vanishing_theta,
    theta_tol = theta_tol
  )
  if (prop) {
    out <- mean(out)
  } else {
    files <- names(out)
    clean_names <- sub(
      pattern = ".*ID(.*)\\.Rds$",
      replacement = "\\1",
      x = files
    )
    names(out) <- clean_names
  }
  out
}
