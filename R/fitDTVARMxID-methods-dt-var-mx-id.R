#' Print Method for Object of Class `dtvarmxid`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `dtvarmxid`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @inheritParams coef.dtvarmxid
#'
#' @method print dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
print.dtvarmxid <- function(x,
                            means = FALSE,
                            mu_eta = TRUE,
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
                            digits = 4,
                            ...) {
  print.summary.dtvarmxid(
    summary.dtvarmxid(
      object = x,
      means = means,
      mu_eta = mu_eta,
      alpha = alpha,
      beta = beta,
      nu = nu,
      psi = psi,
      theta = theta,
      converged = converged,
      grad_tol = grad_tol,
      hess_tol = hess_tol,
      vanishing_theta = vanishing_theta,
      theta_tol = theta_tol,
      digits = digits
    )
  )
}

#' Summary Method for Object of Class `dtvarmxid`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `dtvarmxid`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @inheritParams coef.dtvarmxid
#'
#' @method summary dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
summary.dtvarmxid <- function(object,
                              means = FALSE,
                              mu_eta = TRUE,
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
                              digits = 4,
                              ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = object$output,
      FUN = coef,
      mu_eta = mu_eta,
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
    out <- colMeans(out)
  }
  print_summary <- round(
    x = out,
    digits = digits
  )
  class(out) <- c(
    "summary.dtvarmxid",
    class(out)
  )
  attr(out, "fit") <- object
  attr(out, "means") <- means
  attr(out, "mu_eta") <- mu_eta
  attr(out, "alpha") <- alpha
  attr(out, "beta") <- beta
  attr(out, "nu") <- nu
  attr(out, "psi") <- psi
  attr(out, "theta") <- theta
  attr(out, "converged") <- converged
  attr(out, "grad_tol") <- grad_tol
  attr(out, "hess_tol") <- hess_tol
  attr(out, "vanishing_theta") <- vanishing_theta
  attr(out, "theta_tol") <- theta_tol
  attr(out, "digits") <- digits
  attr(out, "print_summary") <- print_summary
  out
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.dtvarmxid
print.summary.dtvarmxid <- function(x,
                                    ...) {
  print_summary <- attr(
    x = x,
    which = "print_summary"
  )
  means <- attr(
    x = x,
    which = "means"
  )
  object <- attr(
    x = x,
    which = "fit"
  )
  cat("Call:\n")
  base::print(object$call)
  if (means) {
    cat("\nMeans of the estimated paramaters per individual.\n")
  } else {
    cat("\nEstimated paramaters per individual.\n")
  }
  print(print_summary)
  invisible(x)
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `dtvarmxid`.
#' @param mu_eta Logical.
#'   If `mu_eta = TRUE`,
#'   include estimates of the `mu_eta` vector, if available.
#'   If `mu_eta = FALSE`,
#'   exclude estimates of the `mu_eta` vector.
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
#' @param grad_tol Numeric scalar.
#'   Tolerance for the maximum absolute gradient
#'   if `converged = TRUE`.
#' @param hess_tol Numeric scalar.
#'   Tolerance for Hessian eigenvalues;
#'   eigenvalues must be strictly greater than this value
#'   if `converged = TRUE`.
#' @param vanishing_theta Logical.
#'   Test for measurement error variance going to zero
#'   if `converged = TRUE`.
#' @param theta_tol Numeric.
#'   Tolerance for vanishing theta test
#'   if `converged` and `theta_tol` are `TRUE`.
#' @param ... additional arguments.
#' @return Returns a list of vectors of parameter estimates.
#'
#' @method coef dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
coef.dtvarmxid <- function(object,
                           mu_eta = TRUE,
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
  if (converged) {
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
  }
  parnames <- names(
    coef(fit[[1]])
  )
  idx <- integer(0)
  if (mu_eta) {
    idx <- c(
      idx,
      grep(
        pattern = "^mu_eta_",
        x = parnames
      )
    )
  }
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
#' @param mu_eta Logical.
#'   If `mu_eta = TRUE`,
#'   include estimates of the `mu_eta` vector, if available.
#'   If `mu_eta = FALSE`,
#'   exclude estimates of the `mu_eta` vector.
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
#' @param grad_tol Numeric scalar.
#'   Tolerance for the maximum absolute gradient
#'   if `converged = TRUE`.
#' @param hess_tol Numeric scalar.
#'   Tolerance for Hessian eigenvalues;
#'   eigenvalues must be strictly greater than this value
#'   if `converged = TRUE`.
#' @param vanishing_theta Logical.
#'   Test for measurement error variance going to zero
#'   if `converged = TRUE`.
#' @param theta_tol Numeric.
#'   Tolerance for vanishing theta test
#'   if `converged` and `theta_tol` are `TRUE`.
#' @param robust Logical.
#'   If `TRUE`, use robust (sandwich) sampling variance-covariance matrix.
#'   If `FALSE`, use normal theory sampling variance-covariance matrix.
#' @param ... additional arguments.
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov dtvarmxid
#' @keywords methods
#' @import OpenMx
#' @export
vcov.dtvarmxid <- function(object,
                           mu_eta = TRUE,
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
                           robust = FALSE,
                           ...) {
  fit <- object$output
  if (robust) {
    if (is.null(object$robust)) {
      fit <- lapply(
        X = fit,
        FUN = function(i) {
          utils::capture.output(
            suppressMessages(
              suppressWarnings(
                sandwich <- OpenMx::imxRobustSE(
                  model = i,
                  details = TRUE
                )
              )
            )
          )
          i@output$vcov <- sandwich$cov
          i@output$standardErrors <- sandwich$SE
          i
        }
      )
    } else {
      fit <- mapply(
        FUN = function(i,
                       sandwich) {
          i@output$vcov <- sandwich$cov
          i@output$standardErrors <- sandwich$SE
          i
        },
        i = fit,
        sandwich = object$robust
      )
    }
  }
  if (converged) {
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
  }
  parnames <- names(
    coef(fit[[1]])
  )
  idx <- integer(0)
  if (mu_eta) {
    idx <- c(
      idx,
      grep(
        pattern = "^mu_eta_",
        x = parnames
      )
    )
  }
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
      code <- tryCatch(
        {
          !(
            is.null(i$output) ||
              is.null(i$output$status) ||
              i$output$status$code != 0L
          )
        },
        error = function(e) {
          FALSE
        }
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
