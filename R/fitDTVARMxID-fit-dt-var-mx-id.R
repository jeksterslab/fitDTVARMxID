#' Fit the First-Order Discrete-Time Vector Autoregressive Model by ID
#'
#' The function fits
#' the first-order discrete-time vector autoregressive model
#' for each unit ID.
#'
#' @details The measurement model is given by
#' \deqn{
#'   \mathbf{y}_{i, t}
#'   =
#'   \boldsymbol{\nu}
#'   +
#'   \boldsymbol{\Lambda}
#'   \boldsymbol{\eta}_{i, t}
#'   +
#'   \boldsymbol{\varepsilon}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\varepsilon}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Theta}
#'   \right)
#' }
#' where
#' \eqn{\mathbf{y}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' and
#' \eqn{\boldsymbol{\varepsilon}_{i, t}}
#' are random variables
#' and
#' \eqn{\boldsymbol{\nu}},
#' \eqn{\boldsymbol{\Lambda}},
#' and
#' \eqn{\boldsymbol{\Theta}}
#' are model parameters.
#' \eqn{\mathbf{y}_{i, t}}
#' represents a vector of observed random variables,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' a vector of latent random variables,
#' and
#' \eqn{\boldsymbol{\varepsilon}_{i, t}}
#' a vector of random measurement errors,
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\Lambda}}
#' denotes a matrix of factor loadings,
#' and
#' \eqn{\boldsymbol{\Theta}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\varepsilon}}.
#' In this model,
#' \eqn{\boldsymbol{\Lambda}} is an identity matrix and
#' \eqn{\boldsymbol{\Theta}} is a diagonal matrix.
#'
#' The dynamic structure is given by
#' \deqn{
#'   \boldsymbol{\eta}_{i, t}
#'   =
#'   \boldsymbol{\alpha}
#'   +
#'   \boldsymbol{\beta}
#'   \boldsymbol{\eta}_{i, t - 1}
#'   +
#'   \boldsymbol{\zeta}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\zeta}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Psi}
#'   \right)
#' }
#' where
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' are random variables,
#' and
#' \eqn{\boldsymbol{\alpha}},
#' \eqn{\boldsymbol{\beta}},
#' and
#' \eqn{\boldsymbol{\Psi}}
#' are model parameters.
#' Here,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' is a vector of latent variables
#' at time \eqn{t} and individual \eqn{i},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}}
#' represents a vector of latent variables
#' at time \eqn{t - 1} and individual \eqn{i},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' represents a vector of dynamic noise
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\alpha}}
#' denotes a vector of intercepts,
#' \eqn{\boldsymbol{\beta}}
#' a matrix of autoregression
#' and cross regression coefficients,
#' and
#' \eqn{\boldsymbol{\Psi}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\zeta}_{i, t}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param data Data frame.
#'   A data frame object of data for potentially
#'   multiple subjects that contain
#'   a column of subject ID numbers
#'   (i.e., an ID variable), and
#'   at least one column of observed values.
#' @param observed Character vector.
#'   A vector of character strings
#'   of the names of the observed variables in the data.
#' @param id Character string.
#'   A character string of the name of the ID variable in the data.
#' @param alpha_fixed Logical.
#'   If `TRUE`, the dynamic model intercept vector `alpha` is fixed.
#'   If `FALSE`, `alpha` is estimated.
#' @param alpha_free Logical vector indicating which elements of `alpha`
#'   are freely estimated. If `NULL`, all elements are free.
#'   Ignored if `alpha_fixed = TRUE`.
#' @param alpha_values Numeric vector of values for `alpha`.
#'   If `alpha_fixed = TRUE`, these are fixed values.
#'   If `alpha_fixed = FALSE`, these are starting values.
#'   If `NULL`, defaults to a vector of zeros.
#' @param alpha_lbound Numeric vector of lower bounds for `alpha`.
#'   If `NULL`, no lower bounds are set.
#'   Ignored if `alpha_fixed = TRUE`.
#' @param alpha_ubound Numeric vector of upper bounds for `alpha`.
#'   If `NULL`, no upper bounds are set.
#'   Ignored if `alpha_fixed = TRUE`.
#' @param beta_fixed Logical.
#'   If `TRUE`, the dynamic model coefficient matrix `beta` is fixed.
#'   If `FALSE`, `beta` is estimated.
#' @param beta_free Logical matrix indicating which elements of `beta`
#'   are freely estimated. If `NULL`, all elements are free.
#'   Ignored if `beta_fixed = TRUE`.
#' @param beta_values Numeric matrix of values for `beta`.
#'   If `beta_fixed = TRUE`, these are fixed values.
#'   If `beta_fixed = FALSE`, these are starting values.
#'   If `NULL`, defaults to a zero matrix.
#' @param beta_lbound Numeric matrix of lower bounds for `beta`.
#'   If `NULL`, defaults to -1.5.
#'   Ignored if `beta_fixed = TRUE`.
#' @param beta_ubound Numeric matrix of upper bounds for `beta`.
#'   If `NULL`, defaults to +1.5.
#'   Ignored if `beta_fixed = TRUE`.
#' @param psi_diag Logical.
#'   If `TRUE`, `psi` is diagonal.
#'   If `FALSE`, `psi` is symmetric.
#' @param psi_d_free Logical vector
#'   indicating free/fixed status of the elements of `psi_d`.
#'   If `NULL`, all element of `psi_d` are free.
#' @param psi_d_values Numeric vector
#'   with starting values for `psi_d`.
#'   If `NULL`, defaults to a vector of ones.
#' @param psi_d_lbound Numeric vector
#'   with lower bounds for `psi_d`.
#'   If `NULL`, no lower bounds are set.
#' @param psi_d_ubound Numeric vector
#'   with upper bounds for `psi_d`.
#'   If `NULL`, no upper bounds are set.
#' @param psi_l_free Logical matrix
#'   indicating which strictly-lower-triangular elements of `psi_l` are free.
#'   Ignored if `psi_diag = TRUE`.
#' @param psi_l_values Numeric matrix
#'   of starting values for the strictly-lower-triangular elements of `psi_l`.
#'   If `NULL`, defaults to a null matrix.
#' @param psi_l_lbound Numeric matrix
#'   with lower bounds for `psi_l`.
#'   If `NULL`, no lower bounds are set.
#' @param psi_l_ubound Numeric matrix
#'   with upper bounds for `psi_l`.
#'   If `NULL`, no upper bounds are set.
#' @param nu_fixed Logical.
#'   If `TRUE`, the measurement model intercept vector `nu` is fixed.
#'   If `FALSE`, `nu` is estimated.
#' @param nu_free Logical vector indicating which elements of `nu`
#'   are freely estimated. If `NULL`, all elements are free.
#'   Ignored if `nu_fixed = TRUE`.
#' @param nu_values Numeric vector of values for `nu`.
#'   If `nu_fixed = TRUE`, these are fixed values.
#'   If `nu_fixed = FALSE`, these are starting values.
#'   If `NULL`, defaults to a vector of zeros.
#' @param nu_lbound Numeric vector of lower bounds for `nu`.
#'   If `NULL`, no lower bounds are set.
#'   Ignored if `nu_fixed = TRUE`.
#' @param nu_ubound Numeric vector of upper bounds for `nu`.
#'   If `NULL`, no upper bounds are set.
#'   Ignored if `nu_fixed = TRUE`.
#' @param theta_diag Logical.
#'   If `TRUE`, `theta` is diagonal.
#'   If `FALSE`, `theta` is symmetric.
#' @param theta_fixed Logical.
#'   If `TRUE`, the measurement error matrix `theta`
#'   is fixed to `SoftPlus(theta_d_values)`.
#'   If `FALSE`, only diagonal elements are estimated
#'   (off-diagonals fixed to zero).
#' @param theta_d_free Logical vector
#'   indicating free/fixed status of the diagonal parameters `theta_d`.
#'   If `NULL`, all element of `theta_d` are free.
#' @param theta_d_values Numeric vector
#'   with starting values for `theta_d`.
#'   If `theta_fixed = TRUE`, these are fixed values.
#'   If `theta_fixed = FALSE`, these are starting values.
#'   If `NULL`, defaults to an identity matrix.
#' @param theta_d_lbound Numeric vector
#'   with lower bounds for `theta_d`.
#'   If `NULL`, no lower bounds are set.
#' @param theta_d_ubound Numeric vector
#'   with upper bounds for `theta_d`.
#'   If `NULL`, no upper bounds are set.
#' @param theta_d_equal Logical.
#'   When `TRUE`, all free diagonal elements of `theta_d` are constrained
#'   to be equal and estimated as a single shared parameter (`theta_eq`).
#'   Ignored if no diagonal elements are free.
#' @param theta_l_free Logical matrix
#'   indicating which strictly-lower-triangular elements of `theta_l` are free.
#'   Ignored if `theta_diag = TRUE`.
#' @param theta_l_values Numeric matrix
#'   of starting values for the strictly-lower-triangular elements of `theta_l`.
#'   If `NULL`, defaults to a null matrix.
#' @param theta_l_lbound Numeric matrix
#'   with lower bounds for `theta_l`.
#'   If `NULL`, no lower bounds are set.
#' @param theta_l_ubound Numeric matrix
#'   with upper bounds for `theta_l`.
#'   If `NULL`, no upper bounds are set.
#' @param mu0_fixed Logical.
#'   If `TRUE`, the initial mean vector `mu0` is fixed.
#'   If `FALSE`, `mu0` is estimated.
#' @param mu0_func Logical.
#'   If `TRUE` and `mu0_fixed = TRUE`, `mu0` is fixed to
#'   \eqn{ (I - \beta)^{-1} \alpha }.
#' @param mu0_free Logical vector indicating which elements of `mu0`
#'   are freely estimated.
#' @param mu0_values Numeric vector of values for `mu0`.
#'   If `mu0_fixed = TRUE`, these are fixed values.
#'   If `mu0_fixed = FALSE`, these are starting values.
#'   If `NULL`, defaults to a vector of zeros.
#' @param mu0_lbound Numeric vector of lower bounds for `mu0`.
#'   If `NULL`, no lower bounds are set.
#'   Ignored if `mu0_fixed = TRUE`.
#' @param mu0_ubound Numeric vector of upper bounds for `mu0`.
#'   If `NULL`, no upper bounds are set.
#'   Ignored if `mu0_fixed = TRUE`.
#' @param sigma0_fixed Logical.
#'   If `TRUE`, the initial covariance matrix `sigma0` is fixed.
#'   If `FALSE`, `sigma0` is estimated.
#' @param sigma0_func Logical.
#'   If `TRUE` and `sigma0_fixed = TRUE`, `sigma0` is fixed to
#'   \eqn{ (I - \beta \otimes \beta)^{-1} \mathrm{Vec}(\Psi) }.
#' @param sigma0_diag Logical.
#'   If `TRUE`, `sigma0` is diagonal.
#'   If `FALSE`, `sigma0` is symmetric.
#' @param sigma0_d_free Logical vector
#'   indicating free/fixed status of the elements of `sigma0_d`.
#'   If `NULL`, all element of `sigma0_d` are free.
#' @param sigma0_d_values Numeric vector
#'   with starting values for `sigma0_d`.
#'   If `NULL`, defaults to a vector of ones.
#' @param sigma0_d_lbound Numeric vector
#'   with lower bounds for `sigma0_d`.
#'   If `NULL`, no lower bounds are set.
#' @param sigma0_d_ubound Numeric vector
#'   with upper bounds for `sigma0_d`.
#'   If `NULL`, no upper bounds are set.
#' @param sigma0_l_free Logical matrix
#'   indicating which
#'   strictly-lower-triangular elements of `sigma0_l` are free.
#'   Ignored if `sigma0_diag = TRUE`.
#' @param sigma0_l_values Numeric matrix
#'   of starting values
#'   for the strictly-lower-triangular elements of `sigma0_l`.
#'   If `NULL`, defaults to a null matrix.
#' @param sigma0_l_lbound Numeric matrix
#'   with lower bounds for `sigma0_l`.
#'   If `NULL`, no lower bounds are set.
#' @param sigma0_l_ubound Numeric matrix
#'   with upper bounds for `sigma0_l`.
#'   If `NULL`, no upper bounds are set.
#' @param robust Logical.
#'   If `TRUE`, calculate robust (sandwich) sampling variance-covariance matrix.
#' @param tries_explore Integer.
#'   Number of extra tries for the wide exploration
#'   phase using `OpenMx::mxTryHardWideSearch()` with `checkHess = FALSE`.
#' @param tries_local Integer.
#'   Number of extra tries for local polishing via
#'   `OpenMx::mxTryHard()` when gradients remain above tolerance.
#' @param max_attempts Integer.
#'   Maximum number of remediation attempts
#'   after the first Hessian computation fails the criteria.
#'   Each attempt may nudge off bounds,
#'   refit locally without the Hessian, and,
#'   on the last attempt, relax bounds.
#' @param grad_tol Numeric.
#'   Tolerance for the maximum absolute gradient.
#'   Smaller values are stricter.
#' @param hess_tol Numeric.
#'   Minimum allowable Hessian eigenvalue.
#'   Smaller values are less strict.
#' @param eps Numeric.
#'   Proximity threshold to detect parameters on their bounds
#'   and to nudge them inward by `10 * eps`.
#' @param factor Numeric.
#'   Multiplicative factor to relax parameter bounds
#'   on the final remediation attempt.
#'   Lower bounds are divided by `factor` and
#'   upper bounds are multiplied by `factor`.
#' @param overwrite Logical.
#'   If `TRUE`, existing intermediate files are overwritten.
#'   Defaults to `FALSE`.
#' @param path Character string.
#'   Directory in which to save intermediate files.
#' @param prefix Alphanumeric character string.
#'   Prefix to use when naming intermediate files.
#' @param seed Random seed for reproducibility.
#' @param silent Logical.
#'   If `TRUE`, suppresses messages during the model fitting stage.
#' @param ncores Positive integer.
#'   Number of cores to use.
#' @param clean Logical.
#'   If `TRUE`, clean intermediate files saved in `path`.
#'
#' @return Returns an object of class `dtvarmxid` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{fun}{Function used ("FitDTVARMxID").}
#'     \item{output}{A list of fitted OpenMx models.}
#'     \item{robust}{A list of output from [OpenMx::imxRobustSE()]
#'         with argument `details = TRUE` if `random = TRUE`
#'         for each `id`.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Generate data using the simStateSpace package------------------------------
#' set.seed(42)
#' k <- 2
#' n <- 5
#' time <- 100
#' alpha <- rep(x = 0, times = k)
#' beta <- matrix(
#'   data = c(.5, .0, .2, .5),
#'   nrow = k,
#'   ncol = k
#' )
#' psi <- matrix(
#'   data = c(exp(-4.1), exp(-3.9), exp(-3.9), exp(-3.2)),
#'   nrow = k,
#'   ncol = k
#' )
#' psi_l <- t(chol(psi))
#' nu <- rep(x = 5, times = k)
#' lambda <- diag(k)
#' theta <- matrix(
#'   data = c(exp(-2), 0, 0, exp(-2.8)),
#'   nrow = k,
#'   ncol = k
#' )
#' theta_l <- t(chol(theta))
#' mu0 <- c(solve(diag(k) - beta) %*% alpha)
#' sigma0 <- matrix(
#'   data = c(
#'     solve(diag(k * k) - beta %x% beta) %*% c(psi)
#'   ),
#'   nrow = k,
#'   ncol = k
#' )
#' sigma0_l <- t(chol(sigma0))
#' sim <- simStateSpace::SimSSMIVary(
#'   n = n,
#'   time,
#'   mu0 = list(mu0),
#'   sigma0_l = list(sigma0_l),
#'   alpha = list(alpha),
#'   beta = simStateSpace::SimBetaN(
#'     n = n,
#'     beta = beta,
#'     vcov_beta_vec_l = t(chol(0.1 * diag(k * k)))
#'   ),
#'   psi_l = list(psi_l),
#'   nu = list(nu),
#'   lambda = list(lambda),
#'   theta_l = list(theta_l)
#' )
#' data <- as.data.frame(sim)
#'
#' # Fit the model--------------------------------------------------------------
#' library(fitDTVARMxID)
#' fit <- FitDTVARMxID(
#'   data = data,
#'   observed = paste0("y", seq_len(k)),
#'   id = "id"
#' )
#' print(fit)
#' summary(fit)
#' coef(fit)
#' vcov(fit)
#' converged(fit)
#' }
#'
#' @references
#' Hunter, M. D. (2017).
#' State space modeling in an open source, modular,
#' structural equation modeling environment.
#' *Structural Equation Modeling: A Multidisciplinary Journal*,
#' *25*(2), 307–324.
#' \doi{10.1080/10705511.2017.1369354}
#'
#' Neale, M. C., Hunter, M. D., Pritikin, J. N.,
#' Zahery, M., Brick, T. R., Kirkpatrick, R. M., Estabrook, R.,
#' Bates, T. C., Maes, H. H., & Boker, S. M. (2015).
#' OpenMx 2.0: Extended structural equation and statistical modeling.
#' *Psychometrika*,
#' *81*(2), 535–549.
#' \doi{10.1007/s11336-014-9435-8}
#'
#' @family DTVAR Functions
#' @keywords fitDTVARMxID fit
#' @import OpenMx
#' @importFrom stats coef vcov
#' @export
FitDTVARMxID <- function(data,
                         observed,
                         id,
                         alpha_fixed = TRUE,
                         alpha_free = NULL,
                         alpha_values = NULL,
                         alpha_lbound = NULL,
                         alpha_ubound = NULL,
                         beta_fixed = FALSE,
                         beta_free = NULL,
                         beta_values = NULL,
                         beta_lbound = NULL,
                         beta_ubound = NULL,
                         psi_diag = FALSE,
                         psi_d_free = NULL,
                         psi_d_values = NULL,
                         psi_d_lbound = NULL,
                         psi_d_ubound = NULL,
                         psi_l_free = NULL,
                         psi_l_values = NULL,
                         psi_l_lbound = NULL,
                         psi_l_ubound = NULL,
                         nu_fixed = FALSE,
                         nu_free = NULL,
                         nu_values = NULL,
                         nu_lbound = NULL,
                         nu_ubound = NULL,
                         theta_diag = TRUE,
                         theta_fixed = FALSE,
                         theta_d_free = NULL,
                         theta_d_values = NULL,
                         theta_d_lbound = NULL,
                         theta_d_ubound = NULL,
                         theta_d_equal = FALSE,
                         theta_l_free = NULL,
                         theta_l_values = NULL,
                         theta_l_lbound = NULL,
                         theta_l_ubound = NULL,
                         mu0_fixed = TRUE,
                         mu0_func = FALSE,
                         mu0_free = NULL,
                         mu0_values = NULL,
                         mu0_lbound = NULL,
                         mu0_ubound = NULL,
                         sigma0_fixed = TRUE,
                         sigma0_func = FALSE,
                         sigma0_diag = FALSE,
                         sigma0_d_free = NULL,
                         sigma0_d_values = NULL,
                         sigma0_d_lbound = NULL,
                         sigma0_d_ubound = NULL,
                         sigma0_l_free = NULL,
                         sigma0_l_values = NULL,
                         sigma0_l_lbound = NULL,
                         sigma0_l_ubound = NULL,
                         robust = FALSE,
                         tries_explore = 100,
                         tries_local = 10,
                         max_attempts = 10,
                         grad_tol = 1e-2,
                         hess_tol = 1e-8,
                         eps = 1e-6,
                         factor = 10,
                         overwrite = FALSE,
                         path = getwd(),
                         prefix = "FitDTVARMxID",
                         seed = NULL,
                         silent = FALSE,
                         ncores = NULL,
                         clean = TRUE) {
  stopifnot(
    dir.exists(path),
    grepl(
      pattern = "^[A-Za-z0-9]+$",
      x = prefix
    )
  )
  if (!silent) {
    if (interactive()) {
      message(
        paste0(
          "Intermediate files will be saved in ",
          path,
          "\n"
        )
      )
    }
  }
  args <- list(
    data = data,
    observed = observed,
    id = id,
    alpha_fixed = alpha_fixed,
    alpha_free = alpha_free,
    alpha_values = alpha_values,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound,
    beta_fixed = beta_fixed,
    beta_free = beta_free,
    beta_values = beta_values,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_diag = psi_diag,
    psi_d_free = psi_d_free,
    psi_d_values = psi_d_values,
    psi_d_lbound = psi_d_lbound,
    psi_d_ubound = psi_d_ubound,
    psi_l_free = psi_l_free,
    psi_l_values = psi_l_values,
    psi_l_lbound = psi_l_lbound,
    psi_l_ubound = psi_l_ubound,
    nu_fixed = nu_fixed,
    nu_free = nu_free,
    nu_values = nu_values,
    nu_lbound = nu_lbound,
    nu_ubound = nu_ubound,
    theta_diag = theta_diag,
    theta_fixed = theta_fixed,
    theta_d_free = theta_d_free,
    theta_d_values = theta_d_values,
    theta_d_lbound = theta_d_lbound,
    theta_d_ubound = theta_d_ubound,
    theta_d_equal = theta_d_equal,
    theta_l_free = theta_l_free,
    theta_l_values = theta_l_values,
    theta_l_lbound = theta_l_lbound,
    theta_l_ubound = theta_l_ubound,
    mu0_fixed = mu0_fixed,
    mu0_func = mu0_func,
    mu0_free = mu0_free,
    mu0_values = mu0_values,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_func = sigma0_func,
    sigma0_diag = sigma0_diag,
    sigma0_d_free = sigma0_d_free,
    sigma0_d_values = sigma0_d_values,
    sigma0_d_lbound = sigma0_d_lbound,
    sigma0_d_ubound = sigma0_d_ubound,
    sigma0_l_free = sigma0_l_free,
    sigma0_l_values = sigma0_l_values,
    sigma0_l_lbound = sigma0_l_lbound,
    sigma0_l_ubound = sigma0_l_ubound,
    tries_explore = tries_explore,
    tries_local = tries_local,
    max_attempts = max_attempts,
    grad_tol = grad_tol,
    hess_tol = hess_tol,
    eps = eps,
    factor = factor,
    overwrite = overwrite,
    path = path,
    prefix = prefix,
    seed = seed,
    silent = silent,
    ncores = ncores,
    clean = clean
  )
  output <- .FitDTVARMxID(
    data = data,
    observed = observed,
    id = id,
    alpha_fixed = alpha_fixed,
    alpha_free = alpha_free,
    alpha_values = alpha_values,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound,
    beta_fixed = beta_fixed,
    beta_free = beta_free,
    beta_values = beta_values,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_diag = psi_diag,
    psi_d_free = psi_d_free,
    psi_d_values = psi_d_values,
    psi_d_lbound = psi_d_lbound,
    psi_d_ubound = psi_d_ubound,
    psi_l_free = psi_l_free,
    psi_l_values = psi_l_values,
    psi_l_lbound = psi_l_lbound,
    psi_l_ubound = psi_l_ubound,
    nu_fixed = nu_fixed,
    nu_free = nu_free,
    nu_values = nu_values,
    nu_lbound = nu_lbound,
    nu_ubound = nu_ubound,
    theta_diag = theta_diag,
    theta_fixed = theta_fixed,
    theta_d_free = theta_d_free,
    theta_d_values = theta_d_values,
    theta_d_lbound = theta_d_lbound,
    theta_d_ubound = theta_d_ubound,
    theta_d_equal = theta_d_equal,
    theta_l_free = theta_l_free,
    theta_l_values = theta_l_values,
    theta_l_lbound = theta_l_lbound,
    theta_l_ubound = theta_l_ubound,
    mu0_fixed = mu0_fixed,
    mu0_func = mu0_func,
    mu0_free = mu0_free,
    mu0_values = mu0_values,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_func = sigma0_func,
    sigma0_diag = sigma0_diag,
    sigma0_d_free = sigma0_d_free,
    sigma0_d_values = sigma0_d_values,
    sigma0_d_lbound = sigma0_d_lbound,
    sigma0_d_ubound = sigma0_d_ubound,
    sigma0_l_free = sigma0_l_free,
    sigma0_l_values = sigma0_l_values,
    sigma0_l_lbound = sigma0_l_lbound,
    sigma0_l_ubound = sigma0_l_ubound,
    tries_explore = tries_explore,
    tries_local = tries_local,
    max_attempts = max_attempts,
    grad_tol = grad_tol,
    hess_tol = hess_tol,
    eps = eps,
    factor = factor,
    overwrite = overwrite,
    path = path,
    prefix = prefix,
    seed = seed,
    silent = silent,
    ncores = ncores,
    clean = clean
  )
  if (robust) {
    robust <- .Robust(
      fit = output,
      ncores = ncores
    )
  } else {
    robust <- NULL
  }
  out <- list(
    call = match.call(),
    args = args,
    fun = "FitDTVARMxID",
    output = output,
    robust = robust
  )
  class(out) <- c(
    "dtvarmxid",
    class(out)
  )
  out
}
