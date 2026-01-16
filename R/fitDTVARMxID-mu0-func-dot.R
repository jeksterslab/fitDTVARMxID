.FitDTVARMxIDMu0Func <- function(k,
                                 statenames,
                                 name,
                                 name_beta,
                                 name_alpha,
                                 center) {
  # x0
  # initial condition
  # mean
  # alpha is specified as a covariate (gamma)
  # mu0_values will implied by the algebra
  mu0_iden <- paste0(
    name,
    "_iden"
  )
  mu0_vec <- paste0(
    name,
    "_vec"
  )
  beta <- name_beta
  alpha <- name_alpha
  if (center) {
    out <- list(
      mu0 = OpenMx::mxAlgebraFromString(
        algString = alpha,
        name = name,
        dimnames = list(
          statenames,
          name
        )
      ),
      mu0_vec = OpenMx::mxAlgebraFromString(
        algString = paste0(
          "cvectorize(",
          name,
          ")"
        ),
        name = mu0_vec
      )
    )
  } else {
    out <- list(
      mu0_iden = OpenMx::mxMatrix(
        type = "Iden",
        nrow = k,
        ncol = k,
        name = mu0_iden
      ),
      mu0 = OpenMx::mxAlgebraFromString(
        algString = paste0(
          "solve(",
          mu0_iden,
          " - ",
          beta,
          ") %*% ",
          alpha
        ),
        name = name,
        dimnames = list(
          statenames,
          name
        )
      ),
      mu0_vec = OpenMx::mxAlgebraFromString(
        algString = paste0(
          "cvectorize(",
          name,
          ")"
        ),
        name = mu0_vec
      )
    )
  }
  out
}
