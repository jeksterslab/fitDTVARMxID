.FitDTVARMxIDMuFunc <- function(k,
                                statenames,
                                center,
                                name,
                                name_beta,
                                name_alpha) {
  # mu_values will implied by the algebra
  # stable mean
  # alpha is specified as a covariate (gamma)
  if (center) {
    mu <- list(
      OpenMx::mxAlgebraFromString(
        algString = "mu_eta",
        name = name,
        dimnames = list(
          statenames,
          name
        )
      )
    )
  } else {
    mu_iden <- paste0(
      name,
      "_iden"
    )
    beta <- name_beta
    alpha <- name_alpha
    mu <- list(
      mu_iden = OpenMx::mxMatrix(
        type = "Iden",
        nrow = k,
        ncol = k,
        name = mu_iden
      ),
      mu = OpenMx::mxAlgebraFromString(
        algString = paste0(
          "solve(",
          mu_iden,
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
      )
    )
  }
  mu
}
