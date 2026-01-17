.FitDTVARMxIDMuEtaFixed <- function(k,
                                    mu_eta_values,
                                    name) {
  # B
  # latent variables on covariates
  if (is.null(mu_eta_values)) {
    mu_eta <- OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = 1,
      name = name
    )
  } else {
    mu_eta <- OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = FALSE,
      values = mu_eta_values,
      byrow = FALSE,
      name = name
    )
  }
  mu_eta
}
