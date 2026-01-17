.FitDTVARMxIDMuEta <- function(k,
                               statenames,
                               center,
                               mu_eta_fixed,
                               mu_eta_free,
                               mu_eta_values,
                               mu_eta_lbound,
                               mu_eta_ubound,
                               name_mu_eta,
                               name_alpha,
                               name_beta) {
  # B
  # latent variables on covariates
  if (mu_eta_fixed) {
    mu_eta <- .FitDTVARMxIDMuEtaFixed(
      k = k,
      mu_eta_values = mu_eta_values,
      name = name_mu_eta
    )
  } else {
    mu_eta_values <- tryCatch(
      {
        .MxHelperDTVARAlphaValues(
          p = k,
          val = mu_eta_values
        )
      },
      error = function(e) {
        stop("Error in `mu_eta_values`: ", e$message)
      },
      warning = function(w) {
        stop("Warning in `mu_eta_values`: ", w$message)
      }
    )
    mu_eta <- .MxHelperFullMxMatrix(
      m = k,
      n = 1,
      free_val = mu_eta_free,
      values = mu_eta_values,
      lbound_val = mu_eta_lbound,
      ubound_val = mu_eta_ubound,
      vec = TRUE,
      row = statenames,
      col = 1,
      name = name_mu_eta
    )
  }
  c(
    mu_eta,
    OpenMx::mxAlgebraFromString(
      algString = paste0(
        name_mu_eta,
        " - ",
        name_beta,
        " %*% ",
        name_mu_eta
      ),
      name = name_alpha
    ),
    OpenMx::mxAlgebraFromString(
      algString = paste0(
        name_mu_eta,
        " - ",
        name_beta,
        " %*% ",
        name_mu_eta
      ),
      name = "B"
    )
  )
}
