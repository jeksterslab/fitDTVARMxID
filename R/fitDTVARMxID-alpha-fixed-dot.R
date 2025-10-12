.FitDTVARMxIDAlphaFixed <- function(k,
                                    alpha_values,
                                    name) {
  # B
  # latent variables on covariates
  if (is.null(alpha_values)) {
    out <- OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = 1,
      name = name
    )
  } else {
    out <- OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = FALSE,
      values = alpha_values,
      byrow = FALSE,
      name = name
    )
  }
  out
}
