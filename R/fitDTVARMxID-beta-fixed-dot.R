.FitDTVARMxBetaFixed <- function(k,
                                 beta_values,
                                 name) {
  # A
  # auto regression and cross regression coefficients
  if (is.null(beta_values)) {
    out <- OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = k,
      name = name
    )
  } else {
    out <- OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = beta_values,
      byrow = FALSE,
      name = name
    )
  }
  out
}
