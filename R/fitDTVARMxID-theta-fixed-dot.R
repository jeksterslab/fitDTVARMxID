.FitDTVARMxIDThetaFixed <- function(k,
                                    observed,
                                    theta_d_values,
                                    name) {
  # R
  # measurement error
  if (is.null(theta_d_values)) {
    out <- OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = k,
      dimnames = list(
        observed,
        observed
      ),
      name = name
    )
  } else {
    out <- OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = Softplus(theta_d_values),
      byrow = FALSE,
      dimnames = list(
        observed,
        observed
      ),
      name = name
    )
  }
  out
}
