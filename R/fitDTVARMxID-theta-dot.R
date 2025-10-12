.FitDTVARMxIDTheta <- function(k,
                               observed,
                               theta_fixed,
                               theta_d_free,
                               theta_d_values,
                               theta_d_lbound,
                               theta_d_ubound,
                               theta_d_equal,
                               name) {
  # R
  # measurement error
  if (theta_fixed) {
    out <- .FitDTVARMxIDThetaFixed(
      k = k,
      observed = observed,
      theta_d_values = theta_d_values,
      name = name
    )
  } else {
    out <- .FitDTVARMxIDThetaDiag(
      k = k,
      observed = observed,
      theta_d_free = theta_d_free,
      theta_d_values = theta_d_values,
      theta_d_lbound = theta_d_lbound,
      theta_d_ubound = theta_d_ubound,
      theta_d_equal = theta_d_equal,
      name = name
    )
  }
  out
}
