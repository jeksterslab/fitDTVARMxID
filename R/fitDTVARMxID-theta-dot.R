.FitDTVARMxIDTheta <- function(k,
                               observed,
                               theta_diag,
                               theta_fixed,
                               theta_d_free,
                               theta_d_values,
                               theta_d_lbound,
                               theta_d_ubound,
                               theta_d_equal,
                               theta_l_free,
                               theta_l_values,
                               theta_l_lbound,
                               theta_l_ubound,
                               name) {
  # R
  # measurement error
  if (theta_fixed) {
    theta <- .FitDTVARMxIDThetaFixed(
      k = k,
      observed = observed,
      theta_diag = theta_diag,
      theta_d_values = theta_d_values,
      theta_l_values = theta_l_values,
      name = name
    )
  } else {
    if (theta_diag) {
      theta <- .FitDTVARMxIDThetaDiag(
        k = k,
        observed = observed,
        theta_d_free = theta_d_free,
        theta_d_values = theta_d_values,
        theta_d_lbound = theta_d_lbound,
        theta_d_ubound = theta_d_ubound,
        theta_d_equal = theta_d_equal,
        name = name
      )
    } else {
      theta <- .FitDTVARMxIDThetaSym(
        k = k,
        observed = observed,
        theta_d_free = theta_d_free,
        theta_d_values = theta_d_values,
        theta_d_lbound = theta_d_lbound,
        theta_d_ubound = theta_d_ubound,
        theta_d_equal = theta_d_equal,
        theta_l_free = theta_l_free,
        theta_l_values = theta_l_values,
        theta_l_lbound = theta_l_lbound,
        theta_l_ubound = theta_l_ubound,
        name = name
      )
    }
  }
  c(
    theta,
    OpenMx::mxAlgebraFromString(
      algString = name,
      name = "R"
    )
  )
}
