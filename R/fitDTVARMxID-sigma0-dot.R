.FitDTVARMxIDSigma0 <- function(k,
                                statenames,
                                sigma0_fixed,
                                sigma0_func,
                                sigma0_diag,
                                sigma0_d_free,
                                sigma0_d_values,
                                sigma0_d_lbound,
                                sigma0_d_ubound,
                                sigma0_l_free,
                                sigma0_l_values,
                                sigma0_l_lbound,
                                sigma0_l_ubound,
                                name,
                                name_beta,
                                name_psi) {
  # P0
  # Initial condition covariance matrix
  if (sigma0_fixed) {
    if (sigma0_func) {
      sigma0 <- .FitDTVARMxIDSigma0Func(
        k = k,
        statenames = statenames,
        name = name,
        name_beta = name_beta,
        name_psi = name_psi
      )
    } else {
      sigma0 <- .FitDTVARMxIDSigma0Fixed(
        k = k,
        statenames = statenames,
        sigma0_diag = sigma0_diag,
        sigma0_d_values = sigma0_d_values,
        sigma0_l_values = sigma0_l_values,
        name = name
      )
    }
  } else {
    if (sigma0_diag) {
      sigma0 <- .FitDTVARMxIDSigma0Diag(
        k = k,
        statenames = statenames,
        sigma0_d_free = sigma0_d_free,
        sigma0_d_values = sigma0_d_values,
        sigma0_d_lbound = sigma0_d_lbound,
        sigma0_d_ubound = sigma0_d_ubound,
        name = name
      )
    } else {
      sigma0 <- .FitDTVARMxIDSigma0Sym(
        k = k,
        statenames = statenames,
        sigma0_d_free = sigma0_d_free,
        sigma0_d_values = sigma0_d_values,
        sigma0_d_lbound = sigma0_d_lbound,
        sigma0_d_ubound = sigma0_d_ubound,
        sigma0_l_free = sigma0_l_free,
        sigma0_l_values = sigma0_l_values,
        sigma0_l_lbound = sigma0_l_lbound,
        sigma0_l_ubound = sigma0_l_ubound,
        name = name
      )
    }
  }
  c(
    sigma0,
    OpenMx::mxAlgebraFromString(
      algString = name,
      name = "P0"
    )
  )
}
