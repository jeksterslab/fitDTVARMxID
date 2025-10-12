.FitDTVARMxIDSigma0Fixed <- function(k,
                                     statenames,
                                     sigma0_diag,
                                     sigma0_d_values,
                                     sigma0_l_values,
                                     name) {
  # P0
  # Initial condition covariance matrix
  if (sigma0_diag) {
    out <- .FitDTVARMxIDSigma0FixedDiag(
      k = k,
      statenames = statenames,
      sigma0_d_values = sigma0_d_values,
      name = name
    )
  } else {
    out <- .FitDTVARMxIDSigma0FixedSym(
      k = k,
      statenames = statenames,
      sigma0_d_values = sigma0_d_values,
      sigma0_l_values = sigma0_l_values,
      name = name
    )
  }
  out
}
