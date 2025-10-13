.FitDTVARMxIDSigma0FixedDiag <- function(k,
                                         statenames,
                                         sigma0_d_values,
                                         name) {
  # P0
  # Initial condition covariance matrix
  sigma0_d <- paste0(
    name,
    "_d"
  )
  if (is.null(sigma0_d_values)) {
    sigma0_d_values <- rep(
      x = log(expm1(1)),
      times = k
    )
  }
  .MxHelperSigmaDiagFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = sigma0_d,
    d_free = rep(x = FALSE, times = k),
    d_values = sigma0_d_values,
    d_lbound = rep(x = NA, times = k),
    d_ubound = rep(x = NA, times = k),
    d_rows = statenames,
    d_cols = sigma0_d,
    d_equal = FALSE
  )
}
