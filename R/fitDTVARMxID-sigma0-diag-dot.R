.FitDTVARMxIDSigma0Diag <- function(k,
                                    statenames,
                                    sigma0_d_free,
                                    sigma0_d_values,
                                    sigma0_d_lbound,
                                    sigma0_d_ubound,
                                    name) {
  # P0
  # Initial condition covariance matrix
  sigma0_d <- paste0(
    name,
    "_d"
  )
  .MxHelperSigmaDiagFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = sigma0_d,
    d_free = sigma0_d_free,
    d_values = sigma0_d_values,
    d_lbound = sigma0_d_lbound,
    d_ubound = sigma0_d_ubound,
    d_rows = statenames,
    d_cols = sigma0_d
  )
}
