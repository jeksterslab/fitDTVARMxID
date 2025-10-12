.FitDTVARMxIDThetaDiag <- function(k,
                                   observed,
                                   theta_d_free,
                                   theta_d_values,
                                   theta_d_lbound,
                                   theta_d_ubound,
                                   theta_d_equal,
                                   name) {
  # R
  # measurement error
  theta_d <- paste0(name, "_d")
  .MxHelperSigmaDiagFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = theta_d,
    d_free = theta_d_free,
    d_values = theta_d_values,
    d_lbound = theta_d_lbound,
    d_ubound = theta_d_ubound,
    d_rows = observed,
    d_cols = theta_d,
    d_equal = theta_d_equal
  )
}
