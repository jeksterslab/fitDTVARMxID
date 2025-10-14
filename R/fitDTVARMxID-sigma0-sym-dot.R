.FitDTVARMxIDSigma0Sym <- function(k,
                                   statenames,
                                   sigma0_d_free,
                                   sigma0_d_values,
                                   sigma0_d_lbound,
                                   sigma0_d_ubound,
                                   sigma0_l_free,
                                   sigma0_l_values,
                                   sigma0_l_lbound,
                                   sigma0_l_ubound,
                                   name) {
  # P0
  # Initial condition covariance matrix
  sigma0_d <- paste0(
    name,
    "_d"
  )
  sigma0_l <- paste0(
    name,
    "_l"
  )
  sigma0_iden <- paste0(
    name,
    "_iden"
  )
  .MxHelperSigmaFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = sigma0_d,
    sdiag_name = sigma0_l,
    iden_name = sigma0_iden,
    d_free = sigma0_d_free,
    d_values = sigma0_d_values,
    d_lbound = sigma0_d_lbound,
    d_ubound = sigma0_d_ubound,
    d_rows = statenames,
    d_cols = sigma0_d,
    d_equal = FALSE,
    l_free = sigma0_l_free,
    l_values = sigma0_l_values,
    l_lbound = sigma0_l_lbound,
    l_ubound = sigma0_l_ubound,
    l_rows = statenames,
    l_cols = statenames
  )
}
