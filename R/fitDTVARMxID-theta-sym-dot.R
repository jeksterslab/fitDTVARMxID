.FitDTVARMxIDThetaSym <- function(k,
                                  observed,
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
  theta_d <- paste0(
    name,
    "_d"
  )
  theta_l <- paste0(
    name,
    "_l"
  )
  theta_iden <- paste0(
    name,
    "_iden"
  )
  .MxHelperSigmaFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = theta_d,
    sdiag_name = theta_l,
    iden_name = theta_iden,
    d_free = theta_d_free,
    d_values = theta_d_values,
    d_lbound = theta_d_lbound,
    d_ubound = theta_d_ubound,
    d_rows = observed,
    d_cols = theta_d,
    d_equal = theta_d_equal,
    l_free = theta_l_free,
    l_values = theta_l_values,
    l_lbound = theta_l_lbound,
    l_ubound = theta_l_ubound,
    l_rows = observed,
    l_cols = observed
  )
}
