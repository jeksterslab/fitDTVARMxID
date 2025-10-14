.FitDTVARMxIDSigma0FixedSym <- function(k,
                                        statenames,
                                        sigma0_d_values,
                                        sigma0_l_values,
                                        name) {
  # P0
  # Initial condition covariance matrix
  sigma0_iden <- paste0(
    name,
    "_iden"
  )
  sigma0_l <- paste0(
    name,
    "_l"
  )
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
  .MxHelperSigmaFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = sigma0_d,
    sdiag_name = sigma0_l,
    iden_name = sigma0_iden,
    d_free = rep(x = FALSE, times = k),
    d_values = sigma0_d_values,
    d_lbound = rep(x = NA, times = k),
    d_ubound = rep(x = NA, times = k),
    d_rows = statenames,
    d_cols = sigma0_d,
    d_equal = FALSE,
    l_free = matrix(data = FALSE, nrow = k, ncol = k),
    l_values = matrix(data = 0, nrow = k, ncol = k),
    l_lbound = rep(x = NA, times = k),
    l_ubound = rep(x = NA, times = k),
    l_rows = statenames,
    l_cols = statenames
  )
}
