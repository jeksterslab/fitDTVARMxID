.FitDTVARMxIDPsiDiag <- function(k,
                                 statenames,
                                 psi_d_free,
                                 psi_d_values,
                                 psi_d_lbound,
                                 psi_d_ubound,
                                 name) {
  # Q
  # process noise
  psi_d <- paste0(
    name,
    "_d"
  )
  .MxHelperSigmaDiagFromLDLMxMatrix(
    p = k,
    name = name,
    column_name = psi_d,
    d_free = psi_d_free,
    d_values = psi_d_values,
    d_lbound = psi_d_lbound,
    d_ubound = psi_d_ubound,
    d_rows = statenames,
    d_cols = psi_d,
    d_equal = FALSE
  )
}
