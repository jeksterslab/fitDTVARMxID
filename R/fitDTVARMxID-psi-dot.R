.FitDTVARMxIDPsi <- function(k,
                             statenames,
                             psi_diag,
                             psi_d_free,
                             psi_d_values,
                             psi_d_lbound,
                             psi_d_ubound,
                             psi_l_free,
                             psi_l_values,
                             psi_l_lbound,
                             psi_l_ubound,
                             name) {
  # Q
  # process noise
  if (psi_diag) {
    out <- .FitDTVARMxIDPsiDiag(
      k = k,
      statenames = statenames,
      psi_d_free = psi_d_free,
      psi_d_values = psi_d_values,
      psi_d_lbound = psi_d_lbound,
      psi_d_ubound = psi_d_ubound,
      name = name
    )
  } else {
    out <- .FitDTVARMxIDPsiSym(
      k = k,
      statenames = statenames,
      psi_d_free = psi_d_free,
      psi_d_values = psi_d_values,
      psi_d_lbound = psi_d_lbound,
      psi_d_ubound = psi_d_ubound,
      psi_l_free = psi_l_free,
      psi_l_values = psi_l_values,
      psi_l_lbound = psi_l_lbound,
      psi_l_ubound = psi_l_ubound,
      name = name
    )
  }
  out
}
