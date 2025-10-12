.FitDTVARMxIDNuFixed <- function(k,
                                 nu_values,
                                 name) {
  # D
  # observed variables on covariates
  if (is.null(nu_values)) {
    out <- OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = 1,
      name = name
    )
  } else {
    out <- OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = FALSE,
      values = nu_values,
      byrow = FALSE,
      name = name
    )
  }
  out
}
