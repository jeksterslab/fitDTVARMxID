.FitDTVARMxIDNu <- function(k,
                            observed,
                            nu_fixed,
                            nu_free,
                            nu_values,
                            nu_lbound,
                            nu_ubound,
                            name) {
  # D
  # observed variables on covariates
  if (nu_fixed) {
    out <- .FitDTVARMxIDNuFixed(
      k = k,
      nu_values = nu_values,
      name = name
    )
  } else {
    nu_values <- tryCatch(
      {
        .MxHelperDTVARAlphaValues(
          p = k,
          val = nu_values
        )
      },
      error = function(e) {
        stop("Error in `nu_values`: ", e$message)
      },
      warning = function(w) {
        stop("Warning in `nu_values`: ", w$message)
      }
    )
    out <- .MxHelperFullMxMatrix(
      m = k,
      n = 1,
      free_val = nu_free,
      values = nu_values,
      lbound_val = nu_lbound,
      ubound_val = nu_ubound,
      vec = TRUE,
      row = observed,
      col = 1,
      name = name
    )
  }
  out
}
