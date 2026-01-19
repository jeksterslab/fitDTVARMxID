.FitDTVARMxIDBeta <- function(k,
                              statenames,
                              beta_fixed,
                              beta_free,
                              beta_values,
                              beta_lbound,
                              beta_ubound,
                              name) {
  # A
  # auto regression and cross regression coefficients
  if (beta_fixed) {
    beta <- .FitDTVARMxBetaFixed(
      k = k,
      beta_values = beta_values,
      name = name
    )
  } else {
    beta_values <- tryCatch(
      {
        .MxHelperDTVARBetaValues(
          p = k,
          val = beta_values
        )
      },
      error = function(e) {
        # nocov start
        stop("Error in `beta_values`: ", e$message)
        # nocov end
      },
      warning = function(w) {
        # nocov start
        stop("Warning in `beta_values`: ", w$message)
        # nocov end
      }
    )
    if (is.null(beta_lbound)) {
      beta_lbound <- matrix(
        data = -2.5,
        nrow = k,
        ncol = k
      )
    }
    if (is.null(beta_ubound)) {
      beta_ubound <- matrix(
        data = +2.5,
        nrow = k,
        ncol = k
      )
    }
    beta <- .MxHelperFullMxMatrix(
      m = k,
      n = k,
      values = beta_values,
      free_val = beta_free,
      lbound_val = beta_lbound,
      ubound_val = beta_ubound,
      vec = TRUE,
      row = statenames,
      col = statenames,
      name = name
    )
  }
  c(
    beta,
    OpenMx::mxAlgebraFromString(
      algString = name,
      name = "A"
    )
  )
}
