.FitDTVARMxIDAlpha <- function(k,
                               statenames,
                               center,
                               alpha_fixed,
                               alpha_free,
                               alpha_values,
                               alpha_lbound,
                               alpha_ubound,
                               name) {
  # B
  # latent variables on covariates
  if (alpha_fixed) {
    alpha <- .FitDTVARMxIDAlphaFixed(
      k = k,
      alpha_values = alpha_values,
      name = name
    )
  } else {
    alpha_values <- tryCatch(
      {
        .MxHelperDTVARAlphaValues(
          p = k,
          val = alpha_values
        )
      },
      error = function(e) {
        # nocov start
        stop("Error in `alpha_values`: ", e$message)
        # nocov end
      },
      warning = function(w) {
        # nocov start
        stop("Warning in `alpha_values`: ", w$message)
        # nocov end
      }
    )
    alpha <- .MxHelperFullMxMatrix(
      m = k,
      n = 1,
      free_val = alpha_free,
      values = alpha_values,
      lbound_val = alpha_lbound,
      ubound_val = alpha_ubound,
      vec = TRUE,
      row = statenames,
      col = 1,
      name = name
    )
  }
  c(
    alpha,
    OpenMx::mxAlgebraFromString(
      algString = name,
      name = "B"
    )
  )
}
