.FitDTVARMxIDAlpha <- function(k,
                               statenames,
                               alpha_fixed,
                               alpha_free,
                               alpha_values,
                               alpha_lbound,
                               alpha_ubound,
                               name_alpha,
                               name_beta,
                               center) {
  # B
  # latent variables on covariates
  if (alpha_fixed) {
    alpha <- .FitDTVARMxIDAlphaFixed(
      k = k,
      alpha_values = alpha_values,
      name = name_alpha
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
        stop("Error in `alpha_values`: ", e$message)
      },
      warning = function(w) {
        stop("Warning in `alpha_values`: ", w$message)
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
      name = name_alpha
    )
  }
  if (center) {
    out <- c(
      alpha,
      OpenMx::mxAlgebraFromString(
        algString = paste0(
          name_alpha,
          " - ",
          name_beta,
          " %*% ",
          name_alpha
        ),
        name = "B"
      )
    )
  } else {
    out <- c(
      alpha,
      OpenMx::mxAlgebraFromString(
        algString = name_alpha,
        name = "B"
      )
    )
  }
  out
}
