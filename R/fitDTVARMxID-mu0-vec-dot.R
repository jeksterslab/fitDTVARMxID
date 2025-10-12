.FitDTVARMxIDMu0Vec <- function(k,
                                statenames,
                                mu0_free,
                                mu0_values,
                                mu0_lbound,
                                mu0_ubound,
                                name) {
  # x0
  # initial condition
  # mean
  mu0_values <- tryCatch(
    {
      .MxHelperDTVARAlphaValues(
        p = k,
        val = mu0_values
      )
    },
    error = function(e) {
      stop("Error in `mu0_values`: ", e$message)
    },
    warning = function(w) {
      stop("Warning in `mu0_values`: ", w$message)
    }
  )
  .MxHelperFullMxMatrix(
    m = k,
    n = 1,
    free_val = mu0_free,
    values = mu0_values,
    lbound_val = mu0_lbound,
    ubound_val = mu0_ubound,
    vec = TRUE,
    row = statenames,
    col = 1,
    name = name
  )
}
