.FitDTVARMxIDMu0 <- function(k,
                             statenames,
                             mu0_fixed,
                             mu0_func,
                             mu0_free,
                             mu0_values,
                             mu0_lbound,
                             mu0_ubound,
                             name,
                             name_beta,
                             name_alpha,
                             center) {
  # x0
  # initial condition
  # mean
  if (mu0_fixed) {
    if (mu0_func) {
      mu0 <- .FitDTVARMxIDMu0Func(
        k = k,
        statenames = statenames,
        name = name,
        name_beta = name_beta,
        name_alpha = name_alpha,
        center = center
      )
    } else {
      mu0 <- .FitDTVARMxIDMu0Fixed(
        k = k,
        statenames = statenames,
        mu0_values = mu0_values,
        name = name
      )
    }
  } else {
    mu0 <- .FitDTVARMxIDMu0Vec(
      k = k,
      statenames = statenames,
      mu0_free = mu0_free,
      mu0_values = mu0_values,
      mu0_lbound = mu0_lbound,
      mu0_ubound = mu0_ubound,
      name = name
    )
  }
  c(
    mu0,
    OpenMx::mxAlgebraFromString(
      algString = name,
      name = "x0"
    )
  )
}
