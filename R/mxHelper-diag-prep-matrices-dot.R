.MxHelperDiagPrepMatrices <- function(p,
                                      free_val,
                                      values,
                                      lbound_val,
                                      ubound_val,
                                      vec,
                                      name) {
  values <- .MxHelperDiagValues(
    p = p,
    val = values
  )
  free <- .MxHelperDiagFree(
    p = p,
    val = free_val
  )
  labels <- .MxHelperDiagLabels(
    p = p,
    name = name,
    sep = "underscore"
  )
  if (vec) {
    vec <- .MxHelperDiagLabels(
      p = p,
      name = name,
      sep = "bracket"
    )
    vec_run <- TRUE
  } else {
    vec_run <- FALSE
  }
  lbound <- .MxHelperDiagBound(
    p = p,
    val = lbound_val
  )
  ubound <- .MxHelperDiagBound(
    p = p,
    val = ubound_val
  )
  for (i in seq_len(p)) {
    if (!free[i]) {
      labels[i] <- NA
      lbound[i] <- NA
      ubound[i] <- NA
      if (vec_run) {
        vec[i] <- NA
      }
    }
  }
  list(
    free = free,
    values = values,
    labels = labels,
    lbound = lbound,
    ubound = ubound,
    vec = vec
  )
}
