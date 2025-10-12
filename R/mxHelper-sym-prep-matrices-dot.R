.MxHelperSymPrepMatrices <- function(p,
                                     free_val,
                                     values,
                                     lbound_val,
                                     ubound_val,
                                     vec,
                                     name) {
  values <- .MxHelperSymValues(
    p = p,
    val = values
  )
  free <- .MxHelperSymFree(
    p = p,
    val = free_val
  )
  labels <- .MxHelperSymLabels(
    p = p,
    name = name,
    sep = "underscore"
  )
  if (vec) {
    vec <- .MxHelperSymLabels(
      p = p,
      name = name,
      sep = "bracket"
    )
    vec_run <- TRUE
  } else {
    vec_run <- FALSE
  }
  lbound <- .MxHelperSymBound(
    p = p,
    val = lbound_val
  )
  ubound <- .MxHelperSymBound(
    p = p,
    val = ubound_val
  )
  for (j in seq_len(p)) {
    for (i in seq_len(p)) {
      if (!free[i, j]) {
        labels[i, j] <- NA
        lbound[i, j] <- NA
        ubound[i, j] <- NA
        if (vec_run) {
          vec[i, j] <- NA
        }
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
