.MxHelperSigmaFromLDLMxMatrix <- function(p,
                                          name,
                                          column_name,
                                          sdiag_name,
                                          iden_name,
                                          d_free,
                                          d_values,
                                          d_lbound,
                                          d_ubound,
                                          d_rows,
                                          d_cols,
                                          l_free,
                                          l_values,
                                          l_lbound,
                                          l_ubound,
                                          l_rows,
                                          l_cols) {
  if (is.null(d_values)) {
    d_values <- rep(
      x = log(expm1(1)),
      times = p
    )
  } else {
    if (is.matrix(d_values)) {
      if (all(dim(d_values) == c(p, p))) {
        d_values <- diag(d_values)
      }
      if (all(dim(d_values) == c(p, 1))) {
        d_values <- c(d_values)
      }
    }
    stopifnot(
      is.vector(d_values),
      length(d_values) == p
    )
  }
  out <- list()
  sdiag <- .MxHelperSdiagMxMatrix(
    p = p,
    free_val = l_free,
    values = l_values,
    lbound_val = l_lbound,
    ubound_val = l_ubound,
    vec = TRUE,
    row = l_rows,
    col = l_cols,
    name = sdiag_name
  )
  if (length(sdiag) == 1) {
    names(sdiag) <- sdiag_name
  } else {
    names(sdiag) <- c(
      sdiag_name,
      paste0(
        sdiag_name,
        "_",
        "vec"
      )
    )
  }
  out <- c(
    out,
    sdiag
  )
  column <- .MxHelperFullMxMatrix(
    m = p,
    n = 1,
    free_val = d_free,
    values = d_values,
    lbound_val = d_lbound,
    ubound_val = d_ubound,
    vec = TRUE,
    row = d_rows,
    col = d_cols,
    name = column_name
  )
  if (length(column) == 1) {
    names(column) <- column_name
  } else {
    names(column) <- c(
      column_name,
      paste0(
        column_name,
        "_",
        "vec"
      )
    )
  }
  out <- c(
    out,
    column
  )
  iden <- OpenMx::mxMatrix(
    type = "Iden",
    nrow = p,
    ncol = p,
    name = iden_name
  )
  out <- c(
    out,
    stats::setNames(
      object = list(iden),
      nm = iden_name
    )
  )
  sigma <- .MxHelperSigmaFromLDLSoftplusAlgebra(
    sdiag = sdiag_name,
    column = column_name,
    iden = iden_name,
    name = name
  )
  out <- c(
    out,
    stats::setNames(
      object = list(sigma),
      nm = name
    )
  )
  sigma_vech <- OpenMx::mxAlgebraFromString(
    algString = paste0(
      "vech(",
      name,
      ")"
    ),
    name = paste0(
      name,
      "_",
      "vech"
    )
  )
  out <- c(
    out,
    stats::setNames(
      object = list(sigma_vech),
      nm = paste0(
        name,
        "_",
        "vech"
      )
    )
  )
  free <- sdiag[[1]]@free
  diag(free) <- c(column[[1]]@free)
  vec <- matrix(
    data = NA,
    nrow = p,
    ncol = p
  )
  for (j in seq_len(p)) {
    for (i in seq_len(p)) {
      if (free[i, j]) {
        vec[i, j] <- paste0(
          name,
          "[",
          i,
          ",",
          j,
          "]"
        )
      }
    }
  }
  vec <- unique(
    c(
      stats::na.omit(
        c(
          vec
        )
      )
    )
  )
  p <- length(vec)
  if (p > 0) {
    vec_free <- OpenMx::mxMatrix(
      type = "Full",
      nrow = p,
      ncol = 1,
      labels = vec,
      dimnames = list(
        vec,
        paste0(
          name,
          "_vec"
        )
      ),
      name = paste0(
        name,
        "_vec"
      )
    )
    out <- c(
      out,
      stats::setNames(
        object = list(vec_free),
        nm = paste0(
          name,
          "_vec"
        )
      )
    )
  }
  out
}
