.FitDTVARMxIDLambda <- function(k,
                                observed,
                                statenames) {
  # C
  # measurement model factor loadings
  c(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = 1,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        observed,
        statenames
      ),
      name = "lambda"
    ),
    OpenMx::mxAlgebraFromString(
      algString = "lambda",
      dimnames = list(
        observed,
        statenames
      ),
      name = "C"
    )
  )
}
