.FitDTVARMxIDX <- function() {
  # u
  # covariates
  OpenMx::mxMatrix(
    type = "Unit",
    nrow = 1,
    ncol = 1,
    name = "covariate"
  )
}
