.FitDTVARMxIDModelID <- function(id,
                                 data,
                                 observed,
                                 beta,
                                 alpha,
                                 lambda,
                                 nu,
                                 psi,
                                 theta,
                                 mu0,
                                 sigma0,
                                 covariate,
                                 mu,
                                 sigma) {
  OpenMx::mxModel(
    model = paste0(
      "DTVAR",
      "_",
      "ID",
      id
    ),
    beta,
    alpha,
    lambda,
    nu,
    psi,
    theta,
    mu0,
    sigma0,
    covariate,
    mu,
    sigma,
    OpenMx::mxExpectationStateSpace(
      A = "A",
      B = "B",
      C = "C",
      D = "D",
      Q = "Q",
      R = "R",
      x0 = "x0",
      P0 = "P0",
      u = "covariate",
      dimnames = observed
    ),
    OpenMx::mxFitFunctionML(),
    OpenMx::mxData(
      observed = data,
      type = "raw"
    )
  )
}
