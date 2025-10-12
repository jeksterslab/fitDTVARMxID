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
      A = "beta",
      B = "alpha",
      C = "lambda",
      D = "nu",
      Q = "psi",
      R = "theta",
      x0 = "mu0",
      P0 = "sigma0",
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
