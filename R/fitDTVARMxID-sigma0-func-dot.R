.FitDTVARMxIDSigma0Func <- function(k,
                                    statenames,
                                    name,
                                    name_beta,
                                    name_psi) {
  # P0
  # Initial condition covariance matrix
  # sigma0_values will be implied by the algebra
  sigma0_iden <- paste0(
    name,
    "_iden"
  )
  sigma0_vector <- paste0(
    name,
    "_vector"
  )
  sigma0_mat <- paste0(
    name,
    "_mat"
  )
  sigma0_vec <- paste0(
    name,
    "_vec"
  )
  beta <- name_beta
  psi <- name_psi
  list(
    sigma0_iden = OpenMx::mxMatrix(
      type = "Iden",
      nrow = k * k,
      ncol = k * k,
      name = sigma0_iden
    ),
    sigma0_vector = OpenMx::mxAlgebraFromString(
      algString = paste0(
        "solve(",
        sigma0_iden,
        " - ",
        beta,
        " %x% ",
        beta,
        ") %*% cvectorize(",
        psi,
        ")"
      ),
      name = sigma0_vector
    ),
    sigma0_mat = OpenMx::mxMatrix(
      "Full",
      nrow = k,
      ncol = k,
      labels = paste0(
        sigma0_vector,
        "[",
        1:(k * k),
        ",",
        1,
        "]"
      ),
      dimnames = list(
        statenames,
        statenames
      ),
      name = sigma0_mat
    ),
    sigma0 = OpenMx::mxAlgebraFromString(
      algString = paste0(
        "0.5 * (",
        sigma0_mat,
        " + t(",
        sigma0_mat,
        "))"
      ),
      dimnames = list(
        statenames,
        statenames
      ),
      name = "sigma0"
    ),
    sigma0_vec = OpenMx::mxAlgebraFromString(
      algString = paste0(
        "vech(",
        name,
        ")"
      ),
      name = sigma0_vec
    )
  )
}
