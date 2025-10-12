.FitDTVARMxIDSigmaFunc <- function(k,
                                   statenames,
                                   name,
                                   name_beta,
                                   name_psi) {
  # sigma_values will be implied by the algebra
  # stable covariance
  sigma_iden <- paste0(
    name,
    "_iden"
  )
  sigma_vector <- paste0(
    name,
    "_vector"
  )
  sigma_mat <- paste0(
    name,
    "_mat"
  )
  sigma_vec <- paste0(
    name,
    "_vec"
  )
  beta <- name_beta
  psi <- name_psi
  list(
    sigma_iden = OpenMx::mxMatrix(
      type = "Iden",
      nrow = k * k,
      ncol = k * k,
      name = sigma_iden
    ),
    sigma_vector = OpenMx::mxAlgebraFromString(
      algString = paste0(
        "solve(",
        sigma_iden,
        " - (",
        beta,
        " %x% ",
        beta,
        ")) %*% cvectorize(",
        psi,
        ")"
      ),
      name = sigma_vector
    ),
    sigma_mat = OpenMx::mxMatrix(
      "Full",
      nrow = k,
      ncol = k,
      labels = paste0(
        sigma_vector,
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
      name = sigma_mat
    ),
    sigma = OpenMx::mxAlgebraFromString(
      algString = paste0(
        "0.5 * (",
        sigma_mat,
        " + t(",
        sigma_mat,
        "))"
      ),
      dimnames = list(
        statenames,
        statenames
      ),
      name = name
    ),
    sigma_vec = OpenMx::mxAlgebraFromString(
      algString = paste0(
        "vech(",
        name,
        ")"
      ),
      name = sigma_vec
    )
  )
}
