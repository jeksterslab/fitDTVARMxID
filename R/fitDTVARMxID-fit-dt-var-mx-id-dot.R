.FitDTVARMxID <- function(data,
                          observed,
                          id,
                          center,
                          mu_eta_fixed,
                          mu_eta_free,
                          mu_eta_values,
                          mu_eta_lbound,
                          mu_eta_ubound,
                          alpha_fixed,
                          alpha_free,
                          alpha_values,
                          alpha_lbound,
                          alpha_ubound,
                          beta_fixed,
                          beta_free,
                          beta_values,
                          beta_lbound,
                          beta_ubound,
                          psi_diag,
                          psi_d_free,
                          psi_d_values,
                          psi_d_lbound,
                          psi_d_ubound,
                          psi_l_free,
                          psi_l_values,
                          psi_l_lbound,
                          psi_l_ubound,
                          nu_fixed,
                          nu_free,
                          nu_values,
                          nu_lbound,
                          nu_ubound,
                          theta_diag,
                          theta_fixed,
                          theta_d_free,
                          theta_d_values,
                          theta_d_lbound,
                          theta_d_ubound,
                          theta_d_equal,
                          theta_l_free,
                          theta_l_values,
                          theta_l_lbound,
                          theta_l_ubound,
                          mu0_fixed,
                          mu0_func,
                          mu0_free,
                          mu0_values,
                          mu0_lbound,
                          mu0_ubound,
                          sigma0_fixed,
                          sigma0_func,
                          sigma0_diag,
                          sigma0_d_free,
                          sigma0_d_values,
                          sigma0_d_lbound,
                          sigma0_d_ubound,
                          sigma0_l_free,
                          sigma0_l_values,
                          sigma0_l_lbound,
                          sigma0_l_ubound,
                          tries_explore,
                          tries_local,
                          max_attempts,
                          grad_tol,
                          hess_tol,
                          eps,
                          factor,
                          overwrite,
                          path,
                          prefix,
                          seed,
                          silent,
                          ncores,
                          clean) {
  threads <- OpenMx::mxOption(
    key = "Number of Threads"
  )
  on.exit(
    OpenMx::mxOption(
      key = "Number of Threads",
      value = threads
    ),
    add = TRUE
  )
  model <- .FitDTVARMxIDBuildModelID(
    data = data,
    observed = observed,
    id = id,
    center = center,
    mu_eta_fixed = mu_eta_fixed,
    mu_eta_free = mu_eta_free,
    mu_eta_values = mu_eta_values,
    mu_eta_lbound = mu_eta_lbound,
    mu_eta_ubound = mu_eta_ubound,
    alpha_fixed = alpha_fixed,
    alpha_free = alpha_free,
    alpha_values = alpha_values,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound,
    beta_fixed = beta_fixed,
    beta_free = beta_free,
    beta_values = beta_values,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_diag = psi_diag,
    psi_d_free = psi_d_free,
    psi_d_values = psi_d_values,
    psi_d_lbound = psi_d_lbound,
    psi_d_ubound = psi_d_ubound,
    psi_l_free = psi_l_free,
    psi_l_values = psi_l_values,
    psi_l_lbound = psi_l_lbound,
    psi_l_ubound = psi_l_ubound,
    nu_fixed = nu_fixed,
    nu_free = nu_free,
    nu_values = nu_values,
    nu_lbound = nu_lbound,
    nu_ubound = nu_ubound,
    theta_diag = theta_diag,
    theta_fixed = theta_fixed,
    theta_d_free = theta_d_free,
    theta_d_values = theta_d_values,
    theta_d_lbound = theta_d_lbound,
    theta_d_ubound = theta_d_ubound,
    theta_d_equal = theta_d_equal,
    theta_l_free = theta_l_free,
    theta_l_values = theta_l_values,
    theta_l_lbound = theta_l_lbound,
    theta_l_ubound = theta_l_ubound,
    mu0_fixed = mu0_fixed,
    mu0_func = mu0_func,
    mu0_free = mu0_free,
    mu0_values = mu0_values,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_func = sigma0_func,
    sigma0_diag = sigma0_diag,
    sigma0_d_free = sigma0_d_free,
    sigma0_d_values = sigma0_d_values,
    sigma0_d_lbound = sigma0_d_lbound,
    sigma0_d_ubound = sigma0_d_ubound,
    sigma0_l_free = sigma0_l_free,
    sigma0_l_values = sigma0_l_values,
    sigma0_l_lbound = sigma0_l_lbound,
    sigma0_l_ubound = sigma0_l_ubound,
    overwrite = overwrite,
    path = path,
    prefix = prefix
  )
  if (is.null(ncores)) {
    par <- FALSE
  } else {
    ncores <- min(
      as.integer(ncores),
      parallel::detectCores(),
      length(model)
    )
    if (ncores > 1) {
      par <- TRUE
    } else {
      par <- FALSE
    }
  }
  if (par) {
    OpenMx::mxOption(
      key = "Number of Threads",
      value = 1
    )
    os_type <- Sys.info()["sysname"]
    if (os_type == "Darwin") {
      fork <- TRUE
    } else if (os_type == "Linux") {
      fork <- TRUE
    } else {
      fork <- FALSE
    }
    if (fork) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      check_hess <- c(
        FALSE,
        TRUE,
        TRUE
      )
      for (i in seq_len(2)) {
        try(
          fit <- parallel::mclapply(
            X = model,
            FUN = .MxHelperReadRunSave,
            tries_explore = tries_explore,
            tries_local = tries_local,
            max_attempts = max_attempts,
            grad_tol = grad_tol,
            hess_tol = hess_tol,
            eps = eps,
            silent = silent,
            check_hess = check_hess[i],
            mc.cores = ncores
          )
        )
      }
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterEvalQ(cl = cl, library(OpenMx))
      if (!is.null(seed)) {
        parallel::clusterSetRNGStream(
          cl = cl,
          iseed = seed
        )
      }
      on.exit(
        parallel::stopCluster(cl = cl),
        add = TRUE
      )
      check_hess <- c(
        FALSE,
        TRUE,
        TRUE
      )
      for (i in seq_len(2)) {
        try(
          fit <- parallel::parLapply(
            cl = cl,
            X = model,
            fun = .MxHelperReadRunSave,
            tries_explore = tries_explore,
            tries_local = tries_local,
            max_attempts = max_attempts,
            grad_tol = grad_tol,
            hess_tol = hess_tol,
            eps = eps,
            silent = silent,
            check_hess = check_hess[i]
          )
        )
      }
    }
  } else {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    check_hess <- c(
      FALSE,
      TRUE,
      TRUE
    )
    for (i in seq_len(2)) {
      try(
        fit <- lapply(
          X = model,
          FUN = .MxHelperReadRunSave,
          tries_explore = tries_explore,
          tries_local = tries_local,
          max_attempts = max_attempts,
          grad_tol = grad_tol,
          hess_tol = hess_tol,
          eps = eps,
          silent = silent,
          check_hess = check_hess[i]
        )
      )
    }
  }
  names(fit) <- basename(model)
  if (clean) {
    unlink(model)
  }
  fit
}
