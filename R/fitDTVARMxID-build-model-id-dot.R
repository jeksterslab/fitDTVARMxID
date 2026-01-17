.FitDTVARMxIDBuildModelID <- function(data,
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
                                      overwrite,
                                      path,
                                      prefix) {
  ids <- sort(
    unique(data[, id])
  )
  n <- length(ids)
  k <- length(observed)
  statenames <- paste0(
    "eta",
    seq_len(k)
  )
  data <- lapply(
    X = ids,
    FUN = function(i) {
      data[which(data[, id] == i), , drop = FALSE]
    }
  )
  if (!is.list(beta_values)) {
    beta_values <- list(beta_values)
  }
  beta <- lapply(
    X = rep(x = beta_values, length.out = n),
    FUN = function(beta_values) {
      .FitDTVARMxIDBeta(
        k = k,
        statenames = statenames,
        beta_fixed = beta_fixed,
        beta_free = beta_free,
        beta_values = beta_values,
        beta_lbound = beta_lbound,
        beta_ubound = beta_ubound,
        name = "beta"
      )
    }
  )
  if (center) {
    if (!mu_eta_fixed && !nu_fixed) {
      stop(
        "`mu_eta` and `nu` cannot be modeled at the same time at the moment."
      )
    }
    if (!is.list(mu_eta_values)) {
      mu_eta_values <- list(mu_eta_values)
    }
    alpha <- lapply(
      X = rep(x = mu_eta_values, length.out = n),
      FUN = function(mu_eta_values) {
        .FitDTVARMxIDMuEta(
          k = k,
          statenames = statenames,
          mu_eta_fixed = mu_eta_fixed,
          mu_eta_free = mu_eta_free,
          mu_eta_values = mu_eta_values,
          mu_eta_lbound = mu_eta_lbound,
          mu_eta_ubound = mu_eta_ubound,
          name_mu_eta = "mu_eta",
          name_alpha = "alpha",
          name_beta = "beta"
        )
      }
    )
  } else {
    if (!alpha_fixed && !nu_fixed) {
      stop(
        "`alpha` and `nu` cannot be modeled at the same time at the moment."
      )
    }
    if (!is.list(alpha_values)) {
      alpha_values <- list(alpha_values)
    }
    alpha <- lapply(
      X = rep(x = alpha_values, length.out = n),
      FUN = function(alpha_values) {
        .FitDTVARMxIDAlpha(
          k = k,
          statenames = statenames,
          alpha_fixed = alpha_fixed,
          alpha_free = alpha_free,
          alpha_values = alpha_values,
          alpha_lbound = alpha_lbound,
          alpha_ubound = alpha_ubound,
          name = "alpha"
        )
      }
    )
  }
  lambda <- lapply(
    X = seq_len(n),
    FUN = function(i) {
      .FitDTVARMxIDLambda(
        k = k,
        observed = observed,
        statenames = statenames
      )
    }
  )
  if (!is.list(nu_values)) {
    nu_values <- list(nu_values)
  }
  nu <- lapply(
    X = rep(x = nu_values, length.out = n),
    FUN = function(nu_values) {
      .FitDTVARMxIDNu(
        k = k,
        observed = observed,
        nu_fixed = nu_fixed,
        nu_free = nu_free,
        nu_values = nu_values,
        nu_lbound = nu_lbound,
        nu_ubound = nu_ubound,
        name = "nu"
      )
    }
  )
  if (!is.list(psi_d_values)) {
    psi_d_values <- list(psi_d_values)
  }
  if (!is.list(psi_l_values)) {
    psi_l_values <- list(psi_l_values)
  }
  psi <- mapply(
    psi_d_values = rep(x = psi_d_values, length.out = n),
    psi_l_values = rep(x = psi_l_values, length.out = n),
    FUN = function(psi_d_values,
                   psi_l_values) {
      .FitDTVARMxIDPsi(
        k = k,
        statenames = statenames,
        psi_diag = psi_diag,
        psi_d_free = psi_d_free,
        psi_d_values = psi_d_values,
        psi_d_lbound = psi_d_lbound,
        psi_d_ubound = psi_d_ubound,
        psi_l_free = psi_l_free,
        psi_l_values = psi_l_values,
        psi_l_lbound = psi_l_lbound,
        psi_l_ubound = psi_l_ubound,
        name = "psi"
      )
    },
    SIMPLIFY = FALSE
  )
  if (!is.list(theta_d_values)) {
    theta_d_values <- list(theta_d_values)
  }
  theta <- lapply(
    X = rep(x = theta_d_values, length.out = n),
    FUN = function(theta_d_values) {
      .FitDTVARMxIDTheta(
        k = k,
        observed = observed,
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
        name = "theta"
      )
    }
  )
  if (!is.list(mu0_values)) {
    mu0_values <- list(mu0_values)
  }
  mu0 <- lapply(
    X = rep(x = mu0_values, length.out = n),
    FUN = function(mu0_values) {
      .FitDTVARMxIDMu0(
        k = k,
        statenames = statenames,
        mu0_fixed = mu0_fixed,
        mu0_func = mu0_func,
        mu0_free = mu0_free,
        mu0_values = mu0_values,
        mu0_lbound = mu0_lbound,
        mu0_ubound = mu0_ubound,
        name = "mu0",
        name_beta = "beta",
        name_alpha = "alpha",
        center = center
      )
    }
  )
  if (!is.list(sigma0_d_values)) {
    sigma0_d_values <- list(sigma0_d_values)
  }
  if (!is.list(sigma0_l_values)) {
    sigma0_l_values <- list(sigma0_l_values)
  }
  sigma0 <- mapply(
    sigma0_d_values = rep(x = sigma0_d_values, length.out = n),
    sigma0_l_values = rep(x = sigma0_l_values, length.out = n),
    FUN = function(sigma0_d_values,
                   sigma0_l_values) {
      .FitDTVARMxIDSigma0(
        k = k,
        statenames = statenames,
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
        name = "sigma0",
        name_beta = "beta",
        name_psi = "psi"
      )
    },
    SIMPLIFY = FALSE
  )
  mu <- lapply(
    X = seq_len(n),
    FUN = function(i) {
      .FitDTVARMxIDMuFunc(
        k = k,
        statenames = statenames,
        center = center,
        name = "mu",
        name_beta = "beta",
        name_alpha = "alpha"
      )
    }
  )
  sigma <- lapply(
    X = seq_len(n),
    FUN = function(i) {
      .FitDTVARMxIDSigmaFunc(
        k = k,
        statenames = statenames,
        name = "sigma",
        name_beta = "beta",
        name_psi = "psi"
      )
    }
  )
  covariate <- lapply(
    X = seq_len(n),
    FUN = function(i) {
      .FitDTVARMxIDX()
    }
  )
  matrices <- lapply(
    X = seq_len(n),
    FUN = function(i) {
      list(
        id = ids[i],
        data = data[[i]],
        beta = beta[[i]],
        alpha = alpha[[i]],
        lambda = lambda[[i]],
        nu = nu[[i]],
        psi = psi[[i]],
        theta = theta[[i]],
        mu0 = mu0[[i]],
        sigma0 = sigma0[[i]],
        mu = mu[[i]],
        sigma = sigma[[i]],
        covariate = covariate[[i]]
      )
    }
  )
  submodels <- lapply(
    X = seq_len(n),
    FUN = function(i) {
      mat <- matrices[[i]]
      out <- .FitDTVARMxIDModelID(
        id = mat$id,
        data = mat$data,
        observed = observed,
        beta = mat$beta,
        alpha = mat$alpha,
        lambda = mat$lambda,
        nu = mat$nu,
        psi = mat$psi,
        theta = mat$theta,
        mu0 = mat$mu0,
        sigma0 = mat$sigma0,
        covariate = mat$covariate,
        mu = mat$mu,
        sigma = mat$sigma
      )
      fn <- file.path(
        path,
        paste0(
          prefix,
          "_",
          methods::slot(
            object = out,
            name = "name"
          ),
          ".Rds"
        )
      )
      needs_rewrite <- !file.exists(fn) || {
        obj <- tryCatch(readRDS(fn), error = function(e) NULL)
        is.null(obj) || !inherits(obj, "MxModel")
      }
      if (overwrite || needs_rewrite) {
        saveRDS(
          object = out,
          file = fn
        )
      }
      fn
    }
  )
  c(
    do.call(
      what = "rbind",
      args = submodels
    )
  )
}
