## ---- test-fitDTVARMxID-fitdtvarmxid-center-false-alpha-fixed
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }
    k <- 2
    n <- 10
    time <- 1000
    alpha <- stats::runif(n = k)
    beta <- 0.90 * diag(k)
    mu_eta <- c(solve(diag(k) - beta) %*% alpha)
    psi <- diag(k)
    psi_l <- t(chol(psi))
    mu0 <- c(solve(diag(k) - beta) %*% alpha)
    sigma0 <- matrix(
      data = c(
        solve(diag(k * k) - beta %x% beta) %*% c(psi)
      ),
      nrow = k,
      ncol = k
    )
    sigma0_l <- t(chol(sigma0))
    sim <- simStateSpace::SimSSMVARIVary(
      n = n,
      time = time,
      mu0 = list(mu0),
      sigma0_l = list(sigma0_l),
      alpha = list(alpha),
      beta = list(beta),
      psi_l = list(psi_l)
    )
    data <- as.data.frame(sim)
    fit <- FitDTVARMxID(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      center = FALSE,
      alpha_fixed = TRUE,
      alpha_values = list(alpha),
      theta_fixed = TRUE, # no measurement component
      robust = TRUE,
      seed = 42
    )
    print(fit)
    summary(fit)
    coef(fit)
    vcov(fit)
    testthat::test_that(
      paste(text, "converged"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(converged(fit, prop = FALSE))
        )
      }
    )
    library(OpenMx)
    alpha_hat <- colMeans(
      do.call(
        what = "rbind",
        args = lapply(
          X = seq_len(n),
          FUN = function(i) {
            c(
              mxEvalByName(
                name = "alpha",
                model = fit$output[[i]]
              )
            )
          }
        )
      )
    )
    beta_hat <- colMeans(
      do.call(
        what = "rbind",
        args = lapply(
          X = seq_len(n),
          FUN = function(i) {
            c(
              mxEvalByName(
                name = "beta",
                model = fit$output[[i]]
              )
            )
          }
        )
      )
    )
    psi_hat <- colMeans(
      do.call(
        what = "rbind",
        args = lapply(
          X = seq_len(n),
          FUN = function(i) {
            c(
              mxEvalByName(
                name = "psi",
                model = fit$output[[i]]
              )
            )
          }
        )
      )
    )
    testthat::test_that(
      paste(text, "alpha"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            abs(
              c(
                alpha
              ) - c(
                alpha_hat
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "beta"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            abs(
              c(
                beta
              ) - c(
                beta_hat
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "psi"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            abs(
              c(
                psi
              ) - c(
                psi_hat
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "error"),
      {
        testthat::skip_on_cran()
        testthat::expect_error(
          FitDTVARMxID(
            data = data,
            observed = paste0("y", seq_len(k)),
            id = "id",
            center = TRUE,
            alpha_fixed = FALSE,
            nu_fixed = FALSE,
            theta_fixed = TRUE # no measurement component
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-fitdtvarmxid-center-false-alpha-fixed",
  tol = 0.20
)
