## ---- test-fitDTVARMxID-fitdtvarmxid
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    k <- 2
    n <- 5
    time <- 1000
    alpha <- rep(x = 0, times = k)
    beta <- 0.90 * diag(k)
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
      theta_fixed = TRUE # no measurement component
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
    testthat::test_that(
      paste(text, "alpha"),
      {
        testthat::skip_on_cran()
        for (i in seq_len(n)) {
          testthat::expect_true(
            all(
              abs(
                c(
                  alpha
                ) - c(
                  mxEvalByName(
                    name = "alpha",
                    model = fit$output[[i]]
                  )
                )
              ) <= tol
            )
          )
        }
      }
    )
    testthat::test_that(
      paste(text, "beta"),
      {
        testthat::skip_on_cran()
        for (i in seq_len(n)) {
          testthat::expect_true(
            all(
              abs(
                c(
                  beta
                ) - c(
                  mxEvalByName(
                    name = "beta",
                    model = fit$output[[i]]
                  )
                )
              ) <= tol
            )
          )
        }
      }
    )
    testthat::test_that(
      paste(text, "psi"),
      {
        testthat::skip_on_cran()
        for (i in seq_len(n)) {
          testthat::expect_true(
            all(
              abs(
                c(
                  psi
                ) - c(
                  mxEvalByName(
                    name = "psi",
                    model = fit$output[[i]]
                  )
                )
              ) <= tol
            )
          )
        }
      }
    )
  },
  text = "test-fitDTVARMxID-fitdtvarmxid",
  tol = 0.10
)
