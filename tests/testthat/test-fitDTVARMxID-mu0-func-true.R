## ---- test-fitDTVARMxID-mu0-func-true
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    mu0 <- fitDTVARMxID:::.FitDTVARMxIDMu0(
      k = k,
      statenames = statenames,
      mu0_fixed = TRUE,
      mu0_func = TRUE,
      mu0_free = NULL,
      mu0_values = NULL,
      mu0_lbound = NULL,
      mu0_ubound = NULL,
      name = "mu0",
      name_beta = "beta",
      name_alpha = "alpha",
      center = FALSE
    )
    mu0_name <- mu0$mu0@name
    mu0_vec_name <- mu0$mu0_vec@name
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(mu0$mu0) == "MxAlgebra"
        )
        testthat::expect_true(
          class(mu0$mu0_vec) == "MxAlgebra"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          mu0_name == "mu0"
        )
        testthat::expect_true(
          mu0_vec_name == "mu0_vec"
        )
      }
    )
  },
  text = "test-fitDTVARMxID-mu0-func-true"
)
