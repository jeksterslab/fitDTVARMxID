## ---- test-fitDTVARMxID-sigma0-func-true
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    sigma0 <- fitDTVARMxID:::.FitDTVARMxIDSigma0(
      k = k,
      statenames = statenames,
      sigma0_fixed = TRUE,
      sigma0_func = TRUE,
      sigma0_diag = FALSE,
      sigma0_d_free = NULL,
      sigma0_d_values = NULL,
      sigma0_d_lbound = NULL,
      sigma0_d_ubound = NULL,
      sigma0_l_free = NULL,
      sigma0_l_values = NULL,
      sigma0_l_lbound = NULL,
      sigma0_l_ubound = NULL,
      name = "sigma0",
      name_beta = "beta",
      name_psi = "psi"
    )
    sigma0_name <- sigma0$sigma0@name
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(sigma0$sigma0) == "MxAlgebra"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          sigma0_name == "sigma0"
        )
      }
    )
  },
  text = "test-fitDTVARMxID-sigma0-func-true"
)
