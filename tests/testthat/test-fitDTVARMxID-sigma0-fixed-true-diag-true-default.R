## ---- test-fitDTVARMxID-sigma0-fixed-true-diag-true-default
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
      sigma0_func = FALSE,
      sigma0_diag = TRUE,
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
    sigma0_d_name <- sigma0$sigma0_d@name
    sigma0_name <- sigma0$sigma0@name
    sigma0_vech_name <- sigma0$sigma0_vech@name
    sigma0_d_values <- sigma0$sigma0_d@values
    sigma0_d_labels <- sigma0$sigma0_d@labels
    sigma0_d_free <- sigma0$sigma0_d@free
    sigma0_d_lbound <- sigma0$sigma0_d@lbound
    sigma0_d_ubound <- sigma0$sigma0_d@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(sigma0$sigma0_d) == "FullMatrix"
        )
        testthat::expect_true(
          class(sigma0$sigma0) == "MxAlgebra"
        )
        testthat::expect_true(
          class(sigma0$sigma0_vech) == "MxAlgebra"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          sigma0_d_name == "sigma0_d"
        )
        testthat::expect_true(
          sigma0_name == "sigma0"
        )
        testthat::expect_true(
          sigma0_vech_name == "sigma0_vech"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all.equal(
            c(sigma0_d_values),
            rep(x = log(expm1(1)), times = k)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(sigma0_d_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          !all(sigma0_d_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(sigma0_d_lbound)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(sigma0_d_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-sigma0-fixed-true-diag-true-default"
)
