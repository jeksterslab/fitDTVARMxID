## ---- test-fitDTVARMxID-sigma0-fixed-false-diag-true-default
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
      sigma0_fixed = FALSE,
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
    sigma0_d_vec_name <- sigma0$sigma0_d_vec@name
    sigma0_name <- sigma0$sigma0@name
    sigma0_vech_name <- sigma0$sigma0_vech@name
    sigma0_vec_name <- sigma0$sigma0_vec@name
    sigma0_d_values <- sigma0$sigma0_d@values
    sigma0_d_vec_values <- sigma0$sigma0_d_vec@values
    sigma0_vec_values <- sigma0$sigma0_vec@values
    sigma0_d_labels <- sigma0$sigma0_d@labels
    sigma0_d_vec_labels <- sigma0$sigma0_d_vec@labels
    sigma0_vec_labels <- sigma0$sigma0_vec@labels
    sigma0_d_free <- sigma0$sigma0_d@free
    sigma0_d_vec_free <- sigma0$sigma0_d_vec@free
    sigma0_vec_free <- sigma0$sigma0_vec@free
    sigma0_d_lbound <- sigma0$sigma0_d@lbound
    sigma0_d_vec_lbound <- sigma0$sigma0_d_vec@lbound
    sigma0_vec_lbound <- sigma0$sigma0_vec@lbound
    sigma0_d_ubound <- sigma0$sigma0_d@ubound
    sigma0_d_vec_ubound <- sigma0$sigma0_d_vec@ubound
    sigma0_vec_ubound <- sigma0$sigma0_vec@ubound
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
        testthat::expect_true(
          class(sigma0$sigma0_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(sigma0_d_values),
            rep(x = log(expm1(1)), times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_values),
            rep(x = 0, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_values),
            rep(x = 0, times = k)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(sigma0_d_labels),
            paste0("sigma0_d_", idx, "_1")
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_labels),
            paste0("sigma0_d[", idx, ",1]")
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_labels),
            paste0("sigma0[", idx, ",", idx, "]")
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(sigma0_d_free),
            rep(x = TRUE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_free),
            rep(x = FALSE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_free),
            rep(x = FALSE, times = k)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(sigma0_d_lbound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_lbound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_lbound),
            rep(x = NA_real_, times = k)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(sigma0_d_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-sigma0-fixed-false-diag-true-default"
)
