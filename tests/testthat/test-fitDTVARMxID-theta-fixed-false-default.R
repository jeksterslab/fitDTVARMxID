## ---- test-fitDTVARMxID-theta-fixed-false-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    observed <- paste0("y", idx)
    theta <- fitDTVARMxID:::.FitDTVARMxIDTheta(
      k = k,
      observed = observed,
      theta_diag = TRUE,
      theta_fixed = FALSE,
      theta_d_free = NULL,
      theta_d_values = NULL,
      theta_d_lbound = NULL,
      theta_d_ubound = NULL,
      theta_d_equal = FALSE,
      name = "theta"
    )
    theta_d_name <- theta$theta_d@name
    theta_d_vec_name <- theta$theta_d_vec@name
    theta_name <- theta$theta@name
    theta_vech_name <- theta$theta_vech@name
    theta_vec_name <- theta$theta_vec@name
    theta_d_values <- theta$theta_d@values
    theta_d_vec_values <- theta$theta_d_vec@values
    theta_vec_values <- theta$theta_vec@values
    theta_d_labels <- theta$theta_d@labels
    theta_d_vec_labels <- theta$theta_d_vec@labels
    theta_vec_labels <- theta$theta_vec@labels
    theta_d_free <- theta$theta_d@free
    theta_d_vec_free <- theta$theta_d_vec@free
    theta_vec_free <- theta$theta_vec@free
    theta_d_lbound <- theta$theta_d@lbound
    theta_d_vec_lbound <- theta$theta_d_vec@lbound
    theta_vec_lbound <- theta$theta_vec@lbound
    theta_d_ubound <- theta$theta_d@ubound
    theta_d_vec_ubound <- theta$theta_d_vec@ubound
    theta_vec_ubound <- theta$theta_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(theta$theta_d) == "FullMatrix"
        )
        testthat::expect_true(
          class(theta$theta) == "MxAlgebra"
        )
        testthat::expect_true(
          class(theta$theta_vech) == "MxAlgebra"
        )
        testthat::expect_true(
          class(theta$theta_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(theta_d_values),
            rep(x = log(expm1(1)), times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_d_vec_values),
            rep(x = 0, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_vec_values),
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
            c(theta_d_labels),
            paste0("theta_d_", idx, "_1")
          )
        )
        testthat::expect_true(
          identical(
            c(theta_d_vec_labels),
            paste0("theta_d[", idx, ",1]")
          )
        )
        testthat::expect_true(
          identical(
            c(theta_vec_labels),
            paste0("theta[", idx, ",", idx, "]")
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
            c(theta_d_free),
            rep(x = TRUE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_d_vec_free),
            rep(x = FALSE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_vec_free),
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
            c(theta_d_lbound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_d_vec_lbound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_vec_lbound),
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
            c(theta_d_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_d_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(theta_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-theta-fixed-false-default"
)
