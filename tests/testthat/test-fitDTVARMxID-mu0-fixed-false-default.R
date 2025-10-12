## ---- test-fitDTVARMxID-mu0-fixed-false-default
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
      mu0_fixed = FALSE,
      mu0_func = FALSE,
      mu0_free = NULL,
      mu0_values = NULL,
      mu0_lbound = NULL,
      mu0_ubound = NULL,
      name = "mu0",
      name_beta = "beta",
      name_alpha = "alpha"
    )
    mu0_name <- mu0$mu0@name
    mu0_values <- mu0$mu0@values
    mu0_labels <- mu0$mu0@labels
    mu0_free <- mu0$mu0@free
    mu0_lbound <- mu0$mu0@lbound
    mu0_ubound <- mu0$mu0@ubound
    mu0_vec_name <- mu0$mu0_vec@name
    mu0_vec_values <- mu0$mu0_vec@values
    mu0_vec_labels <- mu0$mu0_vec@labels
    mu0_vec_free <- mu0$mu0_vec@free
    mu0_vec_lbound <- mu0$mu0_vec@lbound
    mu0_vec_ubound <- mu0$mu0_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(mu0$mu0) == "FullMatrix"
        )
        testthat::expect_true(
          class(mu0$mu0_vec) == "FullMatrix"
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
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            c(mu0_values) == 0
          )
        )
        testthat::expect_true(
          all(
            c(mu0_vec_values) == 0
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
            c(mu0_labels) == paste0("mu0_", idx, "_1")
          )
        )
        testthat::expect_true(
          all(
            c(mu0_vec_labels) == paste0("mu0[", idx, ",1]")
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(mu0_free)
        )
        testthat::expect_true(
          all(!mu0_vec_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(mu0_lbound)
          )
        )
        testthat::expect_true(
          all(
            is.na(mu0_vec_lbound)
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
            is.na(mu0_ubound)
          )
        )
        testthat::expect_true(
          all(
            is.na(mu0_vec_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-mu0-fixed-false-default"
)
