## ---- test-fitDTVARMxID-mu-eta-fixed-false-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    mu_eta <- fitDTVARMxID:::.FitDTVARMxIDMuEta(
      k = k,
      statenames = statenames,
      center = FALSE,
      mu_eta_fixed = FALSE,
      mu_eta_free = NULL,
      mu_eta_values = NULL,
      mu_eta_lbound = NULL,
      mu_eta_ubound = NULL,
      name_mu_eta = "mu_eta",
      name_alpha = "alpha",
      name_beta = "beta"
    )
    mu_eta_name <- mu_eta$mu_eta@name
    mu_eta_values <- mu_eta$mu_eta@values
    mu_eta_labels <- mu_eta$mu_eta@labels
    mu_eta_free <- mu_eta$mu_eta@free
    mu_eta_lbound <- mu_eta$mu_eta@lbound
    mu_eta_ubound <- mu_eta$mu_eta@ubound
    mu_eta_vec_name <- mu_eta$mu_eta_vec@name
    mu_eta_vec_values <- mu_eta$mu_eta_vec@values
    mu_eta_vec_labels <- mu_eta$mu_eta_vec@labels
    mu_eta_vec_free <- mu_eta$mu_eta_vec@free
    mu_eta_vec_lbound <- mu_eta$mu_eta_vec@lbound
    mu_eta_vec_ubound <- mu_eta$mu_eta_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(mu_eta$mu_eta) == "FullMatrix"
        )
        testthat::expect_true(
          class(mu_eta$mu_eta_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          mu_eta_name == "mu_eta"
        )
        testthat::expect_true(
          mu_eta_vec_name == "mu_eta_vec"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            c(mu_eta_values) == 0
          )
        )
        testthat::expect_true(
          all(
            c(mu_eta_vec_values) == 0
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
            c(mu_eta_labels) == paste0("mu_eta_", idx, "_1")
          )
        )
        testthat::expect_true(
          all(
            c(mu_eta_vec_labels) == paste0("mu_eta[", idx, ",1]")
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(mu_eta_free)
        )
        testthat::expect_true(
          all(!mu_eta_vec_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(mu_eta_lbound)
          )
        )
        testthat::expect_true(
          all(
            is.na(mu_eta_vec_lbound)
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
            is.na(mu_eta_ubound)
          )
        )
        testthat::expect_true(
          all(
            is.na(mu_eta_vec_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-mu-eta-fixed-false-default"
)
