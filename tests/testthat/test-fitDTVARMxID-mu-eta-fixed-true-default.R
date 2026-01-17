## ---- test-fitDTVARMxID-mu-eta-fixed-true-default
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
      mu_eta_fixed = TRUE,
      mu_eta_free = NULL,
      mu_eta_values = NULL,
      mu_eta_lbound = NULL,
      mu_eta_ubound = NULL,
      name_mu_eta = "mu_eta",
      name_alpha = "alpha",
      name_beta = "beta"
    )
    mu_eta_name <- mu_eta[[1]]@name
    mu_eta_values <- mu_eta[[1]]@values
    mu_eta_labels <- mu_eta[[1]]@labels
    mu_eta_free <- mu_eta[[1]]@free
    mu_eta_lbound <- mu_eta[[1]]@lbound
    mu_eta_ubound <- mu_eta[[1]]@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(mu_eta[[1]]) == "ZeroMatrix"
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
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(mu_eta_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          !all(mu_eta_free)
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
      }
    )
  },
  text = "test-fitDTVARMxID-mu-eta-fixed-true-default"
)
