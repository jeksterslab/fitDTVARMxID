## ---- test-fitDTVARMxID-beta-fixed-false-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    beta <- fitDTVARMxID:::.FitDTVARMxIDBeta(
      k = k,
      statenames = statenames,
      beta_fixed = FALSE,
      beta_free = NULL,
      beta_values = NULL,
      beta_lbound = NULL,
      beta_ubound = NULL,
      name = "beta"
    )
    beta_name <- beta$beta@name
    beta_values <- beta$beta@values
    beta_labels <- beta$beta@labels
    beta_free <- beta$beta@free
    beta_lbound <- beta$beta@lbound
    beta_ubound <- beta$beta@ubound
    beta_vec_name <- beta$beta_vec@name
    beta_vec_values <- beta$beta_vec@values
    beta_vec_labels <- beta$beta_vec@labels
    beta_vec_free <- beta$beta_vec@free
    beta_vec_lbound <- beta$beta_vec@lbound
    beta_vec_ubound <- beta$beta_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(beta$beta) == "FullMatrix"
        )
        testthat::expect_true(
          class(beta$beta_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          beta_name == "beta"
        )
        testthat::expect_true(
          beta_vec_name == "beta_vec"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            beta_values == diag(
              x = 0,
              nrow = k,
              ncol = k
            )
          )
        )
        testthat::expect_true(
          all(
            beta_vec_values == matrix(
              data = 0,
              nrow = k * k,
              ncol = 1
            )
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
            c(beta_labels) == c(
              outer(
                X = idx,
                Y = idx,
                FUN = function(x, y) {
                  paste0(
                    "beta",
                    "_",
                    x,
                    "_",
                    y
                  )
                }
              )
            )
          )
        )
        testthat::expect_true(
          all(
            c(beta_vec_labels) == c(
              outer(
                X = idx,
                Y = idx,
                FUN = function(x, y) {
                  paste0(
                    "beta",
                    "[",
                    x,
                    ",",
                    y,
                    "]"
                  )
                }
              )
            )
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(beta_free)
        )
        testthat::expect_true(
          all(!beta_vec_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(beta_lbound), rep(x = -2.5, times = k * k)
          )
        )
        testthat::expect_true(
          all(
            is.na(beta_vec_lbound)
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
            c(beta_ubound), rep(x = +2.5, times = k * k)
          )
        )
        testthat::expect_true(
          all(
            is.na(beta_vec_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-beta-fixed-false-default"
)
