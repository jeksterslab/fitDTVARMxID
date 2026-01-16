## ---- test-fitDTVARMxID-beta-fixed-true-default
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
      beta_fixed = TRUE,
      beta_free = NULL,
      beta_values = NULL,
      beta_lbound = NULL,
      beta_ubound = NULL,
      name = "beta"
    )
    beta_name <- beta[[1]]@name
    beta_values <- beta[[1]]@values
    beta_labels <- beta[[1]]@labels
    beta_free <- beta[[1]]@free
    beta_lbound <- beta[[1]]@lbound
    beta_ubound <- beta[[1]]@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(beta[[1]]) == "ZeroMatrix"
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
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(beta_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(!beta_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(beta_lbound)
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
            is.na(beta_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-beta-fixed-true-default"
)
