## ---- test-fitDTVARMxID-beta-fixed-true
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
      beta_free = diag(
        x = TRUE,
        nrow = k,
        ncol = k
      ),
      beta_values = matrix(
        data = 1,
        nrow = k,
        ncol = k
      ),
      beta_lbound = matrix(
        data = -1,
        nrow = k,
        ncol = k
      ),
      beta_ubound = matrix(
        data = +1,
        nrow = k,
        ncol = k
      ),
      name = "beta"
    )
    beta_name <- beta@name
    beta_values <- beta@values
    beta_labels <- beta@labels
    beta_free <- beta@free
    beta_lbound <- beta@lbound
    beta_ubound <- beta@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(beta) == "FullMatrix"
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
            beta_values == matrix(
              data = 1,
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
  text = "test-fitDTVARMxID-beta-fixed-true"
)
