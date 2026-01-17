## ---- test-fitDTVARMxID-alpha-fixed-false-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    alpha <- fitDTVARMxID:::.FitDTVARMxIDAlpha(
      k = k,
      statenames = statenames,
      center = FALSE,
      alpha_fixed = FALSE,
      alpha_free = NULL,
      alpha_values = NULL,
      alpha_lbound = NULL,
      alpha_ubound = NULL,
      name = "alpha"
    )
    alpha_name <- alpha$alpha@name
    alpha_values <- alpha$alpha@values
    alpha_labels <- alpha$alpha@labels
    alpha_free <- alpha$alpha@free
    alpha_lbound <- alpha$alpha@lbound
    alpha_ubound <- alpha$alpha@ubound
    alpha_vec_name <- alpha$alpha_vec@name
    alpha_vec_values <- alpha$alpha_vec@values
    alpha_vec_labels <- alpha$alpha_vec@labels
    alpha_vec_free <- alpha$alpha_vec@free
    alpha_vec_lbound <- alpha$alpha_vec@lbound
    alpha_vec_ubound <- alpha$alpha_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(alpha$alpha) == "FullMatrix"
        )
        testthat::expect_true(
          class(alpha$alpha_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          alpha_name == "alpha"
        )
        testthat::expect_true(
          alpha_vec_name == "alpha_vec"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            c(alpha_values) == 0
          )
        )
        testthat::expect_true(
          all(
            c(alpha_vec_values) == 0
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
            c(alpha_labels) == paste0("alpha_", idx, "_1")
          )
        )
        testthat::expect_true(
          all(
            c(alpha_vec_labels) == paste0("alpha[", idx, ",1]")
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(alpha_free)
        )
        testthat::expect_true(
          all(!alpha_vec_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(alpha_lbound)
          )
        )
        testthat::expect_true(
          all(
            is.na(alpha_vec_lbound)
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
            is.na(alpha_ubound)
          )
        )
        testthat::expect_true(
          all(
            is.na(alpha_vec_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-alpha-fixed-false-default"
)
