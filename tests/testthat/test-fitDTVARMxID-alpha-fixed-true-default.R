## ---- test-fitDTVARMxID-alpha-fixed-true-default
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
      alpha_fixed = TRUE,
      alpha_free = NULL,
      alpha_values = NULL,
      alpha_lbound = NULL,
      alpha_ubound = NULL,
      name_alpha = "alpha",
      center = FALSE
    )
    alpha_name <- alpha[[1]]@name
    alpha_values <- alpha[[1]]@values
    alpha_labels <- alpha[[1]]@labels
    alpha_free <- alpha[[1]]@free
    alpha_lbound <- alpha[[1]]@lbound
    alpha_ubound <- alpha[[1]]@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(alpha[[1]]) == "ZeroMatrix"
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
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            is.na(alpha_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          !all(alpha_free)
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
      }
    )
  },
  text = "test-fitDTVARMxID-alpha-fixed-true-default"
)
