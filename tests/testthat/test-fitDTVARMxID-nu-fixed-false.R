## ---- test-fitDTVARMxID-nu-fixed-false
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    observed <- paste0("y", idx)
    nu <- fitDTVARMxID:::.FitDTVARMxIDNu(
      k = k,
      observed = observed,
      nu_fixed = FALSE,
      nu_free = rep(x = TRUE, times = k),
      nu_values = rep(x = 1, times = k),
      nu_lbound = rep(x = -1, times = k),
      nu_ubound = rep(x = +1, times = k),
      name = "nu"
    )
    nu_name <- nu$nu@name
    nu_values <- nu$nu@values
    nu_labels <- nu$nu@labels
    nu_free <- nu$nu@free
    nu_lbound <- nu$nu@lbound
    nu_ubound <- nu$nu@ubound
    nu_vec_name <- nu$nu_vec@name
    nu_vec_values <- nu$nu_vec@values
    nu_vec_labels <- nu$nu_vec@labels
    nu_vec_free <- nu$nu_vec@free
    nu_vec_lbound <- nu$nu_vec@lbound
    nu_vec_ubound <- nu$nu_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(nu$nu) == "FullMatrix"
        )
        testthat::expect_true(
          class(nu$nu_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          nu_name == "nu"
        )
        testthat::expect_true(
          nu_vec_name == "nu_vec"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            c(nu_values) == 1
          )
        )
        testthat::expect_true(
          all(
            c(nu_vec_values) == 0
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
            c(nu_labels) == paste0("nu_", idx, "_1")
          )
        )
        testthat::expect_true(
          all(
            c(nu_vec_labels) == paste0("nu[", idx, ",1]")
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(nu_free)
        )
        testthat::expect_true(
          all(!nu_vec_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            c(nu_lbound) == c(
              matrix(
                data = -1,
                nrow = k,
                ncol = 1
              )
            )
          )
        )
        testthat::expect_true(
          all(
            is.na(nu_vec_lbound)
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
            c(nu_ubound) == c(
              matrix(
                data = 1,
                nrow = k,
                ncol = 1
              )
            )
          )
        )
        testthat::expect_true(
          all(
            is.na(nu_vec_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-nu-fixed-false"
)
