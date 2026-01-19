## ---- test-fitDTVARMxID-softplus
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    set.seed(42)
    testthat::test_that(
      paste(text, "d_uc softplus-transforms back to d_vec"),
      {
        testthat::skip_on_cran()
        A <- matrix(rnorm(9), 3, 3)
        X <- crossprod(A) + diag(1e-6, 3)
        out <- LDL(X)
        testthat::expect_equal(
          Softplus(out$d_uc),
          out$d_vec,
          tolerance = 1e-10
        )
      }
    )
    testthat::test_that(
      paste(text, "Softplus and InvSoftplus are numerical inverses"),
      {
        testthat::skip_on_cran()
        x <- rnorm(100)
        y <- Softplus(x)
        x_rec <- InvSoftplus(y)
        testthat::expect_equal(
          x_rec,
          x,
          tolerance = 1e-10
        )
      }
    )
    testthat::test_that(
      paste(text, "Softplus output is strictly positive"),
      {
        testthat::skip_on_cran()
        x <- c(-50, -5, 0, 5, 50)
        y <- Softplus(x)
        testthat::expect_true(
          all(y > 0)
        )
        testthat::expect_true(
          all(is.finite(y))
        )
      }
    )
  },
  text = "test-fitDTVARMxID-softplus"
)
