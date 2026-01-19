## ---- test-fitDTVARMxID-ldl
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    set.seed(42)
    testthat::test_that(
      paste(text, "LDL reconstructs the input matrix"),
      {
        testthat::skip_on_cran()
        A <- matrix(
          data = rnorm(16),
          nrow = 4,
          ncol = 4
        )
        X <- crossprod(A) + diag(1e-6, 4)
        out <- LDL(X)
        testthat::expect_equal(
          out$l_mat_unit %*% out$d_mat %*% t(out$l_mat_unit),
          X,
          tolerance = 1e-8
        )
        testthat::expect_equal(
          out$diff,
          X - out$y,
          tolerance = 1e-12
        )
        testthat::expect_true(
          max(abs(out$diff)) < 1e-8
        )
      }
    )
    testthat::test_that(
      paste(text, "LDL diagonal entries are positive"),
      {
        testthat::skip_on_cran()
        A <- matrix(rnorm(25), 5, 5)
        X <- crossprod(A) + diag(1e-6, 5)
        out <- LDL(X)
        testthat::expect_true(
          all(out$d_vec > 0)
        )
      }
    )
  },
  text = "test-fitDTVARMxID-ldl"
)
