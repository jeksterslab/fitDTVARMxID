## ---- test-fitDTVARMxID-sigma0-fixed-false-diag-false
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    sigma0 <- fitDTVARMxID:::.FitDTVARMxIDSigma0(
      k = k,
      statenames = statenames,
      sigma0_fixed = FALSE,
      sigma0_diag = FALSE,
      sigma0_d_free = matrix(
        data = TRUE,
        nrow = k,
        ncol = 1
      ),
      sigma0_d_values = matrix(
        data = 1,
        nrow = k,
        ncol = 1
      ),
      sigma0_d_lbound = matrix(
        data = -1,
        nrow = k,
        ncol = 1
      ),
      sigma0_d_ubound = matrix(
        data = 1,
        nrow = k,
        ncol = 1
      ),
      sigma0_l_free = matrix(
        data = TRUE,
        nrow = k,
        ncol = k
      ),
      sigma0_l_values = matrix(
        data = 1,
        nrow = k,
        ncol = k
      ),
      sigma0_l_lbound = matrix(
        data = -1,
        nrow = k,
        ncol = k
      ),
      sigma0_l_ubound = matrix(
        data = +1,
        nrow = k,
        ncol = k
      ),
      name = "sigma0",
      name_beta = "beta",
      name_psi = "psi"
    )
    sigma0_l_name <- sigma0$sigma0_l@name
    sigma0_l_vec_name <- sigma0$sigma0_l_vec@name
    sigma0_d_name <- sigma0$sigma0_d@name
    sigma0_d_vec_name <- sigma0$sigma0_d_vec@name
    sigma0_iden_name <- sigma0$sigma0_iden@name
    sigma0_name <- sigma0$sigma0@name
    sigma0_vech_name <- sigma0$sigma0_vech@name
    sigma0_vec_name <- sigma0$sigma0_vec@name
    sigma0_l_values <- sigma0$sigma0_l@values
    sigma0_l_vec_values <- sigma0$sigma0_l_vec@values
    sigma0_d_values <- sigma0$sigma0_d@values
    sigma0_d_vec_values <- sigma0$sigma0_d_vec@values
    sigma0_iden_values <- sigma0$sigma0_iden@values
    sigma0_vec_values <- sigma0$sigma0_vec@values
    sigma0_l_labels <- sigma0$sigma0_l@labels
    sigma0_l_vec_labels <- sigma0$sigma0_l_vec@labels
    sigma0_d_labels <- sigma0$sigma0_d@labels
    sigma0_d_vec_labels <- sigma0$sigma0_d_vec@labels
    sigma0_iden_labels <- sigma0$sigma0_iden@labels
    sigma0_vec_labels <- sigma0$sigma0_vec@labels
    sigma0_l_free <- sigma0$sigma0_l@free
    sigma0_l_vec_free <- sigma0$sigma0_l_vec@free
    sigma0_d_free <- sigma0$sigma0_d@free
    sigma0_d_vec_free <- sigma0$sigma0_d_vec@free
    sigma0_iden_free <- sigma0$sigma0_iden@free
    sigma0_vec_free <- sigma0$sigma0_vec@free
    sigma0_l_lbound <- sigma0$sigma0_l@lbound
    sigma0_l_vec_lbound <- sigma0$sigma0_l_vec@lbound
    sigma0_d_lbound <- sigma0$sigma0_d@lbound
    sigma0_d_vec_lbound <- sigma0$sigma0_d_vec@lbound
    sigma0_iden_lbound <- sigma0$sigma0_iden@lbound
    sigma0_vec_lbound <- sigma0$sigma0_vec@lbound
    sigma0_l_ubound <- sigma0$sigma0_l@ubound
    sigma0_l_vec_ubound <- sigma0$sigma0_l_vec@ubound
    sigma0_d_ubound <- sigma0$sigma0_d@ubound
    sigma0_d_vec_ubound <- sigma0$sigma0_d_vec@ubound
    sigma0_iden_ubound <- sigma0$sigma0_iden@ubound
    sigma0_vec_ubound <- sigma0$sigma0_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(sigma0$sigma0_l) == "SdiagMatrix"
        )
        testthat::expect_true(
          class(sigma0$sigma0_l_vec) == "FullMatrix"
        )
        testthat::expect_true(
          class(sigma0$sigma0_d) == "FullMatrix"
        )
        testthat::expect_true(
          class(sigma0$sigma0_iden) == "IdenMatrix"
        )
        testthat::expect_true(
          class(sigma0$sigma0) == "MxAlgebra"
        )
        testthat::expect_true(
          class(sigma0$sigma0_vech) == "MxAlgebra"
        )
        testthat::expect_true(
          class(sigma0$sigma0_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        sigma0_l_values_results <- matrix(
          data = 1,
          nrow = k,
          ncol = k
        )
        sigma0_l_values_results[
          upper.tri(
            x = sigma0_l_values_results,
            diag = TRUE
          )
        ] <- 0
        testthat::expect_true(
          identical(
            c(sigma0_l_values), c(sigma0_l_values_results)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_l_vec_values), rep(x = 0, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_values), rep(x = 1, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_values), rep(x = 0, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_iden_values), c(diag(k))
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_values), rep(x = 0, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        sigma0_l_labels_results <- outer(
          X = idx,
          Y = idx,
          FUN = function(x, y) {
            paste0(
              "sigma0_l_",
              x,
              "_",
              y
            )
          }
        )
        sigma0_l_labels_results[
          upper.tri(
            x = sigma0_l_labels_results,
            diag = TRUE
          )
        ] <- NA
        testthat::expect_true(
          identical(
            c(sigma0_l_labels), c(sigma0_l_labels_results)
          )
        )
        sigma0_l_vec_labels_results <- outer(
          X = idx,
          Y = idx,
          FUN = function(x, y) {
            paste0(
              "sigma0_l[",
              x,
              ",",
              y,
              "]"
            )
          }
        )
        sigma0_l_vec_labels_results[
          upper.tri(
            x = sigma0_l_vec_labels_results,
            diag = TRUE
          )
        ] <- NA
        sigma0_l_vec_labels_results <- na.omit(
          c(sigma0_l_vec_labels_results)
        )
        testthat::expect_true(
          identical(
            c(sigma0_l_vec_labels), c(sigma0_l_vec_labels_results)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_labels), paste0("sigma0_d_", idx, "_1")
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_labels), paste0("sigma0_d[", idx, ",1]")
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_iden_labels), rep(x = NA_character_, times = k * k)
          )
        )
        sigma0_vec_labels_results <- outer(
          X = idx,
          Y = idx,
          FUN = function(x, y) {
            paste0(
              "sigma0[",
              x,
              ",",
              y,
              "]"
            )
          }
        )
        sigma0_vec_labels_results[
          upper.tri(
            x = sigma0_vec_labels_results
          )
        ] <- NA
        sigma0_vec_labels_results <- na.omit(
          c(sigma0_vec_labels_results)
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_labels), c(sigma0_vec_labels_results)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        sigma0_l_free_results <- matrix(
          data = TRUE,
          nrow = k,
          ncol = k
        )
        sigma0_l_free_results[
          upper.tri(
            x = sigma0_l_free_results,
            diag = TRUE
          )
        ] <- FALSE
        testthat::expect_true(
          identical(
            c(sigma0_l_free), c(sigma0_l_free_results)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_l_vec_free), rep(x = FALSE, times = (k * (k - 1)) / 2)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_free), rep(x = TRUE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_free), rep(x = FALSE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_iden_free), c(matrix(data = FALSE, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_free), rep(x = FALSE, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        sigma0_l_lbound_results <- matrix(
          data = -1,
          nrow = k,
          ncol = k
        )
        sigma0_l_lbound_results[
          upper.tri(
            x = sigma0_l_lbound_results,
            diag = TRUE
          )
        ] <- NA
        testthat::expect_true(
          identical(
            c(sigma0_l_lbound), c(sigma0_l_lbound_results)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_l_vec_lbound), rep(x = NA_real_, times = (k * (k - 1)) / 2)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_lbound), rep(x = -1, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_lbound), rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_iden_lbound),
            c(matrix(data = NA_real_, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_lbound),
            rep(x = NA_real_, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::skip_on_cran()
        sigma0_l_ubound_results <- matrix(
          data = 1,
          nrow = k,
          ncol = k
        )
        sigma0_l_ubound_results[
          upper.tri(
            x = sigma0_l_ubound_results,
            diag = TRUE
          )
        ] <- NA
        testthat::expect_true(
          identical(
            c(sigma0_l_ubound),
            c(sigma0_l_ubound_results)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_l_vec_ubound),
            rep(x = NA_real_, times = (k * (k - 1)) / 2)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_ubound),
            rep(x = 1, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_d_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_iden_ubound),
            c(matrix(data = NA_real_, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(sigma0_vec_ubound),
            rep(x = NA_real_, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-sigma0-fixed-false-diag-false"
)
