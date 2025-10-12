## ---- test-fitDTVARMxID-psi-diag-false-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    psi <- fitDTVARMxID:::.FitDTVARMxIDPsi(
      k = k,
      statenames = statenames,
      psi_diag = FALSE,
      psi_d_free = NULL,
      psi_d_values = NULL,
      psi_d_lbound = NULL,
      psi_d_ubound = NULL,
      psi_l_free = NULL,
      psi_l_values = NULL,
      psi_l_lbound = NULL,
      psi_l_ubound = NULL,
      name = "psi"
    )
    psi_l_name <- psi$psi_l@name
    psi_l_vec_name <- psi$psi_l_vec@name
    psi_d_name <- psi$psi_d@name
    psi_d_vec_name <- psi$psi_d_vec@name
    psi_iden_name <- psi$psi_iden@name
    psi_name <- psi$psi@name
    psi_vech_name <- psi$psi_vech@name
    psi_vec_name <- psi$psi_vec@name
    psi_l_values <- psi$psi_l@values
    psi_l_vec_values <- psi$psi_l_vec@values
    psi_d_values <- psi$psi_d@values
    psi_d_vec_values <- psi$psi_d_vec@values
    psi_iden_values <- psi$psi_iden@values
    psi_vec_values <- psi$psi_vec@values
    psi_l_labels <- psi$psi_l@labels
    psi_l_vec_labels <- psi$psi_l_vec@labels
    psi_d_labels <- psi$psi_d@labels
    psi_d_vec_labels <- psi$psi_d_vec@labels
    psi_iden_labels <- psi$psi_iden@labels
    psi_vec_labels <- psi$psi_vec@labels
    psi_l_free <- psi$psi_l@free
    psi_l_vec_free <- psi$psi_l_vec@free
    psi_d_free <- psi$psi_d@free
    psi_d_vec_free <- psi$psi_d_vec@free
    psi_iden_free <- psi$psi_iden@free
    psi_vec_free <- psi$psi_vec@free
    psi_l_lbound <- psi$psi_l@lbound
    psi_l_vec_lbound <- psi$psi_l_vec@lbound
    psi_d_lbound <- psi$psi_d@lbound
    psi_d_vec_lbound <- psi$psi_d_vec@lbound
    psi_iden_lbound <- psi$psi_iden@lbound
    psi_vec_lbound <- psi$psi_vec@lbound
    psi_l_ubound <- psi$psi_l@ubound
    psi_l_vec_ubound <- psi$psi_l_vec@ubound
    psi_d_ubound <- psi$psi_d@ubound
    psi_d_vec_ubound <- psi$psi_d_vec@ubound
    psi_iden_ubound <- psi$psi_iden@ubound
    psi_vec_ubound <- psi$psi_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(psi$psi_l) == "SdiagMatrix"
        )
        testthat::expect_true(
          class(psi$psi_l_vec) == "FullMatrix"
        )
        testthat::expect_true(
          class(psi$psi_d) == "FullMatrix"
        )
        testthat::expect_true(
          class(psi$psi_iden) == "IdenMatrix"
        )
        testthat::expect_true(
          class(psi$psi) == "MxAlgebra"
        )
        testthat::expect_true(
          class(psi$psi_vech) == "MxAlgebra"
        )
        testthat::expect_true(
          class(psi$psi_vec) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(psi_l_values),
            rep(x = 0, times = k * k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_l_vec_values),
            rep(x = 0, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_values),
            rep(x = log(expm1(1)), times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_vec_values),
            rep(x = 0, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_iden_values),
            c(diag(k))
          )
        )
        testthat::expect_true(
          identical(
            c(psi_vec_values),
            rep(x = 0, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
        psi_l_labels_results <- outer(
          X = idx,
          Y = idx,
          FUN = function(x, y) {
            paste0(
              "psi_l_",
              x,
              "_",
              y
            )
          }
        )
        psi_l_labels_results[
          upper.tri(
            x = psi_l_labels_results,
            diag = TRUE
          )
        ] <- NA
        testthat::expect_true(
          identical(
            c(psi_l_labels),
            c(psi_l_labels_results)
          )
        )
        psi_l_vec_labels_results <- outer(
          X = idx,
          Y = idx,
          FUN = function(x, y) {
            paste0(
              "psi_l[",
              x,
              ",",
              y,
              "]"
            )
          }
        )
        psi_l_vec_labels_results[
          upper.tri(
            x = psi_l_vec_labels_results,
            diag = TRUE
          )
        ] <- NA
        psi_l_vec_labels_results <- na.omit(
          c(psi_l_vec_labels_results)
        )
        testthat::expect_true(
          identical(
            c(psi_l_vec_labels),
            c(psi_l_vec_labels_results)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_labels),
            paste0("psi_d_", idx, "_1")
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_vec_labels),
            paste0("psi_d[", idx, ",1]")
          )
        )
        testthat::expect_true(
          identical(
            c(psi_iden_labels),
            rep(x = NA_character_, times = k * k)
          )
        )
        psi_vec_labels_results <- outer(
          X = idx,
          Y = idx,
          FUN = function(x, y) {
            paste0(
              "psi[",
              x,
              ",",
              y,
              "]"
            )
          }
        )
        psi_vec_labels_results[
          upper.tri(
            x = psi_vec_labels_results
          )
        ] <- NA
        psi_vec_labels_results <- na.omit(
          c(psi_vec_labels_results)
        )
        testthat::expect_true(
          identical(
            c(psi_vec_labels),
            c(psi_vec_labels_results)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::skip_on_cran()
        psi_l_free_results <- matrix(
          data = TRUE,
          nrow = k,
          ncol = k
        )
        psi_l_free_results[
          upper.tri(
            x = psi_l_free_results,
            diag = TRUE
          )
        ] <- FALSE
        testthat::expect_true(
          identical(
            c(psi_l_free),
            c(psi_l_free_results)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_l_vec_free),
            rep(x = FALSE, times = (k * (k - 1)) / 2)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_free),
            rep(x = TRUE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_vec_free),
            rep(x = FALSE, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_iden_free),
            c(matrix(data = FALSE, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(psi_vec_free),
            rep(x = FALSE, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          identical(
            c(psi_l_lbound),
            c(matrix(data = NA_real_, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(psi_l_vec_lbound),
            rep(x = NA_real_, times = (k * (k - 1)) / 2)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_lbound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_vec_lbound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_iden_lbound),
            c(matrix(data = NA_real_, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(psi_vec_lbound),
            rep(x = NA_real_, times = (k * (k + 1)) / 2)
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
            c(psi_l_ubound),
            c(matrix(data = NA_real_, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(psi_l_vec_ubound),
            rep(x = NA_real_, times = (k * (k - 1)) / 2)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_d_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
        testthat::expect_true(
          identical(
            c(psi_iden_ubound),
            c(matrix(data = NA_real_, nrow = k, ncol = k))
          )
        )
        testthat::expect_true(
          identical(
            c(psi_vec_ubound),
            rep(x = NA_real_, times = (k * (k + 1)) / 2)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-psi-diag-false-default"
)
