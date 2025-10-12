## ---- test-fitDTVARMxID-psi-diag-true-default
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
      psi_diag = TRUE,
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
    psi_d_name <- psi$psi_d@name
    psi_d_vec_name <- psi$psi_d_vec@name
    psi_name <- psi$psi@name
    psi_vech_name <- psi$psi_vech@name
    psi_vec_name <- psi$psi_vec@name
    psi_d_values <- psi$psi_d@values
    psi_d_vec_values <- psi$psi_d_vec@values
    psi_vec_values <- psi$psi_vec@values
    psi_d_labels <- psi$psi_d@labels
    psi_d_vec_labels <- psi$psi_d_vec@labels
    psi_vec_labels <- psi$psi_vec@labels
    psi_d_free <- psi$psi_d@free
    psi_d_vec_free <- psi$psi_d_vec@free
    psi_vec_free <- psi$psi_vec@free
    psi_d_lbound <- psi$psi_d@lbound
    psi_d_vec_lbound <- psi$psi_d_vec@lbound
    psi_vec_lbound <- psi$psi_vec@lbound
    psi_d_ubound <- psi$psi_d@ubound
    psi_d_vec_ubound <- psi$psi_d_vec@ubound
    psi_vec_ubound <- psi$psi_vec@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          class(psi$psi_d) == "FullMatrix"
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
            c(psi_vec_values),
            rep(x = 0, times = k)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::skip_on_cran()
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
            c(psi_vec_labels),
            paste0(
              "psi[",
              idx,
              ",",
              idx,
              "]"
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
            c(psi_vec_free),
            rep(x = FALSE, times = k)
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
            c(psi_vec_lbound),
            rep(x = NA_real_, times = k)
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
            c(psi_vec_ubound),
            rep(x = NA_real_, times = k)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMxID-psi-diag-true-default"
)
